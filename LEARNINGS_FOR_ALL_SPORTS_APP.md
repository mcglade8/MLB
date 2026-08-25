# MLB Model Learnings → All-Sports-App

Transfer brief from [`mcglade8/MLB`](https://github.com/mcglade8/MLB) (historical R/Python/JS DFS stack) for sibling agents working on [`mcglade8/All-Sports-App`](https://github.com/mcglade8/All-Sports-App).

**Goal:** port the highest-value player-level modeling ideas into All-Sports-App MLB predictions. Do **not** lift broken scrapers, frozen coefficients, or notebook mess wholesale.

## Architecture of the old stack

Three generations coexisted; production never fully replaced research:

```text
Research (baseballr.rmd)
  MLB PBP → platoon rates → park×hand → EMA/rhythm → player + team LMs
        │
Production DFS (MLBAbridged.Rmd / MLBDFS2023)
  DK props → μ/σ FPTS → TopPct MC → Own → PlayVFadeEV → sim + ILP
        │
Builder UI (Baseball2024.js + baseball_data.json)
  Consumes fpts, stdev, jvalue, own, salary, positions, team, opp, order
```

| Layer | Primary files | What it was good at |
|-------|---------------|---------------------|
| Player/team skill models | `baseballr.rmd` | Platoon, park×hand, form, hierarchical team stage |
| Live projections + contest EV | `MLBAbridged.Rmd`, `MLBDFS2023` | Prop→FPTS, ownership leverage, stack ILP |
| Statcast features | `baseball_python.ipynb` | Barrel zone, xStats, platoon-reduced columns |
| Builder contract | `Baseball2024.js`, `baseball_data.json` | Thin projection schema the UI already expects |

---

## Suggested All-Sports-App MLB shape

```text
Feature store
  platoon rates, park×hand×role, form (EMA/rhythm), Statcast quality,
  pitcher BF/volume, game env (implied total / RL / win%)
        ↓
Projection service
  { mean, stdev, components } per player-site scoring rules
  (blend market props with internal model when both exist)
        ↓
Contest metrics
  TopPct (MC or analytic) → ownership → leverageEV
  separate GPP upside score (optional)
        ↓
Builder / API contract
  mean, stdev, ownership, leverage_or_jvalue, salary, positions,
  team, opp, batting_order, opp_pitcher
```

Keep **calibrated mean FPTS** separate from **GPP ranking / jvalue**. The old UI often optimized on jvalue and invented SD from `jvalue*2` — do not repeat that.

---

## P0 — Port first

### 1. Platoon (handedness) matchup features

**Source:** `baseballr.rmd` (~287–378)

Built L/R rates per batter and pitcher (`mbdbL`/`mbdbR`, `mpdbL`/`mpdbR`, `mbdpL`/`mbdpR`, `mpdpL`/`mpdpR`), then mapped each AB to:

- `exp_batter_earned_fps_by_hand`
- `exp_pitcher_allowed_fps_by_hand`
- (and the pitcher-side mirrors)

Batter game model:

```r
lm(game_fp ~ av_pts_ab + exp_batter_earned_fps_by_hand
           + exp_pitcher_allowed_fps_by_hand + park_adjust
           + fp_ema + rhythm_score, ...)
# Adjusted R-squared: 0.1073
```

**Port:** every batter–pitcher projection conditioned on opponent hand. Store platoon-split mean FPTS (or rates) in the feature store.

**Improve while porting:** switch-hitters used raw `max(L,R)` — replace with sample-size shrinkage / Bayesian prior (min PA threshold). Do not use season means with tiny samples as-is.

### 2. Park × handedness × role factors

**Source:** `baseballr.rmd` (~397–448)

Separate home-park points-per-AB tables for R/L batters and R/L pitchers (`park_av_batter_ppab_R/L`, `park_adj_*`), not a single park scalar. Park entered batter, pitcher, and team models.

**Port:** venue feature matrix keyed by `(park, bat_side, pitch_hand, role)` → expected FPTS/PA (or run environment), league-centered.

**Skip:** using home-team display name forever as park ID (relocations/renames); uncentered raw means.

### 3. Canonical projection object: mean + SD

**Source:** `MLBAbridged.Rmd` (~776–788)

Production path was **market props → counting stats → site FPTS μ/σ**, not the research LMs alone:

```r
# Batters (DK)
DKfpts = 14*HRs + 3*Singles + 5*Doubles + 2*(RBIs + Runs + Walks)
SD = sqrt(HRsd^2 + Singlessd^2 + ... + correlation_proxy^2)

# Pitchers (DK)
DKfpts = 2*Strikeouts + 0.75*Outs - 2*ERs - 0.6*(HAs + bbs)
```

Vig was hard-coded `*1.035` when de-vigging overs/unders.

**Port:** MLB projection schema `{mean, stdev, components…}` with site scoring applied downstream (DK/FD/Yahoo as transforms). Same pattern works for other sports props.

**Skip:** frozen `1.035` vig; ad-hoc SD covariance term without estimated correlations; Windows CSV paths as ingest.

### 4. TopPct → ownership → Play/Fade EV

**Source:** `MLBAbridged.Rmd` (~998–1038)

1. 10k independent `rnorm(μ, σ)` trials  
2. Count how often each player tops each position → `TopPct`  
3. Fitted `RelativeOwn` (separate pitcher vs batter formulas)  
4. Normalize to slate shares (2P / 8B on DK classic)  
5. `PlayVFadeEV = TopPct*(1-Own) - (1-TopPct)*Own`

**Port:** product fields `topPct`, `own`, `leverageEV` beside projections. Retrain ownership coefficients on recent contest standings — **do not freeze the 2023 numbers** in the R file.

**Skip:** treating independent normals as true game correlation; treating PlayVFadeEV as ROI.

### 5. Stable builder / API contract

**Source:** `Baseball2024.js`, `baseball_data.json`

UI fields that mattered: `*-fpts`, `stdev`, `*-jvalue`, `*-own-proj`, salary, positions, Team, Opp, Order, OppPitcher. Builds often randomized with `randomizeProjection(jvalue, jvalue*2)`.

**Port:**

```ts
{
  mean: number;
  stdev: number;
  ownership: number;
  leverageOrJvalue: number; // GPP score, not mean points
  salary: number;
  positions: string[];
  team: string;
  opp: string;
  battingOrder?: number;
  oppPitcher?: string;
}
```

Randomize builds using **calibrated σ**, not `jvalue*2`. Keep mean projection separate from GPP ranking score.

---

## P1 — Strong secondary upgrades

### 6. Team / stack second stage

**Source:** `baseballr.rmd` (~886–897)

```r
lm(team_fp ~ sum_preds + park_ag_pitch + rhythm_score + fp_ema, ...)
# Adjusted R-squared: 0.3399  (vs batter ~0.11)
```

Pitchers similarly used a second-level blend (`game_fp ~ pred + mean_projection`).

**Port:** hierarchical pipeline — player model → team residual / total → redistribute into batting-order shares for stack selection.

**Skip:** abandoned multiplicative interaction soups (`A * B * C * …`).

### 7. Form with leak-safe lags

**Source:** `baseballr.rmd` (~590–720, ~628–654)

- `fp_ema = EMA(game_fp, 7)` then shifted one game back (`fp_ema_back`)  
- `rhythm_score` = sum of FPTS over a lagged window (not same-day)

**Port:** rolling form features with strict as-of dates; tune windows with time-based CV. Product “hot/cold” should be backed by these features.

**Skip:** nested R loops / swallowed `tryCatch`; contemporaneous EMA (leakage).

### 8. Pitcher volume + volatility

**Source:** `baseballr.rmd` (~471–558); `MLBDFS2023` percentiles

`mean`/`max` batters faced drove mean vs ceiling projections; `mad_fps` entered the pitcher LM denominator. Cash/GPP used `qnorm(.5/.84/.95)`.

**Port:** explicit IP/BF projection + volatility (MAD/σ) + percentile outcomes for cash vs GPP modes. Separate SP vs RP.

**Skip:** ceiling = `factors * max_BF` as a hard constraint; one model for SP and RP.

### 9. Statcast quality features

**Source:** `baseball_python.ipynb`

- Barrel-ish zones: launch speed ~100–150 mph, launch angle ~25–31.25° (`hr_friendly_launch_*_freq`) for batters and pitchers  
- Expected contact: `est_woba` / `est_slg`  
- Pitch quality: Stuff+ / Location+ / Pitching+ where available  
- Platoon reduction via `reduce_columns_by_handedness`

**Port:** feature-store columns for barrel-zone rates + xStats + pitch quality, always platoon-split.

**Skip:** unclipped raw rates; training on same-day Statcast; tiny Keras nets without calibration as production.

### 10. Game environment features

**Source:** `MLBAbridged.Rmd` (`ImpTot`, `RunLine`, `WinPct`)

Implied total, run line, and win% entered ownership models and scaled sim PA / pitcher “game quality.”

**Port:** shared environment features for all players in a game; optionally jitter environment in MC for slate uncertainty.

**Skip:** manual 30-team rename maps; fragile scraped sportsbook category IDs as the only ingest.

### 11. Correlated sim → optimizer separation

**Source:** `MLBAbridged.Rmd` (~1255–1338, ~1749–1772); `MLBDFS2023` (`cov_est`, late-swap)

Production loop: `simAtBats` / `simPitchers` → ILP maximize `build_proj` under salary, roster, locks/bans, stack rules.

**Known bug — do not port as-is:** AB loops that overwrite `pts` each PA and only keep the last PA’s hit points + accumulated RBI/R.

**Port:** clean separation of (1) projection, (2) correlated contest sim, (3) optimizer. Add real stack covariance (old `cov_est` idea) and ownership leverage (`my_own / proj_own`).

---

## P2 — Optional product surfaces

### 12. Upside / cluster ranking (separate from mean)

**Source:** `baseball_python.ipynb` (KMeans → rates of 0 / 10+ / 20+ FPTS); jvalue blends in notebook + UI

**Port:** optional GPP “upside score” from percentile or cluster membership. Expose beside — never instead of — calibrated mean FPTS.

**Skip:** opaque jvalue as the sole truth users think is “points”; unstable cluster labels without temporal checks; salary baked into the same number as projected points.

---

## Explicitly do NOT port

| Anti-pattern | Why |
|--------------|-----|
| `C:/Users/jorda/...` paths, Google Sheets as API | Not reproducible |
| Year-hardcoded B-Ref HTML scrapes / frozen DK category IDs | Already broken / fragile |
| Interaction-soup LMs + in-sample adj R² chasing | Overfit; no time CV |
| Ad-hoc duplicate-name filters (`Max Muncy`, `Will Smith`, …) | Identity must be IDs |
| Independent `rnorm` draws as “correlated game sims” | Contradicts stacking |
| Opaque jvalue as only projection | Confuses mean vs GPP rank |
| Broken AB accumulation in `simAtBats` / `simGames` | Wrong FPTS |

---

## Suggested implementation order in All-Sports-App

1. **P0.5** — Define MLB projection API contract (`mean`, `stdev`, `own`, `leverage`, matchup context).  
2. **P0.1–0.2** — Feature store: platoon + park×hand×role.  
3. **P0.3** — Prop (and/or model) → μ/σ pipeline with site scoring transforms.  
4. **P0.4** — TopPct / ownership / leverageEV (retrain Own; don’t freeze coeffs).  
5. **P1** — Team stage, form lags, pitcher volume, Statcast, game env, correlated sim+ILP.  
6. **P2** — Upside/cluster score as a separate UI surface.

## Source map (quick)

| Idea | File | Approx. lines / area |
|------|------|----------------------|
| Platoon rates + AB LMs | `baseballr.rmd` | 287–378, 873–875 |
| Park×hand | `baseballr.rmd` | 397–448 |
| EMA / rhythm | `baseballr.rmd` | 590–720 |
| Team second stage | `baseballr.rmd` | 886–897 |
| Prop → DKfpts μ/σ | `MLBAbridged.Rmd` | 776–788 |
| TopPct / Own / EV | `MLBAbridged.Rmd` | 998–1038 |
| Sim + ILP | `MLBAbridged.Rmd` | 1255–1772 |
| Statcast / clusters | `baseball_python.ipynb` | feature cells + KMeans |
| UI contract | `Baseball2024.js`, `baseball_data.json` | jvalue / fpts / own |

---

*Generated for transfer into All-Sports-App. Sibling agents should map each P0 item onto concrete modules (ingest, feature store, prediction service, UI) in that repo.*
