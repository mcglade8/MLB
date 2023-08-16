

library(lpSolve)
library(dplyr)
library(tidyverse)
library(ggplot2)
#library(coach)
library(googlesheets4)
library(R.utils)
library(tm)
library(baseballr)
library(stringi)
library(XML)
library(RCurl)
library(rlist)
options(dplyr.summarise.inform = FALSE)
library(parallel)
library(iterators)
library(foreach)
library(doParallel)
library(rlecuyer)
library(httr)
library(jsonlite)
library(magrittr)
library(rvest)
library(xml2)
library(stringi)


### run these functions before building
getData <- function(dkSalaries){
  currentDirectory <- getwd()
  
  if(!lastPull == Sys.Date()){
    player_stats()
    lastPull <- Sys.Date()
  }
  initializeSaves(currentDirectory)
  if(as.numeric(seconds(difftime(Sys.time(), lastAPIPull, units="secs"))) > 600) {
    apiPull()
    lastAPIPull <- Sys.time()
  }
  contestProjections(dkSalaries)
  preview_slate()
  
}

initializeSaves <- function(currentDirectory){
  write.csv("cleared", file = paste(currentDirectory, "/ts.csv", sep=""))
  write.csv("cleared", file = paste(currentDirectory, "/ps.csv", sep=""))
  write.csv("cleared", file = paste(currentDirectory, "/savedpool.csv", sep=""))
}

getPlayerPool <- function(currentDirectory){
  tryCatch(
    expr={
      playerPoolSaved <-read.csv(paste(currentDirectory, "/savedpool.csv", sep=""))
      
      #playerPoolSaved <-read.csv("savedpool.csv")
    }
    , error=function(e){
      playerPoolSaved <- "cleared"
    }
  )
  assign('playerPoolSaved', playerPoolSaved, envir=.GlobalEnv)
}

getps <- function(currentDirectory){
  tryCatch(
    expr={
      psSaved <-read.csv(paste(currentDirectory, "/ps.csv", sep=""))
      
      #playerPoolSaved <-read.csv("savedpool.csv")
    }
    , error=function(e){
      psSaved <- "cleared"
    }
  )
  assign('psSaved', psSaved, envir=.GlobalEnv)
}

getts <- function(currentDirectory){
  tryCatch(
    expr={
      tsSaved <-read.csv(paste(currentDirectory, "/ts.csv", sep=""))
      
      #playerPoolSaved <-read.csv("savedpool.csv")
    }
    , error=function(e){
      tsSaved <- "cleared"
    }
  )
  assign('tsSaved', tsSaved, envir=.GlobalEnv)
}

getBuiltLineups <- function(currentDirectory){
  tryCatch(
    expr={
      builtLineups <-as.data.frame(read.csv(paste(currentDirectory, "DKLineupsMLB.csv", sep="")))
      blList1 <- list()
      blList2 <- list()
      blList3 <- list()
      blList4 <- list()
      for(l in 1:nrow(builtLineups)){
        for(p in 1:ncol(builtLineups)){
          playerID <- builtLineups[l,p]
          player <- select(filter(oprojections, ID==playerID), Name, Team, Position, Salary, Opp)
          if(p==1) thisLineup <- player else{thisLineup <- rbind(thisLineup, player)}
        }
        thisLineup[nrow(thisLineup)+1,] <- c("Total", "", "", sum(thisLineup$Salary), "")
        thisLineup <- unique(thisLineup)
        if(l %% 4 == 1) blList1[[length(blList1)+1]] <- thisLineup
        if(l %% 4 == 2) blList2[[length(blList2)+1]] <- thisLineup
        if(l %% 4 == 3) blList3[[length(blList3)+1]] <- thisLineup
        if(l %% 4 == 0) blList4[[length(blList4)+1]] <- thisLineup
        
      }
    }
    , error=function(e){
      builtLineups <- "cleared"
    }
  )
  
  assign('builtLineups', builtLineups, envir = .GlobalEnv)
  assign('blList1', blList1, envir = .GlobalEnv)
  assign('blList2', blList2, envir = .GlobalEnv)
  assign('blList3', blList3, envir = .GlobalEnv)
  assign('blList4', blList4, envir = .GlobalEnv)
  
  
}

getContestData <- function(cs){
  
  # Get contest data
  
  #cid <- "148134002"
  
  player_details <- select(cs, Rank, Player, Roster.Position, X.Drafted, FPTS)
  
  player_details$Player <- sub(" Jr.", "", stri_trans_general(str = player_details$Player, id = "Latin-ASCII"))
  
  cs <- select(cs, Rank, Points, Lineup)
  
  cs$Lineup <- stri_trans_general(str = cs$Lineup, id = "Latin-ASCII")
  
  #slate_history <- list()
  # add_to_history <- list(oprojections, cs)
  # 
  # slate_history[[length(slate_history)+1]] <<-  add_to_history
  #   
  
  # Compare build to last night
  
  cashing <- cs %>%
    slice_max(order_by = Points, n = round(nrow(cs)*.25))
  rm(build_df)
  
  currentDirectory <- getwd()
  final_export <- read.csv(paste(currentDirectory, "/DKLineupsMLB.csv", sep=""))
  
  for(l in 1:nrow(final_export)){
    v <- ""
    pts <- c()
    this_l <- final_export[l,]
    for(p in 1:length(this_l)){
      guy <- this_l[p]
      guy <- oprojections$Name[which(oprojections$ID == guy[1,1])]
      guy_pts <- player_details$FPTS[which(player_details$Player == guy)]
      v <- paste0(v, guy)
      pts <- c(pts, as.numeric(guy_pts))
    }
    pts <- sum(pts)
    compare <- c(pts, v)
    compare <- compare[1:2]
    if(l==1){
      build_df <- compare
    }else{
      build_df <- rbind(build_df, compare)
    }
  }
  
  colnames(build_df) <- c("Points", "Lineup")
  build_df <- as.data.frame(build_df)
  build_df$Points <- as.numeric(build_df$Points)
  build_df$Rank <- "Mine"
  
  insert_my_build <- rbind(cashing, build_df) %>%
    slice_max(order_by = Points, n = nrow(cashing))
  
  insert_my_build <- insert_my_build[order(insert_my_build$Points, decreasing = T),]
  
  
  ###payout structure approximating a $4, 10.4k entrant contest
  payout <- c(3000, 1500, 1000, 750, 600, 500, 400, 400, 300, 300, 250, 250, 200, 200, 200, 150, 150, 150, 150, rep(100, 6), rep(75,10), rep(60, 10), rep(50, 10), rep(40,15), rep(30, 20), rep(25,20), rep(20,30), rep(15, 50), rep(10, 100), rep(8, nrow(insert_my_build)-290))
  
  
  insert_my_build$payout <- payout
  
  cost <- nrow(final_export)*4
  revenue <- sum(filter(insert_my_build, Rank == "Mine")$payout)
  best_lineups <- slice_max(filter(insert_my_build, Rank == "Mine"), order_by = Points, n = 5)
  roi <- (revenue-cost)/cost
  return(list(cost, revenue, roi, best_lineups))
  
  
  
}

### Build lineups above, then use this to edit entries for direct upload
lineupEditor <- function(plineups, final_export){
  currentDirectory <- getwd()
  #plineups <- read.csv(paste(currentDirectory, "/DKEntriesMLB.csv", sep=""))
  #View(plineups)
  
  is_sd <- "CPT" %in% colnames(plineups)
  plineups <- filter(plineups, !is.na(Entry.ID) & !is.na(as.numeric(Entry.ID)))
  tryCatch(
    expr ={
      lastBuild <- select(plineups, -Entry.ID, -Contest.Name, -Contest.ID, -Entry.Fee, -X, -Instructions)
    }, error=function(e){
      lastBuild <- select(plineups, -Entry.ID, -Contest.Name, -Contest.ID, -Entry.Fee)
    }
    
  )
  plineups <- plineups %>%
    select(Entry.ID, Contest.Name, Contest.ID, Entry.Fee)
  
  #final_export <- read.csv(paste(currentDirectory, "/DKLineupsMLB.csv", sep=""))
  
  if(nrow(final_export)==nrow(plineups)) final_export <- cbind(plineups, final_export) else{
    while(nrow(final_export) < nrow(plineups)){
      randRow <- sample(1:nrow(lastBuild), size=1)
      final_export <- rbind(final_export, lastBuild[randRow,])
    }
    final_export <- cbind(plineups, final_export)
  }
  
  if(is_sd){
    colnames(final_export) <- c("Entry ID", "Contest Name", "Contest ID", "Entry Fee", "CPT", "UTIL", "UTIL", "UTIL", "UTIL", "UTIL")
    
  }else{
    
    colnames(final_export) <- c("Entry ID", "Contest Name", "Contest ID", "Entry Fee", "P", "P", "C", "1B", "2B", "3B", "SS", "OF",  "OF", "OF")
  }
  assign('final_export', final_export, envir=.GlobalEnv)
}

apiPull <- function(){
  
  #### Run first to pull projections
  ### Game lines
  
  category <- 493
  subcategory <- 4519
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  game_lines_list <- data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]][[1]][["offerSubcategory"]][["offers"]][[1]]
  
  gl_df <- NULL
  for(l in 1:length(game_lines_list)){
    
    outcomes <- game_lines_list[[l]]$outcomes
    gl_vector <- c()
    for(o in 1:3){
      outcome <- outcomes[[o]]
      gl_vector <- c(gl_vector, outcome$label, outcome$oddsDecimal, outcome$line)
      
    }
    if(length(gl_vector)==16){
      if(is.null(gl_df)) gl_df <- gl_vector else gl_df <- rbind(gl_df, gl_vector)
    }
  }
  gl_df <- as.data.frame(gl_df)
  colnames(gl_df) <- c("Away", "Home", "runlineAodds", "runlineHodds", "runlineA", "runlineH", "over", "under", "overOdds", "underOdds", "overLine", "underLine", "a_dup", "h_dup", "mlA", "mlH")
  gl_df <- select(gl_df, -over, -under, -a_dup, -h_dup)
  
  
  
  
  ### Home Runs
  
  category <- 743
  subcategory <- 6606
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  hr_list <- Filter(Negate(is.null), Filter(Negate(is.null), data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  hr_df <- NULL
  for(l in 1:length(hr_list)){
    
    outcomes <- hr_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], hr_list[[l]]$dkPlayerId[o], outcome$oddsDecimal)
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], hr_list[[l]]$dkPlayerId[o], outcome$oddsDecimal))
      }
      
    }
    if(is.null(hr_df)) hr_df <- gl_vector else hr_df <- rbind(hr_df, gl_vector)
  }
  hr_df <- as.data.frame(hr_df)
  colnames(hr_df) <- c("Name", "dkid", "OverHR", "UnderHR")
  
  
  
  
  ### Singles
  
  category <- 743
  subcategory <- 11031
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  singles_list <-  Filter(Negate(is.null), Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  single_df <- NULL
  for(l in 1:length(singles_list)){
    
    outcomes <- singles_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], singles_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1],  singles_list[[l]]$dkPlayerId[o],outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(single_df)) single_df <- gl_vector else single_df <- rbind(single_df, gl_vector)
  }
  single_df <- as.data.frame(single_df)
  colnames(single_df) <- c("Name", "dkid", "OverSingles", "UnderSingles", "SinglesLine")
  
  
  
  
  ### Doubles
  
  category <- 743
  subcategory <- 11032
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  doubles_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  doubles_df <- NULL
  for(l in 1:length(doubles_list)){
    
    outcomes <- doubles_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], doubles_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], doubles_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(doubles_df)) doubles_df <- gl_vector else doubles_df <- rbind(doubles_df, gl_vector)
  }
  doubles_df <- as.data.frame(doubles_df)
  colnames(doubles_df) <- c("Name","dkid", "OverDoubles", "UnderDoubles", "DoublesLine")
  
  
  
  ### RBIs
  
  category <- 743
  subcategory <- 8025
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  rbi_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  rbi_df <- NULL
  for(l in 1:length(rbi_list)){
    
    outcomes <- rbi_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], rbi_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], rbi_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(rbi_df)) rbi_df <- gl_vector else rbi_df <- rbind(rbi_df, gl_vector)
  }
  rbi_df <- as.data.frame(rbi_df)
  colnames(rbi_df) <- c("Name","dkid", "OverRBIs", "UnderRBIs", "RBIsLine")
  
  
  ### Runs
  
  category <- 743
  subcategory <- 7979
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  run_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  run_df <- NULL
  for(l in 1:length(run_list)){
    
    outcomes <- run_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], run_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], run_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(run_df)) run_df <- gl_vector else run_df <- rbind(run_df, gl_vector)
  }
  run_df <- as.data.frame(run_df)
  colnames(run_df) <- c("Name", "dkid", "OverRuns", "UnderRuns", "RunsLine")
  
  
  ### Walks
  
  category <- 743
  subcategory <- 12146
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  walks_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  walks_df <- NULL
  for(l in 1:length(walks_list)){
    
    outcomes <- walks_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], walks_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], walks_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(walks_df)) walks_df <- gl_vector else walks_df <- rbind(walks_df, gl_vector)
  }
  walks_df <- as.data.frame(walks_df)
  colnames(walks_df) <- c("Name","dkid", "OverWalks", "UnderWalks", "WalksLine")
  
  
  ### Steals
  
  
  category <- 743
  subcategory <- 9872
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  steals_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  steals_df <- NULL
  for(l in 1:length(steals_list)){
    
    outcomes <- steals_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], steals_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], steals_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(steals_df)) steals_df <- gl_vector else steals_df <- rbind(steals_df, gl_vector)
  }
  steals_df <- as.data.frame(steals_df)
  colnames(steals_df) <- c("Name", "dkid", "OverSteals", "UnderSteals", "StealsLine")
  
  
  
  ### Outs recorded
  
  
  category <- 1031
  subcategory <- 9883
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  outs_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  outs_df <- NULL
  for(l in 1:length(outs_list)){
    
    outcomes <- outs_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], outs_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], outs_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(outs_df)) outs_df <- gl_vector else outs_df <- rbind(outs_df, gl_vector)
  }
  outs_df <- as.data.frame(outs_df)
  colnames(outs_df) <- c("Name","dkid", "OverOuts", "UnderOuts", "OutsLine")
  
  
  ### Strikeouts recorded
  
  
  category <- 1031
  subcategory <- 9885
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  strikeouts_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  strikeouts_df <- NULL
  for(l in 1:length(strikeouts_list)){
    
    outcomes <- strikeouts_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], strikeouts_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], strikeouts_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(strikeouts_df)) strikeouts_df <- gl_vector else strikeouts_df <- rbind(strikeouts_df, gl_vector)
  }
  strikeouts_df <- as.data.frame(strikeouts_df)
  colnames(strikeouts_df) <- c("Name","dkid", "OverStrikeouts", "UnderStrikeouts", "StrikeoutsLine")
  
  
  ### Earned Runs
  
  
  category <- 1031
  subcategory <- 11064
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  ers_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  ers_df <- NULL
  for(l in 1:length(ers_list)){
    
    outcomes <- ers_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], ers_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], ers_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(ers_df)) ers_df <- gl_vector else ers_df <- rbind(ers_df, gl_vector)
  }
  ers_df <- as.data.frame(ers_df)
  colnames(ers_df) <- c("Name","dkid", "OverERs", "UnderERs", "ERsLine")
  
  
  ### Hits Allowed
  
  
  category <- 1031
  subcategory <- 11035
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  HAs_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  HAs_df <- NULL
  for(l in 1:length(HAs_list)){
    
    outcomes <- HAs_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], HAs_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1], HAs_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(HAs_df)) HAs_df <- gl_vector else HAs_df <- rbind(HAs_df, gl_vector)
  }
  HAs_df <- as.data.frame(HAs_df)
  colnames(HAs_df) <- c("Name","dkid", "OverHAs", "UnderHAs", "HAsLine")
  
  
  
  ### BBs Allowed
  
  
  category <- 1031
  subcategory <- 9886
  
  
  url = gsub(" ", "", paste0('https://sportsbook.draftkings.com//sites/US-SB/api/v5/eventgroups/84240/categories/',category,'/subcategories/',subcategory,'?format=json'))
  response <- GET(url)
  data <- fromJSON(rawToChar(response$content))
  
  bbs_list <- Filter(Negate(is.null),Filter(Negate(is.null),data[["eventGroup"]][["offerCategories"]][["offerSubcategoryDescriptors"]])[[1]][["offerSubcategory"]][["offers"]])[[1]]
  
  bbs_df <- NULL
  for(l in 1:length(bbs_list)){
    
    outcomes <- bbs_list[[l]]$outcomes
    gl_vector <- NULL
    for(o in 1:length(outcomes)){
      outcome <- outcomes[[o]]
      if(is.null(gl_vector)){
        gl_vector <- c(outcome$participant[1], bbs_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1])
      }else{
        gl_vector <- rbind(gl_vector, c(outcome$participant[1],bbs_list[[l]]$dkPlayerId[o], outcome$oddsDecimal, outcome$line[1]))
      }
      
    }
    if(is.null(bbs_df)) bbs_df <- gl_vector else bbs_df <- rbind(bbs_df, gl_vector)
  }
  bbs_df <- as.data.frame(bbs_df)
  colnames(bbs_df) <- c("Name","dkid", "Overbbs", "Underbbs", "bbsLine")
  
  
  
  batter_lines <- merge(hr_df, single_df, all = T) %>%
    merge(doubles_df, all = T) %>%
    merge(walks_df, all = T) %>%
    merge(steals_df, all = T) %>%
    merge(rbi_df, all = T) %>%
    merge(run_df, all = T)
  
  pitcher_lines <- merge(outs_df, strikeouts_df, all = T) %>%
    merge(ers_df, all = T) %>%
    merge(HAs_df, all = T) %>%
    merge(bbs_df, all = T)
  
  for(c in 3:ncol(batter_lines)){
    batter_lines[,c] <- as.numeric(batter_lines[,c])
  }
  
  
  batter_lines <-mutate(batter_lines,
                        HRs = 1/(as.numeric(OverHR)*1.035),
                        HRsd = sqrt(((.5-HRs)*14)^2*(100-100*((1/(OverHR*1.035))/((1/(OverHR*1.035))+((1/(UnderHR*1.035))))))/100),
                        Singles = (1/(as.numeric(OverSingles)*1.035)+SinglesLine+ SinglesLine- 1/(as.numeric(UnderSingles)*1.035))/2,
                        Singlessd = sqrt((((SinglesLine-.5-Singles)*3)^2*(100-100*((1/(OverSingles*1.035))/((1/(OverSingles*1.035))+((1/(UnderSingles*1.035))))))+((SinglesLine+.5-Singles)*3)^2*(100-100*((1/(OverSingles*1.035))/((1/(OverSingles*1.035))+((1/(UnderSingles*1.035)))))))/100),
                        Doubles = (1/(as.numeric(OverDoubles)*1.035)+DoublesLine+ DoublesLine- 1/(as.numeric(UnderDoubles)*1.035))/2,
                        Doublesd = sqrt((((DoublesLine-.5-Doubles)*3)^2*(100-100*((1/(OverDoubles*1.035))/((1/(OverDoubles*1.035))+((1/(UnderDoubles*1.035))))))+((DoublesLine+.5-Doubles)*3)^2*(100-100*((1/(OverDoubles*1.035))/((1/(OverDoubles*1.035))+((1/(UnderDoubles*1.035)))))))/100),
                        Walks = 1/(as.numeric(OverWalks)*1.035),
                        Walkssd = sqrt((((WalksLine-.5-Walks)*3)^2*(100-100*((1/(OverWalks*1.035))/((1/(OverWalks*1.035))+((1/(UnderWalks*1.035))))))+((WalksLine+.5-Walks)*3)^2*(100-100*((1/(OverWalks*1.035))/((1/(OverWalks*1.035))+((1/(UnderWalks*1.035)))))))/100),
                        Steals = 1/(as.numeric(OverSteals)*1.035),
                        Stealssd = sqrt((((StealsLine-.5-Steals)*3)^2*(100-100*((1/(OverSteals*1.035))/((1/(OverSteals*1.035))+((1/(UnderSteals*1.035))))))+((WalksLine+.5-Steals)*3)^2*(100-100*((1/(OverSteals*1.035))/((1/(OverSteals*1.035))+((1/(UnderSteals*1.035)))))))/100),
                        RBIs = (1/(as.numeric(OverRBIs)*1.035)+RBIsLine+RBIsLine- 1/(as.numeric(UnderRBIs)*1.035))/2,
                        RBIssd = sqrt((((RBIsLine-.5-RBIs)*3)^2*(100-100*((1/(OverRBIs*1.035))/((1/(OverRBIs*1.035))+((1/(UnderRBIs*1.035))))))+((RBIsLine+.5-RBIs)*3)^2*(100-100*((1/(OverRBIs*1.035))/((1/(OverRBIs*1.035))+((1/(UnderRBIs*1.035)))))))/100),
                        Runs = (1/(as.numeric(OverRuns)*1.035)+RunsLine+ RunsLine- 1/(as.numeric(UnderRuns)*1.035))/2,
                        Runssd = sqrt((((RunsLine-.5-Runs)*3)^2*(100-100*((1/(OverRuns*1.035))/((1/(OverRuns*1.035))+((1/(UnderRuns*1.035))))))+((RunsLine+.5-Runs)*3)^2*(100-100*((1/(OverRuns*1.035))/((1/(OverRuns*1.035))+((1/(UnderRuns*1.035)))))))/100)
  )
  
  batter_lines[is.na(batter_lines)] <- 0.03
  
  for(c in 3:ncol(pitcher_lines)){
    pitcher_lines[,c] <- as.numeric(pitcher_lines[,c])
  }
  
  for(i in 1:nrow(pitcher_lines)){
    if(is.na(pitcher_lines$OutsLine[i])){
      personal_sum <- pitcher_lines$StrikeoutsLine[i] + pitcher_lines$HAsLine[i] + pitcher_lines$bbsLine[i]
      group_sum <- mean(pitcher_lines$StrikeoutsLine, na.rm= T) + mean(pitcher_lines$HAsLine, na.rm= T) + mean(pitcher_lines$bbsLine, na.rm= T)
      group_outs <- mean(pitcher_lines$OutsLine, na.rm=T)
      
      pitcher_lines$OutsLine[i] <- personal_sum*group_outs/group_sum
      pitcher_lines$OverOuts[i] <- 2
      pitcher_lines$UnderOuts[i] <- 2
    }
  }
  
  pitcher_lines <- mutate(pitcher_lines,
                          Outs = (1/(as.numeric(OverOuts)*1.035)+OutsLine+ OutsLine- 1/(as.numeric(UnderOuts)*1.035))/2,
                          Outssd = sqrt((((OutsLine-.5-Outs)*3)^2*(100-100*((1/(OverOuts*1.035))/((1/(OverOuts*1.035))+((1/(UnderOuts*1.035))))))+((OutsLine+.5-Outs)*3)^2*(100-100*((1/(OverOuts*1.035))/((1/(OverOuts*1.035))+((1/(UnderOuts*1.035)))))))/100),
                          Strikeouts = (1/(as.numeric(OverStrikeouts)*1.035)+StrikeoutsLine+ StrikeoutsLine- 1/(as.numeric(UnderStrikeouts)*1.035))/2,
                          Strikeoutssd = sqrt((((StrikeoutsLine-.5-Strikeouts)*3)^2*(100-100*((1/(OverStrikeouts*1.035))/((1/(OverStrikeouts*1.035))+((1/(UnderStrikeouts*1.035))))))+((StrikeoutsLine+.5-Strikeouts)*3)^2*(100-100*((1/(OverStrikeouts*1.035))/((1/(OverStrikeouts*1.035))+((1/(UnderStrikeouts*1.035)))))))/100),
                          ERs = (1/(as.numeric(OverERs)*1.035)+ERsLine+ ERsLine- 1/(as.numeric(UnderERs)*1.035))/2,
                          ERssd = sqrt((((ERsLine-.5-ERs)*3)^2*(100-100*((1/(OverERs*1.035))/((1/(OverERs*1.035))+((1/(UnderERs*1.035))))))+((ERsLine+.5-ERs)*3)^2*(100-100*((1/(OverERs*1.035))/((1/(OverERs*1.035))+((1/(UnderERs*1.035)))))))/100),
                          HAs = (1/(as.numeric(OverHAs)*1.035)+HAsLine+ HAsLine- 1/(as.numeric(UnderHAs)*1.035))/2,
                          HAssd = sqrt((((HAsLine-.5-HAs)*3)^2*(100-100*((1/(OverHAs*1.035))/((1/(OverHAs*1.035))+((1/(UnderHAs*1.035))))))+((HAsLine+.5-HAs)*3)^2*(100-100*((1/(OverHAs*1.035))/((1/(OverHAs*1.035))+((1/(UnderHAs*1.035)))))))/100),
                          bbs = (1/(as.numeric(Overbbs)*1.035)+bbsLine+ bbsLine- 1/(as.numeric(Underbbs)*1.035))/2,
                          bbssd = sqrt((((bbsLine-.5-bbs)*3)^2*(100-100*((1/(Overbbs*1.035))/((1/(Overbbs*1.035))+((1/(Underbbs*1.035))))))+((bbsLine+.5-bbs)*3)^2*(100-100*((1/(Overbbs*1.035))/((1/(Overbbs*1.035))+((1/(Underbbs*1.035)))))))/100)
  )
  
  
  batter_lines <- mutate(batter_lines,
                         DKfpts = 14*HRs + 3*Singles + 5*Doubles + 2*(RBIs + Runs + Walks),
                         SD = sqrt(HRsd^2+Singlessd^2+Doublesd^2+RBIssd^2+Runssd^2+Walkssd^2+(RBIssd*.7+Runssd*.3+HRsd+Stealssd)^2)
  )
  for(pr in 1:nrow(pitcher_lines)) for(pc in 1:ncol(pitcher_lines)){
    if(is.na(pitcher_lines[pr,pc])){
      pitcher_lines[pr,pc]<- pitcher_lines$Strikeouts[pr]/mean(pitcher_lines$Strikeouts, na.rm=T)*mean(pitcher_lines[,c], na.rm=T)
    }  
  }
  
  pitcher_lines <- mutate(pitcher_lines,
                          DKfpts = 2 * Strikeouts + .75 * Outs - 2*ERs -.6*(HAs+bbs),
                          SD = sqrt((Strikeoutssd*2.75)^2 + (Outssd*.75)^2 + (ERssd*2)^2 + (.6*HAssd)^2 + (.6*bbssd)^2 + (2.75*Strikeoutssd + .75*Outssd + 2*ERssd + .6*HAssd +.6*bbssd)^2)
                          
  )
  
  for(c in 3:ncol(gl_df)){
    gl_df[,c] <- as.numeric(gl_df[,c])
  }
  
  batter_lines <- batter_lines %>%
    mutate(Name = gsub(" Jr.", "", Name))
  pitcher_lines <- pitcher_lines %>%
    mutate(Name = gsub(" Jr.", "", Name))
  
  gl_df <- gl_df %>%
    group_by(Away) %>%
    slice(1)
  
  batter_lines <- merge(batter_lines, abs_rate, all.x=T) %>%
    merge(hr_rate, all.x=T)
  
  batter_lines <- batter_lines %>%
    group_by(Name) %>%
    slice(1)
  
  away <- gl_df$Away
  home <- gl_df$Home
  away_rl <- gl_df$runlineA/gl_df$runlineAodds
  home_rl <- gl_df$runlineH/gl_df$runlineHodds
  away_imp <- (gl_df$overLine-away_rl)/2
  home_imp <-  (gl_df$overLine-home_rl)/2
  away_win <- 1/gl_df$mlA
  home_win <- 1/gl_df$mlH
  
  away_df <- as.data.frame(cbind(away, away_rl, away_imp, away_win))
  home_df <- as.data.frame(cbind(home, home_rl, home_imp, home_win))
  
  away_df <- rename(away_df, Team = away, RunLine = away_rl, ImpTot = away_imp, WinPct = away_win)
  away_df$Opp <- home
  home_df <- rename(home_df, Team = home, RunLine = home_rl, ImpTot = home_imp, WinPct = home_win)
  home_df$Opp <- away
  
  game_lines <- as.data.frame(rbind(away_df, home_df))
  
  game_lines <- game_lines %>%
    mutate(Team = case_when(
      Team == "STL Cardinals" ~ "STL",
      Team == "TOR Blue Jays" ~ "TOR",
      Team == "KC Royals" ~ "KC",
      Team == "CHI Cubs" ~ "CHC",
      Team == "COL Rockies" ~ "COL", 
      Team == "BOS Red Sox" ~ "BOS", 
      Team == "NY Mets" ~ "NYM",
      Team == "ARI Diamondbacks" ~ "ARI",
      Team == "TEX Rangers"~ "TEX",
      Team == "SD Padres" ~ "SD",
      Team == "ATL Braves" ~ "ATL",
      Team == "BAL Orioles" ~ "BAL",
      Team == "SEA Mariners" ~ "SEA",
      Team == "LA Dodgers" ~ "LAD",
      Team == "MIA Marlins" ~ "MIA",
      Team == "DET Tigers" ~ "DET",
      Team == "PIT Pirates" ~ "PIT",
      Team == "CIN Reds" ~ "CIN",
      Team == "MIN Twins" ~ "MIN",
      Team == "HOU Astros" ~ "HOU",
      Team == "MIL Brewers" ~ "MIL",
      Team == "CHI White Sox" ~ "CWS",
      Team == "SF Giants" ~ "SF",
      Team == "PHI Phillies" ~ "PHI",
      Team == "TB Rays" ~ "TB",
      Team == "NY Yankees" ~ "NYY",
      Team == "CLE Guardians" ~ "CLE",
      Team == "LA Angels" ~ "LAA",
      Team == "WAS Nationals" ~ "WAS",
      Team == "OAK Athletics" ~ "OAK"
    ), Opp = case_when(
      Opp == "STL Cardinals" ~ "STL",
      Opp == "TOR Blue Jays" ~ "TOR",
      Opp == "KC Royals" ~ "KC",
      Opp == "CHI Cubs" ~ "CHC",
      Opp == "COL Rockies" ~ "COL", 
      Opp == "BOS Red Sox" ~ "BOS", 
      Opp == "NY Mets" ~ "NYM",
      Opp == "ARI Diamondbacks" ~ "ARI",
      Opp == "TEX Rangers"~ "TEX",
      Opp == "SD Padres" ~ "SD",
      Opp == "ATL Braves" ~ "ATL",
      Opp == "BAL Orioles" ~ "BAL",
      Opp == "SEA Mariners" ~ "SEA",
      Opp == "LA Dodgers" ~ "LAD",
      Opp == "MIA Marlins" ~ "MIA",
      Opp == "DET Tigers" ~ "DET",
      Opp == "PIT Pirates" ~ "PIT",
      Opp == "CIN Reds" ~ "CIN",
      Opp == "MIN Twins" ~ "MIN",
      Opp == "HOU Astros" ~ "HOU",
      Opp == "MIL Brewers" ~ "MIL",
      Opp == "CHI White Sox" ~ "CWS",
      Opp == "SF Giants" ~ "SF",
      Opp == "PHI Phillies" ~ "PHI",
      Opp == "TB Rays" ~ "TB",
      Opp == "NY Yankees" ~ "NYY",
      Opp == "CLE Guardians" ~ "CLE",
      Opp == "LA Angels" ~ "LAA",
      Opp == "WAS Nationals" ~ "WAS",
      Opp == "OAK Athletics" ~ "OAK"
    ))
  
  game_pks_today <- as.data.frame(mlb_game_pks(Sys.Date()))
  batting_orders <- data.frame()
  for(i in 1:nrow(game_pks_today)){
    batting_orders <- rbind(batting_orders, mlb_batting_orders(game_pks_today$game_pk[i]))
  }
  batting_orders <- select(batting_orders, fullName, batting_order) %>%
    rename(Name = fullName) %>%
    mutate(batting_order = as.numeric(batting_order))
  
  assign('batter_lines', batter_lines, envir=.GlobalEnv)
  assign('pitcher_lines', pitcher_lines, envir=.GlobalEnv)
  assign('batting_orders', batting_orders, envir=.GlobalEnv)
  assign('game_lines', game_lines, envir=.GlobalEnv)
  
}

holisticData <- function(){
  
  
  oprojections <- oprojections%>%
    filter(!is.na(Name), DKfpts >0, !(Name == "Max Muncy" & !Team == "LAD"), !(Name == "Will Smith" & !Team == "LAD"), !(Name == "Luis Ortiz" & !Team == "PIT"), !(Name == "Carlos Perez" &!Team == "OAK"), !(Name == "Julio Rodriguez" &!Team == "SEA")) %>%
    filter(!Opp == "0") %>%
    merge(batting_orders, all.x = T)
  
  oprojections$batting_order[is.na(oprojections$batting_order)] <- 0
  
  
  #pb <- txtProgressBar(0, num_lineups, style = 3)
  
  pitcher_summary <- select(oprojections, Name, Position, Salary, DKfpts, SD, Team, Opp) %>%
    filter(Position == "P") %>%
    select(-Position) %>%
    merge(pitcher_stats, by = "Name") %>%
    mutate(points_needed = Salary*.0015+14, odds_to_hit = 1-pnorm(points_needed, mean = DKfpts, sd = SD))
  pitcher_summary <- pitcher_summary[order(pitcher_summary$odds_to_hit, decreasing = T),]
  
  team_summary <- oprojections %>%
    filter(!Position == "P") %>%
    group_by(Team) %>%
    summarise(DKfpts = 9*mean(DKfpts), SD = mean(SD), Own = 9*mean(Own), HRs = 9*mean(HRs), EV = 9*mean(PlayVFadeEV)) %>%
    mutate(SD = sqrt(9*SD^2 + 3*SD), best_outcome = qnorm(.85, mean = DKfpts, sd = SD))
  
  
  assign('oprojections', oprojections, envir=.GlobalEnv)
  assign('pitcher_summary', pitcher_summary, envir=.GlobalEnv)
  assign('team_summary', team_summary, envir=.GlobalEnv)
}

contestProjections <- function(dkSalaries){
  #### Run second to build oprojections suitable for mcapply function
  
  
  DKsalaries <- dkSalaries %>%
    rename(Team = TeamAbbrev) %>%
    mutate(Name = gsub(" Jr.", "", Name))
  
  todays_batters <- batter_lines %>%
    select(Name, DKfpts, SD, HRs) %>%
    mutate(Name = gsub(" Jr.", "", Name)) %>%
    merge(DKsalaries, all.x=T) %>%
    filter(!(Name == "Max Muncy" & !Team == "LAD"), !(Name == "Will Smith" & !Team == "LAD"), !(Name == "Luis Ortiz" & !Team == "PIT"), !(Name == "Carlos Perez" &!Team == "OAK"), !(Name == "Julio Rodriguez" &!Team == "SEA")) %>%
    filter(!is.na(Team))
  
  
  todays_pitchers <- pitcher_lines %>%
    select(Name, DKfpts, SD) %>%
    mutate(Name = gsub(" Jr.", "", Name)) %>%
    merge(DKsalaries, all.x=T) %>%
    filter(!(Name == "Max Muncy" & !Team == "LAD"), !(Name == "Will Smith" & !Team == "LAD"), !(Name == "Luis Ortiz" & !Team == "PIT"), !(Name == "Carlos Perez" &!Team == "OAK"), !(Name == "Julio Rodriguez" &!Team == "SEA")) %>%
    filter(!is.na(Team)) %>%
    mutate(Position = "P")
  todays_pitchers$HRs <- 0
  
  ### Helps identify if I'm missing batters (<9 on a team)
  numbats <- todays_batters %>%
    group_by(Team) %>%
    summarize(n())
  
  oprojections <- rbind(todays_pitchers, todays_batters)
  oprojections <- merge(oprojections, game_lines, all.x = T)
  
  oprojections[is.na(oprojections)] <- 0
  oprojections$RunLine <- as.numeric(oprojections$RunLine)
  oprojections$ImpTot <- as.numeric(oprojections$ImpTot)
  oprojections$WinPct <- as.numeric(oprojections$WinPct)
  
  is_sd <- "CPT" %in% oprojections$Roster.Position
  
  if(is_sd){
    
  }else{
    ### Calc top pct
    
    top_c <- rep(0, nrow(oprojections))
    top_1 <- rep(0, nrow(oprojections))
    top_2 <- rep(0, nrow(oprojections))
    top_3 <- rep(0, nrow(oprojections))
    top_s <- rep(0, nrow(oprojections))
    top_o <- rep(0, nrow(oprojections))
    top_p <- rep(0, nrow(oprojections))
    
    for(i in 1:10000){
      trial <- rnorm(nrow(oprojections), oprojections$DKfpts, oprojections$SD)
      trialc <- trial*as.numeric(grepl( "C",oprojections$Position, fixed = T))
      top_c <- top_c + as.numeric(trialc==trialc[which(trialc==max(trialc))])
      trial1 <- trial*as.numeric(grepl( "1",oprojections$Position, fixed = T))
      top_1 <- top_1 + as.numeric(trial1==trial1[which(trial1==max(trial1))])
      trial2 <- trial*as.numeric(grepl( "2",oprojections$Position, fixed = T))
      top_2 <- top_2 + as.numeric(trial2==trial2[which(trial2==max(trial2))])
      trial3 <- trial*as.numeric(grepl( "3",oprojections$Position, fixed = T))
      top_3 <- top_3 + as.numeric(trial3==trial3[which(trial3==max(trial3))])
      trials <- trial*as.numeric(grepl( "SS",oprojections$Position, fixed = T))
      top_s <- top_s + as.numeric(trials==trials[which(trials==max(trials))])
      trialo <- trial*as.numeric(grepl( "OF",oprojections$Position, fixed = T))
      top_o <- top_o + as.numeric(trialo==trialo[which(trialo==max(trialo))])
      trialp <- trial*as.numeric(grepl( "P",oprojections$Position, fixed = T))
      top_p <- top_p + as.numeric(trialp==trialp[which(trialp==max(trialp))])
    }
    
    top_pct <- (top_c+top_1+top_2+top_3+top_s+top_o*3+top_p*2)/10000
    
    ### Calc own and EV
    
    
    oprojections$TopPct <- top_pct
    oprojections <- mutate(oprojections, RelativeOwn = case_when(
      Position == "P" ~ 0.155767+0.013602*DKfpts-0.051832*(ImpTot + 2*RunLine)-0.025464*ImpTot + 0.684621*TopPct,
      T ~ -0.26554+0.26405*HRs+.31816*WinPct+.01932*(ImpTot + 2*RunLine)+.13054*TopPct/Salary*10000))
    
    oprojections$RelativeOwn[oprojections$RelativeOwn<0.01] <- .01
    
    relownP <- sum(oprojections$RelativeOwn * as.numeric(oprojections$Position == "P"))
    relownB <- sum(oprojections$RelativeOwn * as.numeric(!oprojections$Position == "P"))
    
    oprojections$Own <- case_when(
      oprojections$Position == "P" ~ oprojections$RelativeOwn/relownP*2,
      T ~ oprojections$RelativeOwn/relownB*8
    )
    
    
    
    oprojections<- mutate(oprojections, PlayVFadeEV = TopPct*(1-Own)-(1-TopPct)*Own )
    
    
    oprojections <- oprojections
    oprojections$onslate <- F
    for(n in 1:nrow(oprojections)){
      oprojections$onslate[n] <- grepl(oprojections$Opp[n], oprojections$Game.Info[n], fixed = T)
    }
    oprojections <- filter(oprojections, onslate == T) %>%
      select(-onslate)
    
    oprojections <- oprojections %>%
      group_by(Name) %>%
      slice_max(order_by = DKfpts, n = 1) %>%
      ungroup()
  }
  assign('oprojections', oprojections, envir=.GlobalEnv)
  
  holisticData()
}

player_stats <- function(){
  ### Scrape advanced pitcher data
  theurl <- rvest::read_html("https://www.baseball-reference.com/leagues/majors/2023-advanced-pitching.shtml")
  tables <- rvest::html_nodes(theurl, xpath = '//comment()') %>%
    rvest::html_text() %>%
    paste(collapse = '') %>%    # collapse to a single string
    rvest::read_html() %>%    # reparse to HTML
    rvest::html_node('table#players_advanced_pitching') %>%    # select the desired table
    rvest::html_table() %>%    # parse table
    .[colSums(is.na(.)) < nrow(.)]
  
  colnames(tables) <- tables[1,]
  
  tables <- filter(tables, !Rk == "Rk")
  test <-  tables
  
  for(c in 2: ncol(test)){
    fix <- test[,c]
    temp <- c()
    for(i in 1:nrow(fix)){
      temp[i] <- gsub('%', '', gsub('\\*', '', fix[i,1]))
    }
    
    test[,c] <- temp
  }
  
  test$Name <- stri_trans_general(str = test$Name, id = "Latin-ASCII")
  
  test <- select(test, Name, 'HR%', 'SO%', 'BB%', EV, 'HardH%', 'LD%', 'GB%', 'FB%')
  colnames(test) <- c("Name", "HR", "SO", "BB", "EV", "HardH", "LD", "GB", "FB")
  
  adv <- test
  #### scrape basic pitcher data
  
  theurl <- rvest::read_html("https://www.baseball-reference.com/leagues/majors/2023-standard-pitching.shtml")
  tables <- rvest::html_nodes(theurl, xpath = '//comment()') %>%
    rvest::html_text() %>%
    paste(collapse = '') %>%    # collapse to a single string
    rvest::read_html() %>%    # reparse to HTML
    rvest::html_node('table#players_standard_pitching') %>%    # select the desired table
    rvest::html_table() %>%    # parse table
    .[colSums(is.na(.)) < nrow(.)]
  
  tables <- filter(tables, !Rk == "Rk")
  test <-  tables
  
  for(c in 2: ncol(test)){
    fix <- test[,c]
    temp <- c()
    for(i in 1:nrow(fix)){
      temp[i] <- gsub('%', '', gsub('\\*', '', fix[i,1]))
    }
    
    test[,c] <- temp
  }
  
  test$Name <- stri_trans_general(str = test$Name, id = "Latin-ASCII")
  
  test <- select(test, Name, IP, WHIP)
  
  basic <- test
  
  ### Combine the two
  
  pitcher_stats <- merge(basic, adv)
  pitcher_stats <- as.data.frame(pitcher_stats)
  for(c in 2:11){
    pitcher_stats[,c] <- as.numeric(pitcher_stats[,c])
  }
  sample <- pitcher_stats %>%
    filter(IP > 10) %>%
    select(-IP)
  
  means <- sample %>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  sds <- sample %>%
    summarise_if(is.numeric, sd, na.rm = TRUE)
  
  standardize <- select(pitcher_stats, colnames(sample))
  
  for(s in 1:nrow(standardize)){
    standardize[s, 2:10]<-(standardize[s, 2:10]-means[1,])/sds[1,]
  }
  
  hh_factor <- abs(min(standardize$HardH, na.rm = TRUE))+abs(max(standardize$HardH, na.rm = TRUE))
  FB_factor <- abs(min(standardize$FB, na.rm = TRUE))+abs(max(standardize$FB, na.rm = TRUE))
  st_factor <- hh_factor + FB_factor
  
  standardize <- mutate(standardize, opp_hitter_friendly = 1.5*HR + ((st_factor+HardH)*(st_factor + FB))/st_factor-st_factor - .5*GB + WHIP, fantasy_pitcher_value = 1.5*SO - WHIP - .5*BB) %>%
    select(Name, opp_hitter_friendly, fantasy_pitcher_value)
  
  pitcher_stats <- merge(pitcher_stats, standardize)
  
  pitcher_stats <- group_by(pitcher_stats, Name) %>%
    slice_max(order_by = IP, n = 1, with_ties = FALSE)
  
  
  assign('pitcher_stats', pitcher_stats, envir=.GlobalEnv)
  
  
  ### Scrape standard batter data
  theurl <- rvest::read_html("https://www.baseball-reference.com/leagues/majors/2023-standard-batting.shtml")
  table_one <- xml_find_all(theurl, "//table") %>% html_table
  
  # Additional tables are within the comment tags, ie <!-- tables -->
  # Which is why your xpath is missing them.
  # First get the commented nodes
  alt_tables <- xml2::xml_find_all(theurl,"//comment()") %>% {
    #Find only commented nodes that contain the regex for html table markup
    raw_parts <- as.character(.[grep("\\</?table", as.character(.))])
    # Remove the comment begin and end tags
    strip_html <- stringi::stri_replace_all_regex(raw_parts, c("<\\!--","-->"),c("",""),
                                                  vectorize_all = FALSE)
    # Loop through the pieces that have tables within markup and 
    # apply the same functions
    lapply(grep("<table", strip_html, value = TRUE), function(i){
      rvest::html_table(xml_find_all(read_html(i), "//table")) %>% 
        .[[1]]
    })
  }
  # Put all the data frames into a list.
  all_tables <- c(
    table_one, alt_tables
  )
  
  batter_stats <- all_tables[[2]]
  
  fix <- gsub("#", "", gsub("\\*", "", batter_stats$Name ))
  fix <- sub(" Jr.", "", stri_trans_general(str = fix, id = "Latin-ASCII"))
  
  batter_stats$Name <- fix
  
  batter_stats <- group_by(batter_stats, Name) %>% slice_max(order_by= PA, n=1) %>% ungroup()
  batter_stats <- as.data.frame(batter_stats)
  for(i in 6:(ncol(batter_stats)-1)){
    batter_stats[,i] <- as.numeric(batter_stats[,i])
  }
  
  hr_rate <- batter_stats %>%
    filter(PA > 100) %>%
    mutate(hr_rate = HR/PA) %>%
    select(Name, hr_rate)
  
  abs_rate <- batter_stats %>%
    mutate(abs_rate = PA/G) %>%
    select(Name, abs_rate)
  
  assign('batter_stats', batter_stats, envir=.GlobalEnv)
  assign('abs_rate', abs_rate, envir=.GlobalEnv)
  assign('hr_rate', hr_rate, envir=.GlobalEnv)
}

preview_slate <- function(){
  
  position_strength <- oprojections %>%
    mutate(Cs = case_when(grepl("C", Position, fixed = T)~ DKfpts, T ~ 0), B1s = case_when(grepl("1B", Position, fixed = T)~ DKfpts, T ~ 0), B2s = case_when(grepl("2B", Position, fixed = T)~ DKfpts, T ~ 0), B3s = case_when(grepl("3B", Position, fixed = T)~ DKfpts, T ~ 0), SSs = case_when(grepl("SS", Position, fixed = T)~ DKfpts, T ~ 0), OFs = case_when(grepl("OF", Position, fixed = T)~ DKfpts, T ~ 0))
  
  c_strength <- sum(position_strength$Cs)
  b2_strength <- sum(position_strength$B2s)
  b3_strength <- sum(position_strength$B3s)
  ss_strength <- sum(position_strength$SSs)
  b1_strength <- sum(position_strength$B1s)
  of_strength <- sum(position_strength$OFs)/3
  
  actual_hr_rate <- oprojections %>%
    merge(hr_rate) %>%
    select(Name, Opp, HRs, hr_rate)
  
  p_al_hr <- pitcher_summary %>%
    mutate(op_hr_rate = HR/100)%>%
    select(Opp, op_hr_rate) 
  
  actual_hr_rate <- merge(actual_hr_rate, p_al_hr)
  actual_hr_rate <- merge(actual_hr_rate, abs_rate)
  
  actual_hr_rate <- mutate(actual_hr_rate, correct_hrs = (.75*hr_rate + .25*op_hr_rate)*abs_rate)
  
  actual_hr_rate <- select(actual_hr_rate, Name, correct_hrs)
  
  batter_lines <- merge(batter_lines, actual_hr_rate, all.x=T) %>%
    mutate(HRs = case_when(is.na(correct_hrs) ~ HRs, T ~ correct_hrs)) %>%
    select(-correct_hrs)
  
  assign('batter_lines', batter_lines, envir = .GlobalEnv)
  assign('c_strength', c_strength, envir = .GlobalEnv)
  assign('b2_strength', b2_strength, envir = .GlobalEnv)
  assign('b3_strength', b3_strength, envir = .GlobalEnv)
  assign('ss_strength', ss_strength, envir = .GlobalEnv)
  assign('b1_strength', b1_strength, envir = .GlobalEnv)
  assign('of_strength', of_strength, envir = .GlobalEnv)
  
}

simGames <- function (oprojections, batter_lines, pitcher_lines, batting_orders)  {
  all_players <- oprojections %>%
    select(Name, Team, Opp, Position)
  
  opp_pitchers <- all_players %>%
    filter(Position == "P") %>%
    select(Name, Opp) %>%
    merge(pitcher_lines) %>%
    rename(Team = Opp, OppP = Name) %>%
    mutate(eAB = Outs+HAs+bbs, soRate = Strikeouts/eAB, hRate = HAs/eAB, bbRate = bbs/eAB) %>%
    select(Team, OppP, soRate, hRate, bbRate, eAB)
  
  all_batter_info <- batter_lines %>%
    merge(batting_orders, all.x = T) %>%
    merge(all_players) %>%
    select(Name, HRs, Singles, Doubles, Walks, Steals, RBIs, Runs, batting_order, Team, Opp, abs_rate)%>%
    merge(opp_pitchers, all.x=T)
  
  teams <- unique(all_batter_info$Team)
  build <- NULL
  buildPitchers <- NULL
  
  ### Loops through at bats for each team on each game
  for(n in 1:length(teams)){
    
    team <- all_batter_info %>% filter(Team == teams[n])
    missing_batting_order <- c(1:9)[!c(1:9) %in% team$batting_order]
    team$abs_rate[is.na(team$abs_rate)] <- min(team$abs_rate, na.rm = T)
    
    team <- team[order(team$HRs, decreasing = TRUE),]
    
    for(i in 1:nrow(team)){
      if(is.na(team$batting_order[i])){
        team$batting_order[i] <- missing_batting_order[1]
        missing_batting_order <- missing_batting_order[-1]
      }
    }
    
    team <- team[order(team$batting_order, decreasing = F),]
    
    team <- mutate(team, getHit = case_when(
      is.na(hRate) ~ (HRs+Singles+Doubles)/abs_rate,
      T ~ ((HRs+Singles+Doubles)/abs_rate+ hRate)/2
    ),
    getWalk = case_when(
      is.na(bbRate) ~ (Walks)/abs_rate,
      T ~ ((Walks)/abs_rate+ bbRate)/2
    ),
    getOut = 1-getHit-getWalk,
    scoreOnBase = Steals + Runs,
    build_proj =0,
    on_base = 0)
    team$getHit[is.na(team$getHit)] <- min(team$getHit, na.rm = T)
    team$getWalk[is.na(team$getWalk)] <- min(team$getWalk, na.rm = T)
    team$getOut[is.na(team$getOut)] <- min(team$getOut, na.rm = T)
    
    if(!is.na(team$OppP[1])){
      OppP <-team$OppP[1] 
      Pbuild_proj <- 0
      eAB <- team$eAB[1]
      pABs <- 0
      pConfidence <- runif(1)*.2+0.95
      skipP <- FALSE
    }  else {
      OppP <- NA
      Pbuild_proj <- 0
      skipP <- TRUE
    }
    hConfidence <- runif(1)*.3+.85
    ### need to add pitcher results, as well as logic for swapping to bullpen
    ### currently adding all points to first guy in lineup
    teamOuts <- 0
    thisBatter <- 1
    gameFlowMult <- runif(1)+.5
    while(teamOuts < 27){
      ### Reset outs and on base every inning
      inningOuts <- 0
      team$on_base <- 0
      
      ### Each loop is an at-bat
      while(inningOuts < 3){
        ab_result <- sample(x=c("hit", "walk", "out"), size = 1, prob = c(team$getHit[thisBatter]*pConfidence*hConfidence, team$getWalk[thisBatter]*pConfidence, team$getOut[thisBatter]/pConfidence/hConfidence), replace=T)
        if(ab_result=="hit"){
          hit_type <- sample(x=c(14, 3, 5), size = 1, prob= c(team$HRs[thisBatter], team$Singles[thisBatter], team$Doubles[thisBatter]))
          team$build_proj[thisBatter] <- team$build_proj[thisBatter] +hit_type
          atBatScores <- 0
          ### loop iterates through team on base for runs and RBIs
          for(i in 1:nrow(team)){
            
            # Does the player score who is on first?
            if(team$on_base[i] == 1){
              if(hit_type == 14){
                team$on_base[i] <- 0
                team$build_proj[i] <- team$build_proj[i]+3
                team$build_proj[thisBatter] <- team$build_proj[thisBatter]+3
                atBatScores <- atBatScores+1
                if(!skipP){Pbuild_proj <- Pbuild_proj - 2}
              }
              if(hit_type == 5){
                team$on_base[thisBatter] <- 2
                scored <- sample(x=c(0,3), size = 1, prob = c(.5, .5))
                team$on_base[i] <- scored
                if(scored==0){
                  team$build_proj[i] <- team$build_proj[i]+3
                  team$build_proj[thisBatter] <- team$build_proj[thisBatter]+3
                }
                atBatScores <- atBatScores+as.numeric(scored==0)
                if(!skipP){Pbuild_proj <- Pbuild_proj - 2*as.numeric(scored==0)}
              }
              if(hit_type == 3){
                team$on_base[thisBatter] <- 1
                scored <- sample(x=c(0,2,3), size = 1, prob = c(team$scoreOnBase[i]/2, team$scoreOnBase[i], team$scoreOnBase[i]/2))
                team$on_base[i] <- scored
                if(scored==0){
                  team$build_proj[i] <- team$build_proj[i]+3
                  team$build_proj[thisBatter] <- team$build_proj[thisBatter]+3
                }
                atBatScores <- atBatScores+as.numeric(scored==0)
                if(!skipP){Pbuild_proj <- Pbuild_proj - 2*as.numeric(scored==0)}
              }
            }
            # Player on second
            if(team$on_base[i] == 2){
              if(hit_type == 14){
                team$on_base[i] <- 0
                team$build_proj[i] <- team$build_proj[i]+3
                team$build_proj[thisBatter] <- team$build_proj[thisBatter]+3
                atBatScores <- atBatScores+1
                if(!skipP){Pbuild_proj <- Pbuild_proj - 2}
              }
              if(hit_type == 5){
                team$on_base[thisBatter] <- 2
                scored <- sample(x=c(0,3), size = 1, prob = c(team$scoreOnBase[i], .25))
                team$on_base[i] <- scored
                if(scored==0){
                  team$build_proj[i] <- team$build_proj[i]+3
                  team$build_proj[thisBatter] <- team$build_proj[thisBatter]+3
                }
                atBatScores <- atBatScores+as.numeric(scored==0)
                if(!skipP){Pbuild_proj <- Pbuild_proj - 2*as.numeric(scored==0)}
              }
              if(hit_type == 3){
                team$on_base[thisBatter] <- 1
                scored <- 0
                team$on_base[i] <- scored
                if(scored==0){
                  team$build_proj[i] <- team$build_proj[i]+3
                  team$build_proj[thisBatter] <- team$build_proj[thisBatter]+3
                }
                atBatScores <- atBatScores+as.numeric(scored==0)
                if(!skipP){Pbuild_proj <- Pbuild_proj - 2*as.numeric(scored==0)}
              }
            }
            # Player on third
            if(team$on_base[i] == 3){
              team$on_base[i] <- 0
              team$build_proj[i] <- team$build_proj[i]+3
              team$build_proj[thisBatter] <- team$build_proj[thisBatter]+3
              atBatScores <- atBatScores+1
              if(!skipP){Pbuild_proj <- Pbuild_proj - 2}
            }
            
            if(hit_type==14){
              if(!skipP){
                Pbuild_proj <- Pbuild_proj - 2.6
                pABs <- pABs + 3
              }
              pConfidence <- pConfidence + .07*gameFlowMult
              hConfidence <- hConfidence + .14*gameFlowMult
            }
            if(hit_type==5){
              if(!skipP){
                Pbuild_proj <- Pbuild_proj - 0.6
                pABs <- pABs + 1.5
              }
              pConfidence <- pConfidence + .05*gameFlowMult
              hConfidence <- hConfidence + .09*gameFlowMult
            }
            if(hit_type==3){
              if(!skipP){
                Pbuild_proj <- Pbuild_proj - 0.6
                pABs <- pABs + 1
              }
              pConfidence <- pConfidence + .02*gameFlowMult
              hConfidence <- hConfidence + .07*gameFlowMult
            }
            
          }
        }
        if(ab_result=="out"){
          
          if(inningOuts<2){
            if(1 %in% team$on_base){
              numOuts <- sample(x=c(1,2), size=1, prob=c(.55, .45))
            } else{numOuts <- 1}
          }else{
            numouts <- 1
          }
          inningOuts <- inningOuts+numOuts
          teamOuts <- teamOuts+numOuts
          if(!skipP){
            isK <- runif(1) < team$soRate[1]/(team$getOut[thisBatter])
            Pbuild_proj <- as.numeric(isK)*2 + .75 + Pbuild_proj
            pConfidence <- pConfidence - .05*gameFlowMult - as.numeric(isK)*.07*gameFlowMult
            pABs <- pABs + 0.75 - as.numeric(isK)*.4
          }
          hConfidence <- hConfidence - .07*gameFlowMult
        }
        if(ab_result=="walk"){
          if(1 %in% team$on_base){
            if(2 %in% team$on_base){
              if(3 %in% team$on_base){
                team$build_proj[which(team$on_base == 3)] <- team$build_proj[which(team$on_base == 3)]+3
                team$build_proj[thisBatter] <-team$build_proj[thisBatter]+3
                team$on_base[which(team$on_base == 3)] <- 0
              }
              team$on_base[which(team$on_base == 2)] <- 3
            }
            team$on_base[which(team$on_base == 1)] <- 2
          }
          team$on_base[thisBatter] <- 1
          if(!skipP){
            pABs <- pABs + 1.25
            Pbuild_proj <- Pbuild_proj -.6
            pConfidence <- pConfidence + .04*gameFlowMult
          }
          hConfidence <- hConfidence + .05*gameFlowMult
        }
        thisBatter <- thisBatter+1
        if(thisBatter > nrow(team)) thisBatter <- 1
        
        if(pABs > eAB || pConfidence > 1.8){
          skipP <- TRUE
          if(pConfidence > 1.1) {
            hConfidence <- min(hConfidence, 1.3)
            pConfidence <- 1.1
          }else pConfidence <- 1
          
        } 
        if(skipP){
          pConfidence <- 1.1
          hConfidence <- 1
        }else{
          pConfidence <- min(max(pConfidence, .6), 1.75)
          hConfidence <- min(max(hConfidence, .2), 1.9)
        }
      }
      if(!skipP){
        hConfidence <- min(max(hConfidence, .8), 1.2)
        pConfidence <- pConfidence * .9
      }
    }
    
    
    if(is.null(build)){
      build <- team 
      Pbuild <- data.frame(Name=OppP, build_proj = Pbuild_proj)
      
    }else {
      build <- rbind(build, team)
      Pbuild <- rbind(Pbuild, c(OppP, Pbuild_proj))
    }
    
  }
  build <- select(build, Name, build_proj)
  build <- rbind(build, Pbuild)
  
  return(build)
}

# x is a vector with length == number of lineups to build
f <- function(x,  projections, c_strength, b1_strength, b2_strength, b3_strength, ss_strength, of_strength, bans, locks, batter_lines, pitcher_lines, playerPool) {
  
  
  tryCatch(
    expr={
      #projections <- oprojections#[c(3,4,5,17, 18 ,19 ,21,28,29,30,32,36,37,38,39,41,42,43,44,46,48,52,53,57,60,61,64,66,67,77,78,82,83,86,90,92,97,98,99,102,103,105,108,110,111,112,113,114,116,117,120,121,123,124,125,126,129,130,131,132,135,137,141,144,145,150,152,154,156),]
      projections <- projections %>%
        group_by(Name) %>% 
        slice_tail(n=1) %>%
        ungroup()
      
      build_proj <- simGames(projections, batter_lines, pitcher_lines, batting_orders)
      
      batters <- filter(projections, !Position == "P", !Roster.Position =="P")
      
      build_pos <- c()
      for(b in 1:nrow(batters)){
        build_pos[b] <- sample(c("C", "1B", "2B", "3B", "SS", "OF"), 1,  prob = c(
          case_when(grepl("C", batters$Position[b], fixed = T)~ batters$DKfpts[b]/c_strength, T ~ 0),
          case_when(grepl("1B", batters$Position[b], fixed = T)~ batters$DKfpts[b]/b1_strength, T ~ 0), 
          case_when(grepl("2B", batters$Position[b], fixed = T)~ batters$DKfpts[b]/b2_strength, T ~ 0),
          case_when(grepl("3B", batters$Position[b], fixed = T)~ batters$DKfpts[b]/b3_strength, T ~ 0), 
          case_when(grepl("SS", batters$Position[b], fixed = T)~ batters$DKfpts[b]/ss_strength, T ~ 0),
          case_when(grepl("OF", batters$Position[b], fixed = T)~ batters$DKfpts[b]/of_strength, T ~ 0)))
      }
      batters$Position <- build_pos
      
      #batters <- merge(batters, build_proj, all.x = T)# %>%
      # mutate(build_proj = case_when(is.na(build_proj) ~ 0, T ~ build_proj))
      # 
      # b1s <- batters %>%
      #   filter(Position=="1B")%>%
      #   group_by(Team)%>%
      #   slice_max(order_by = build_proj, n=1)%>%
      #   ungroup()
      # b2s <- batters %>%
      #   filter(Position=="2B")%>%
      #   group_by(Team)%>%
      #   slice_max(order_by = build_proj, n=1)%>%
      #   ungroup()
      # b3s <- batters %>%
      #   filter(Position=="3B")%>%
      #   group_by(Team)%>%
      #   slice_max(order_by = build_proj, n=1)%>%
      #   ungroup()
      # bSSs <- batters %>%
      #   filter(Position=="SS")%>%
      #   group_by(Team)%>%
      #   slice_max(order_by = build_proj, n=1)%>%
      #   ungroup()
      # bCs <- batters %>%
      #   filter(Position=="C")%>%
      #   group_by(Team)%>%
      #   slice_max(order_by = build_proj, n=1)%>%
      #   ungroup()
      # bOFs <- batters %>%
      #   filter(Position=="OF")%>%
      #   group_by(Team)%>%
      #   slice_max(order_by = build_proj, n=3)%>%
      #   ungroup()
      # 
      # batters <- rbind(b1s, b2s, b3s, bSSs, bCs, bOFs)
      
      hold_pos <- select(batters, Name, ID, Position)
      
      pitchers <- filter(projections, Position == "P")
      
      teams <- projections %>%
        select(Team, Opp) %>%
        unique()
      
      
      batters <- mutate(batters, n1 = grepl("1B", Position, fixed = TRUE),
                        n2 = grepl("2B", Position, fixed = TRUE),
                        n3 = grepl("3B", Position, fixed = TRUE),
                        nSS = grepl("SS", Position, fixed = TRUE),
                        nC = grepl("C", Position, fixed = TRUE),
                        nOF = grepl("OF", Position, fixed = TRUE), 
                        nP = 0)%>%
        select(Name, Salary, Team, Opp, n1, n2, n3, nSS, nC, nOF, nP)
      
      pitchers <- mutate(pitchers, stack_size = 1,n1 = 0,
                         n2 = 0,
                         n3 = 0,
                         nSS = 0,
                         nC = 0,
                         nOF = 0,
                         nP = 1) %>%
        select(Name, Salary, Team, Opp, n1, n2, n3, nSS, nC, nOF, nP)
      
      all_considerations <- rbind(pitchers, batters) %>%
        merge(build_proj, all.y = T)# %>%
      # filter(Name %in% playerPool)
      all_considerations <- all_considerations[complete.cases(all_considerations),]
      #cat("\nall_considerations built")
      all_considerations$build_proj <- as.numeric(all_considerations$build_proj)
      
      
      bestTeams <- all_considerations %>%
        filter(nP ==0)%>%
        group_by(Team) %>%
        summarise(teamScore = sum(build_proj))%>%
        slice_max(order_by=teamScore, n=2)
      #cat("\nbestTeams build")
      vplayers <-  rep(1, nrow(all_considerations))
      vpool <- as.numeric(all_considerations$Name %in% playerPool)
      v1b <- all_considerations$n1
      v2b <- all_considerations$n2
      v3b <- all_considerations$n3
      vss <- all_considerations$nSS
      vc <- all_considerations$nC
      vOF <- all_considerations$nOF
      vP <- all_considerations$nP
      vsalary <- all_considerations$Salary
      vlocks <- as.numeric(all_considerations$Name %in% locks)
      vbans <- as.numeric(all_considerations$Name %in% bans)
      vstacks <- as.numeric(all_considerations$Team %in% bestTeams$Team & all_considerations$nP==0)
      vteams <- c()
      #cat(vstacks)
      for(t in 1:nrow(teams)){
        vteam <- as.numeric(all_considerations$Team==teams$Team[t] & all_considerations$nP == 0)
        vteams <- c(vteams, vteam)
      }
      
      matrix_vector <- c(vsalary, vsalary, vplayers, vpool, v1b, v2b, v3b, vss, vc, vOF, vP,  vlocks, vbans, vstacks, vteams) #vteams must be listed last
      
      matrix_vector[is.na(matrix_vector)] <- 0
      matrix_vector[is.infinite(matrix_vector)] <- 0
      
      const.mat = matrix(matrix_vector, nrow = length(matrix_vector)/nrow(all_considerations), byrow = TRUE)
      # 
      # ## Define constraints and direction - 50000 salary
      const.rhs = c(50000, min_salary,  10,  10,   1,   1,    1,    1,   1,   3,     2,  sum(vlocks),     0,    7)
      const.dir = c("<=" ,       ">=","==","==","==","==", "==", "==","==","==",  "==",          "==", "==", ">=")
      
      for(t in 1:nrow(teams)){
        const.dir <- c(const.dir, "<=")
        const.rhs <- c(const.rhs, 5)
      }
      
      
      # 
      # ### Optimize
      objective.in <- all_considerations$build_proj
      objective.in[is.nan(objective.in)] <- 0
      objective.in[is.na(objective.in)] <- 0
      #objective.in[objective.in < 0] <- 0
      optimum = lp(direction = "max", objective.in, const.mat, const.dir, const.rhs, all.bin = TRUE)
      # 
      all_considerations$optimal <- optimum$solution
      #cat("\noptimal lineup generated")
      lineup <- all_considerations %>%
        filter(optimal == 1)
      
      ids <- projections %>%
        filter(Name %in% lineup$Name) %>%
        select(Name, ID)
      
      lineup <- merge(lineup, ids) %>%
        merge(hold_pos, all.x = T)
      
      lineup$Position[is.na(lineup$Position)] <- "P"
      
      lineup <- lineup %>%
        mutate(order = case_when(
          grepl("P", Position, fixed = TRUE) ~ 0,
          grepl("C", Position, fixed = TRUE) ~ 1,
          grepl("1", Position, fixed = TRUE) ~ 2,
          grepl("2", Position, fixed = TRUE) ~ 3,
          grepl("3", Position, fixed = TRUE) ~ 4,
          grepl("SS", Position, fixed = TRUE) ~ 5,
          grepl("OF", Position, fixed = TRUE) ~ 6
          
        ))
      
      lineup <- lineup[order(lineup$order, decreasing = F),]
      
      export <- lineup$ID
      
      cat("\nlineup: ", lineup$Name)
      
      export
    }, error=function(e){  cat("e")
    })
}

processing <- function(build_lineups,  projections, c_strength, b1_strength, b2_strength, b3_strength, ss_strength, of_strength, bans, locks, batter_lines, pitcher_lines, playerPool){
  
  #View(oprojections)
  
  #print(c(build_lineups, min_salary, c_strength, b1_strength, b2_strength, b3_strength, ss_strength, of_strength, bans, locks), sep="\n")
  
  build <- lapply(1:build_lineups, f,  projections, c_strength, b1_strength, b2_strength, b3_strength, ss_strength, of_strength, bans, locks, batter_lines, pitcher_lines, playerPool)
  
  
  final_export <- NULL
  for(n in 1:length(build)){
    v <- build[[n]]
    #cat(v)
    while(!length(unique(v))==10){
      temp <- lapply(1, f, projections, c_strength, b1_strength, b2_strength, b3_strength, ss_strength, of_strength, bans, locks, batter_lines, pitcher_lines, playerPool)
      v <- temp[[1]]
    }
    
    if(is.null(final_export)) final_export <- v else{
      final_export <- as.data.frame(rbind(final_export, v))
    }
    
  }
  #final_export <- unique(final_export)
  colnames(final_export) <- c("P", "P", "C", "1B", "2B", "3B", "SS", "OF",  "OF", "OF")
  
  #assign('build', build, envir=.GlobalEnv)
  assign('final_export', final_export, envir=.GlobalEnv)
  
  #assign('final_export', final_export, envir=.GlobalEnv)
  #write.csv(final_export, "C:/Users/jorda/Downloads/DKLineupsMLB.csv", row.names = FALSE)
  
  ownership <- data.frame(Name = oprojections$Name, proj_own = oprojections$Own, ID = oprojections$ID, count = 0)
  
  for(c in 1:ncol(final_export)){
    for(r in 1:nrow(final_export)){
      this_id <- final_export[r,c]
      ownership$count[which(ownership$ID == this_id)] <- ownership$count[which(ownership$ID == this_id)]+1
    }
  }
  ownership$count <- ownership$count/build_lineups
  ownership <- rename(ownership, my_own = count)
  ownership$leverage <- ownership$my_own/ownership$proj_own
  ownership <- ownership[order(ownership$my_own, decreasing = T),]
  
  assign('ownership', ownership, envir=.GlobalEnv)
  
}

