

function getInfoFromJSON(file){
    var json = {};
    $.ajax({
        'async': false,
        'global': false,
        'url': file,
        'dataType': "json",
        'success': function (data) {
            json = data;
        }
    });
    return json;
}

$(document).ready(function(){
    prepareForSite();
    initializeFunctions();
    colorBySite("site-select");

});

// Prepare the site for the selected DFS site
function prepareForSite(){
    fillPlayersTable();
    initializeLineupsTable();
    fillHighlightTable();
    fillOmitTeamsList();
    document.getElementById("previous-lineups").value = "";
    colorBySite("site-select");

}

// Color the site based on the selected DFS site
function colorBySite(id){
    var el = document.getElementById(id);
    var site = document.getElementById("site-select").value.toLowerCase();
    var colors = {
        "draftkings":{
            "background-color": "#004400",
            "color": "#AAFFAA"
        } ,
        "fanduel": {
            "background-color": "#000044",
            "color": "#AABBFF"
        },
        "yahoo": {
            "background-color": "#440044",
            "color": "#FFAAFF"
        }
    };
    el.style.backgroundColor = colors[site]["background-color"];
    el.style.color = colors[site]["color"];
}

// Move team from one list to the other
// function moveTeam(el){
//     var team_selects = document.getElementsByClassName("team-select");
//     for(let select of team_selects){
//         if(select.id != el.id){
//             var option = document.createElement("option");
//             option.text = el.options[el.selectedIndex].text;
//             option.value = el.value;
//             select.appendChild(option);
//         }
//     }
//     el.remove(el.selectedIndex);
//     el.value = "Choose...";
// }

// Fill the omit teams list based on the teams in the JSON file
function fillOmitTeamsList(){
    var data = getInfoFromJSON("baseball_data.json");
    var teams = [];
    var list = document.getElementById("in-play");
    var list2 = document.getElementById("omitted");
    var list3 = document.getElementById("force-stacks");
    // clear the lists
    while(list.options.length > 0){
        list.remove(0);
    }
    while(list2.options.length > 0){
        list2.remove(0);
    }
    while(list3.options.length > 0){
        list3.remove(0);
    }
    
    for(let player of Object.keys(data)){
        if(!teams.includes(data[player]["Team"])) teams.push(data[player]["Team"]);
    }
    teams.sort();
    for(let team of teams){
        var option = document.createElement("option");
        option.text = team;
        option.value = team;
        list.appendChild(option);
    }
}

// Fill the players table with the players from the JSON file
// Only include data related to the selected DFS site
function fillPlayersTable(){
    clearPlayersTable();
    var table = document.getElementById("players-table");
    var data = getInfoFromJSON("baseball_data.json");
    var site = document.getElementById("site-select").value.toLowerCase();
    var has_th = false;
    for(let player of Object.keys(data)){
        if(!has_th){
            var header = table.insertRow(-1);
            for(let stat of Object.keys(data[player])){
                if(site == "draftkings" && (isSubstring("fanduel", stat) || isSubstring("yahoo", stat))) continue;
                if(site == "fanduel" && (isSubstring("draftkings", stat) || isSubstring("yahoo", stat))) continue;
                if(site == "yahoo" && (isSubstring("draftkings", stat) || isSubstring("fanduel", stat))) continue;
                
                var cell = header.insertCell(-1);
                cell.outerHTML = "<th>" + stat + "</th>";
            }
            has_th = true;
        }else{
            var header = table.rows[0];
        }
        var row = table.insertRow(-1);

        for(let i = 0; i< header.cells.length; i++){
            var stat = header.cells[i].textContent;
            var cell = row.insertCell(-1);
            var this_data = data[player][stat];
            if(Number(this_data) && !(isSubstring("id", stat) || isSubstring("salary", stat) )) {
                this_data = Number(this_data).toFixed(1);
            }
            cell.innerHTML = this_data;
        }
    }

}

// Clear the players table
function clearPlayersTable(){
    var table = document.getElementById("players-table");
    while(table.rows.length > 0){
        table.deleteRow(-1);
    }
}

// Check if a string is a substring
function isSubstring(substring, string){
    return string.toLowerCase().includes(substring.toLowerCase());
}

// add event listeners to relevent elements
function initializeFunctions(){
    var site_select = document.getElementById("site-select");
    site_select.addEventListener("change", fillPlayersTable);

    var nav_buttons = document.getElementsByClassName("nav-button");
    for(let button of nav_buttons){
        button.addEventListener("click", function(){
            toggleContent(button.innerHTML.toLowerCase());
        });
    }
}


// Toggle the content of the page
function toggleContent(content){
    var content_divs = document.getElementById("content").children;
    var nav_buttons = document.getElementsByClassName("nav-button");
    for(let div of content_divs){
        if(div.id.includes(content)){
            div.className = "show";
        }else{
            div.className = "hide";
        }
    }
    for(let button of nav_buttons){
        if(button.innerHTML.toLowerCase() == content){
            button.className = "nav-button selected";
        }else{
            button.className = "nav-button";
        }
    }
}

// Initialize the lineups table
function initializeLineupsTable(){
    var table = document.getElementById("lineups-table");
    var site = document.getElementById("site-select").value.toLowerCase();

    while(table.rows.length > 0){
        table.deleteRow(-1);
    }
    switch(site){
        case "draftkings":
            var headers = ["P", "P", "C", "1B", "2B", "3B", "SS", "OF", "OF", "OF"];
            break;
        case "fanduel":
            var headers = ["P", "C/1B", "2B", "3B", "SS", "OF", "OF", "OF", "UTIL"];
            break;
        case "yahoo":
            var headers = ["P", "P", "C", "1B", "2B", "3B", "SS", "OF", "OF", "OF"];
            break;
    }
    
    var header = table.insertRow(-1);
    for(let h of headers){
        var cell = header.insertCell(-1);
        cell.outerHTML = "<th>" + h + "</th>";
    }

}

// Fill the highlight table
function fillHighlightTable(){
    var table = document.getElementById("highlight-table");
    var data = getInfoFromJSON("baseball_data.json");
    var site = document.getElementById("site-select").value.toLowerCase();
    var highlight = document.getElementById("highlight-select").value;
    var label = document.getElementById("highlight-select").options[document.getElementById("highlight-select").selectedIndex].text;
    

    // Remove all rows from the table
    while(table.rows.length > 0){
        table.deleteRow(-1);
    }

    var header = table.insertRow(-1);
    var headers = ["Rank", "Player", label];

    for(let h of headers){
        var cell = header.insertCell(-1);
        cell.outerHTML = "<th>" + h + "</th>";
    }

    var data_to_show = site + "-" + highlight;
    var sorted_data = sortData(data, data_to_show);
    switch(highlight){
        case "fpts":
            var decimals = 1;
            var mult = 1;
            break;
        case "oth":
            var decimals = 0;
            var mult = 100;
            break;
        case "jvalue":
            var decimals = 2;
            var mult = 1;
            break;
    }

    for(let i = 0; i < sorted_data.length; i++){
        var row = table.insertRow(-1);
        var cell = row.insertCell(-1);
        cell.innerHTML = i + 1;
        cell = row.insertCell(-1);
        cell.innerHTML = sorted_data[i][0];
        cell = row.insertCell(-1);
        cell.innerHTML = (mult * sorted_data[i][1]).toFixed(decimals);
        if(highlight == "oth")  cell.innerHTML += "%";
    }
}

// Sort the data based on the selected highlight
function sortData(data, highlight){
    var sortable = [];
    for(let player of Object.keys(data)){
        sortable.push([player, Number(data[player][highlight])]);
    }
    sortable.sort(function(a, b){
        return b[1] - a[1];
    });

    return sortable;
}

// Grab the top # players from the data
function filterForTopPlays(data){
    var site = document.getElementById("site-select").value.toLowerCase();
    var top_plays = {};
    var num_top_plays = document.getElementById("num-top-plays").value;
    // filter for the top plays based on jvalue for selected site
    var criteria = site + "-jvalue";
    var sorted_data = sortData(data, criteria);
    for(let i = 0; i < num_top_plays; i++){
        top_plays[sorted_data[i][0]] = data[sorted_data[i][0]];
    }
    return top_plays;
}

// sort the data based on the selected highlight
function sortData(data, highlight){
    var omitted_teams = document.getElementById("omitted").options;
    for(let team of omitted_teams){
        if(team.value != "Choose..."){
            for(let player of Object.keys(data)){
                if(data[player]["Team"] == team.value){
                    delete data[player];
                }
            }
        }
    }
    var sortable = [];
    for(let player of Object.keys(data)){
        sortable.push([player, Number(data[player][highlight])]);
    }
    sortable.sort(function(a, b){
        return b[1] - a[1];
    });

    return sortable;
}

const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

// Build lineups based on the constraints and the selected DFS site
async function buildLineups(){
    var site = document.getElementById("site-select").value.toLowerCase();
    var data = filterForTopPlays(getInfoFromJSON("baseball_data.json"));
    var constraints = getConstraints(site);
    
    var players = Object.keys(data);
    var number_of_lineups = document.getElementById("num-lineups").value;
    var lineups_built = document.getElementById("lineups-built").innerHTML;
    
    for(let i = 0, p = Promise.resolve(); i < number_of_lineups; i++){
        p = p.then(() => delay(Math.random() * 1500))
            .then(()=> {return buildOneLineup(site, constraints, players, data)});
    }
}

// return constraints based on the selected DFS site
function getConstraints(site){
    var min_salary = document.getElementById("min-salary").value;
    switch(site){
        case "draftkings":
            return {
                "P":{"equal":2},
                "C":{"min":1},
                "1B":{"min":1},
                "2B":{"min":1},
                "3B":{"min":1},
                "SS":{"min":1},
                "OF":{"min":3},
                "Players":{"equal":10},
                "draftkings-salary":{"max":50000, "min":min_salary*50000/100}
            };
        case "fanduel":
            return {
                "P":{"equal":1},
                "C/1B":{"min":1},
                "2B":{"min":1},
                "3B":{"min":1},
                "SS":{"min":1},
                "OF":{"min":3},
                "Players":{"equal":9},
                "fanduel-salary":{"max":35000, "min":min_salary*35000/100}
            };
        case "yahoo":
            return {
                "P":{"equal":2},
                "C":{"min":1},
                "1B":{"min":1},
                "2B":{"min":1},
                "3B":{"min":1},
                "SS":{"min":1},
                "OF":{"min":3},
                "Players":{"equal":10},
                "yahoo-salary":{"max":200, "min":min_salary*200/100}
            };
    }
}

// Convert players to variables for the solver
async function convertPlayersToVariables(site, players, data){
    let promise = new Promise((resolve, reject) => {
        var teams = [];
        var team_effects = [];
        for(let player of players){
            var info = data[player];

            if(!teams.includes(info["Team"])) {
                teams.push(info["Team"]);
                team_effects.push(Math.random()*2);
            }
            if(info[site + "-position"] == "P") var this_sd = Number(info["stdev"]); else var this_sd = Number(info["stdev"]) * team_effects[teams.indexOf(info["Team"])];

            if(info[site + "-position"].includes("/")){
                var position = info[site + "-position"].split("/");
                var use_pos = position[Math.floor(Math.random() * position.length)];
            }else{
                var use_pos = info[site + "-position"];
            }
            if(site == "fanduel"){
                if(["C", "1B"].includes(use_pos)) use_pos = "C/1B";
            }
            data[player]["build-fpts"] = randomizeProjection(info[site + "-fpts"], this_sd);
            data[player][use_pos] = 1;
            data[player][site + "-position"] = use_pos;
            data[player]["Players"] = 1;
            if(info[site+"-position"] == "P"){ 
                data[player][info["Opp"]] = document.getElementById("allow-bvp-yes").checked ? 1 : 3;
                data[player][info["Team"]] = 1;
            }else{
                data[player][info["Team"]] = 2;
            }
        }
        resolve([data, teams]);
    }).then((result) => {
        var data = result[0];
        var teams = result[1];
        var stack_type = document.getElementById("stack-type").value.split("-");
        data = convertVariablesToStacks(site, data, teams, stack_type);
        return [data, teams];
    });

    return promise;
}

// Convert the variables to stacks for the solver
function convertVariablesToStacks(site, data, teams, stack_type){
    var stacks = {};
    var force_stacks = document.getElementById("force-stacks");
    var force_stacks_list = [];
    for(let stack of force_stacks.options){
        force_stacks_list.push(stack.value);
    }
    for(let team of teams){
        var team_players = [];

        for(let player of Object.keys(data)){
            if("P" in data[player] || data[player]["Team"] != team){
                continue;
            }else{
                team_players.push(data[player]);
            }       
        }
        // if the team is not in the force stacks list and the force stacks list has length > 0, skip the team
        for(let type of stack_type){
            // get the top 'type' players from the team
            var num = Number(type);
            if(num == 1 || num > team_players.length) continue;
            var this_stack = {
                "Players": num,
                "draftkings-salary": 0,
                "fanduel-salary": 0,
                "yahoo-salary": 0,
                "draftkings-fpts": 0,
                "fanduel-fpts": 0,
                "yahoo-fpts": 0,
                "draftkings-jvalue": 0,
                "fanduel-jvalue": 0,
                "yahoo-jvalue": 0,
                "build-fpts": 0,
                "C": 0,
                "1B": 0,
                "2B": 0,
                "3B": 0,
                "SS": 0,
                "OF": 0,
                "P": 0,
                "C/1B": 0,
                "stack_players" : []

            };
            if(force_stacks_list.length > 0 && force_stacks_list.includes(team)) this_stack["force-stack"] = 1;

            this_stack[team] = 2;
            this_stack[type+ "-stack"] = 1;
            // only use the top num players based on "build-fpts"
            team_players.sort(function(a, b){
                return b["build-fpts"] - a["build-fpts"];
            });
            for(let i = 0; i < num; i++){
                this_stack["stack_players"].push(team_players[i]);//[site + "-id"]);
                for(let info in this_stack){
                    if(info == "Players" || info == team) continue;

                    if(Number(team_players[i][info])) this_stack[info] += Number(team_players[i][info]);
                    
                }
            }

            stacks[team + "-" + type] = this_stack;
        }
    }

    return {...stacks, ...data};
}

// Randomize the projection based on the standard deviation
function randomizeProjection(projection, sd){
    var variance = document.getElementById("variance").value;
    sd = Number(sd) * (1 + (variance-50)/100);
    var new_proj = randNormal(Number(projection), sd);
    return new_proj.toFixed(1);
}

function randNormal(mean, stdDev){
    var u1 = Math.random();
    var u2 = Math.random();
    var randStdNormal = Math.sqrt(-2.0 * Math.log(u1)) * Math.sin(2.0 * Math.PI * u2);
    return mean + stdDev * randStdNormal;
}

// Build one lineup based on the constraints and the players
async function buildOneLineup(site, constraints, players, data){
    var converted = await convertPlayersToVariables(site, players, data);
    var variables = converted[0];
    var teams = converted[1];
    var stack_type = document.getElementById("stack-type").value.split("-");
    var force_stacks = document.getElementById("force-stacks");
    if(force_stacks.options.length > 0){
        if(force_stacks.options.length < 4){
            constraints["force-stack"] = {"min": 1};
        }else{
            constraints["force-stack"] = {"min": 2};
        }
    }
    for(let type of stack_type){
        if(type != "1"){ 
            if(!(type + "-stack" in constraints)){
                constraints[type + "-stack"] = {"equal":1};
            }else{
                constraints[type + "-stack"]["equal"] += 1;
            }
        }
    }
    
    for(let t of teams){
        constraints[t] = {"max":3};
    }
    var model = {
        "optimize": "build-fpts",
        "opType": "max",
        "constraints": constraints,
        "variables": variables,
        "binaries": variables
        
    };

    require(['solver'], function(solver){
        console.log(model);
        var result = solver.Solve(model);
        if(result.feasible == false){
            console.log("No solution found");
            buildOneLineup(site, getConstraints(site), Object.keys(data), data);
            return;
        }
        result = retrieveStacks(result, variables);
        if(result == "fail"){
            buildOneLineup(site, getConstraints(site), Object.keys(data), data);
            return;
        }        
        var lineup = [];
        for(let player of players){
            if(player in result){
                lineup.push(player);
            }
        }
        addLineupToTable(lineup, data);
    });

}

// Retrieve the stacks from the result
function retrieveStacks(result, variables){
    for(let key in result){
        if(key.includes("-")){
            var stack = variables[key]["stack_players"];
            for(let player of stack){
                result[player["Unique"]] = 1;
            }
        }
    }
    return result;
}

// Use headers to inform order of players
function getLineupsTableHeaders(){
    var headers = [];
    var table = document.getElementById("lineups-table");
    for(let h of table.rows[0].cells){
        headers.push(h.textContent);
    }
    return headers;
}

function checkOrder(site, lineup, order){
    for(let i = 0; i < order.length; i++){
        if(site == "fanduel" && i == order.length - 1) continue;
        if(!lineup[i][site + "-position"].includes(order[i])) return false;
    }
    return true;
}

// Randomize the order of an array
function shuffle(site, array) {
    let currentIndex = array.length,  randomIndex;

    // While there remain elements to shuffle.
    while (currentIndex > 0) {

        // Pick a remaining element.
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex--;

        // And swap it with the current element.
        [array[currentIndex], array[randomIndex]] = [
        array[randomIndex], array[currentIndex]];
    }

    // Move pitchers to the front of the lineup, OF to the back, and force positions for single position players
    for(let i = 0; i < array.length; i++){
        if(array[i][site + "-position"].includes("P")){
            if(array[0][site + "-position"].includes("P") && site != "fanduel"){
                let temp = array[1];
                array[1] = array[i];
                array[i] = temp;
            }else{
                let temp = array[0];
                array[0] = array[i];
                array[i] = temp;
            }
        }else if(array[i][site + "-position"].includes("OF") && !(array[i][site + "-position"].includes("/"))){
            // if OF is not in the last 3 positions, move it to the back
            // except on fanduel, where it should be one from the back
            if(site == "fanduel" && i < array.length - 5){
                let temp = array[i];
                array = array.slice(0, i).concat(array.slice(i+1));
                array.splice(array.length - 2, 0, temp);

            }else if(!(site=="fanduel") && i < array.length - 3){
                let temp = array[i];
                array = array.slice(0, i).concat(array.slice(i+1));
                array.push(temp);
            }

        }else{
            if(!(array[i][site + "-position"].includes("/"))){
                let force_position = forcePosition(site, array[i][site + "-position"]);
                let temp = array[force_position];
                array[force_position] = array[i];
                array[i] = temp;
            }
        }
    }
    var list = [];
    for(let p of array){
        list.push(p[site + "-position"]);
    }

    return array;
}

// Force a player to a specific position
function forcePosition(site, position){
    switch(site){
        case "draftkings":
            switch(position){
                case "C":
                    return 2;
                case "1B":
                    return 3;
                case "2B":
                    return 4;
                case "3B":
                    return 5;
                case "SS":
                    return 6;
            }
            break;
        case "fanduel":
            switch(position){
                case "C":
                    return 1;
                case "1B":
                    return 1;
                case "2B":
                    return 2;
                case "3B":
                    return 3;
                case "SS":
                    return 4;

            }
            break;
        case "yahoo":
            switch(position){
                case "C":
                    return 2;
                case "1B":
                    return 3;
                case "2B":
                    return 4;
                case "3B":
                    return 5;
                case "SS":
                    return 6;
            }
            break;
    }
}

// Remove built lineups from the table
function clearLineups(){
    var table = document.getElementById("lineups-table");
    while(table.rows.length > 1){
        table.deleteRow(-1);
    }
    document.getElementById('lineups-built').innerHTML = 0;
}

function addLineupToTable(result, data){
    var table = document.getElementById("lineups-table");
    var row = table.insertRow(-1);
    var lineupPlayers = [];
    for(let p of result){
        if(data[p] != undefined) lineupPlayers.push(data[p]);
    }

    // randomize lineupPlayers order and finalize when order matches lineups-table headers
    var headers = getLineupsTableHeaders();
    var orderIsCorrect = false;
    var beginLoop = Date.now();
    var site = document.getElementById("site-select").value.toLowerCase();
    while(!orderIsCorrect){
        orderIsCorrect = checkOrder(site, lineupPlayers, headers);
        if(!orderIsCorrect) lineupPlayers = shuffle(site, lineupPlayers);
        if(Date.now() - beginLoop > 300) break;
    }
    if(!orderIsCorrect){
        table.deleteRow(row.rowIndex);
        console.log("Could not find valid lineup");
        buildOneLineup(site, getConstraints(site), Object.keys(data), data);
        return;
    }
    for(let p of lineupPlayers){
        let c = row.insertCell(-1)
        c.innerHTML = p[site+"-name"] + "<br>" + p[site+"-id"] + "<br>" + p[site+"-salary"] + "<br>" + p.Team;
        //c.style.backgroundColor = getTeamColor(p.Team);
        //c.style.color = getTeamSecondaryColor(p.Team);
    }

    document.getElementById('lineups-built').innerHTML = Number(document.getElementById('lineups-table').rows.length) - 1;
}

function downloadLineups(){
    var lineups = document.getElementById("lineups-table").rows;
    var csv = "data:text/csv;charset=utf-8,";
    for(let l of lineups){
        if(l.rowIndex == 0) {
            for(let c of l.cells){
                csv += c.innerHTML + ",";
            }
        }else{
            for(let c of l.cells){
                csv += c.innerHTML.split("<br>")[1] + ",";
                //if(c.cellIndex < l.length -1) csv += ",";
            }
        }
        csv += "\n";    
    }
    var encodedUri = encodeURI(csv);
    var link = document.createElement("a");
    link.setAttribute("href", encodedUri);
    link.setAttribute("download", "lineups.csv");
    document.body.appendChild(link);
    link.click();
    
}

function downloadEditedLineups(){
    var lineups = document.getElementById("lineups-table").rows;
    var csv = "data:text/csv;charset=utf-8,";
    var previousLineups =JSON.parse(sessionStorage.previous_entries);
    var site = document.getElementById("site-select").value.toLowerCase();
  
    for(let l of lineups){
        if(l.rowIndex == 0) continue;
        var row = [];
        for(let c of l.cells){
            let id = c.innerHTML.split("<br>")[1];
            row.push(id);
        }

        var index = l.rowIndex;
        if(index > previousLineups.length) index = previousLineups.length;
        for(let i = 0; i < row.length; i++){
            if(site == "yahoo") previousLineups[index][i+5] = row[i]; else if(site == "fanduel") previousLineups[index][i+3] = row[i]; else  previousLineups[index][i+4] = row[i];
        }
    }
    if(site == "yahoo"){
        for(let j = 0; j < lineups.length; j++){
            let l = previousLineups[j];
            for(let i = 0; i < 13; i++){
                csv += l[i] + ",";
            }
            csv += "\n";
        }
    }else{
        for(let l of previousLineups){
            csv += l.join(",") + "\n";
        }
    }
    //csv += previousLineups.join("\n");
    var encodedUri = encodeURI(csv);

    var link = document.createElement("a");
    link.setAttribute("href", encodedUri);
    link.setAttribute("download", "lineups.csv");
    document.body.appendChild(link);
    link.click();
}

function handleLineupscsv(){
    var csv = document.getElementById("previous-lineups").files[0];
    var reader = new FileReader();
    // save csv to storage as JSON
    reader.onload = function(e){
        var csv = e.target.result;
        var lines = csv.split("\n");
        var result = [];
        var site = document.getElementById("site-select").value.toLowerCase();
        var headers = lines[0].split(",");
        for(let i = 0; i < lines.length; i++){
            var obj = [];
            var currentline = lines[i].split(",");
            if(site == "yahoo"){
                currentline = cutOffYahooEntries(currentline);
            }
            if(currentline.length > headers.length && site == "yahoo" ) {
                // combine the first and second items in array into one, with a comma between them
                currentline[0] = currentline[0]+ currentline[1];
                currentline.splice(1, 1);
            }
            for(let j = 0; j < headers.length; j++){
                obj[j] = currentline[j];
            }
            result.push(obj);
        }
        //localStorage.DKEntries = JSON.stringify(result);
        //return result;
        //return(JSON.stringify(result));
        //location.reload();

        sessionStorage.previous_entries =  JSON.stringify(result);
    }
    reader.readAsText(csv);

}

// cuts off yahoo entries at the end of the list of players since they like to add extra commas that ruin everything
function cutOffYahooEntries(arr){
    var new_arr = [];
    for(let i = 0; i < 13; i++){
        new_arr.push(arr[i]);
    }
    return new_arr;
}

// add ownership table to the page based on number of times player appears in my built lineups
function updateOwnership(){
    var lineups = document.getElementById("lineups-table").rows;
    var table = document.getElementById("ownership-table");
    var headers = ["Player", "Position", "Team", "Ownership"];
    var positions = getLineupsTableHeaders();
    while(table.rows.length > 0){
        table.deleteRow(-1);
    }
    var header = table.insertRow(-1);
    for(let h of headers){
        var cell = header.insertCell(-1);
        cell.outerHTML = "<th>" + h + "</th>";
    }
    
    
    var ownership = {};
    for(let l of lineups){
        if(l.rowIndex == 0) continue;
        for(let c of l.cells){
            let name = c.innerHTML.split("<br>")[0];
            let team = c.innerHTML.split("<br>")[3];
            let id = c.innerHTML.split("<br>")[1];
            if(!(id in ownership)){
                ownership[id] = {"name": name, "team": team, "count": 1, "position": positions[c.cellIndex]};
            }else{
                ownership[id]["count"]++;
            }
        }
    }
    var sortable = [];
    for(let key in ownership){
        sortable.push([key, ownership[key]]);
    }
    sortable.sort(function(a, b){
        return b[1]["count"] - a[1]["count"];
    });
    for(let i = 0; i < sortable.length; i++){
        var row = table.insertRow(-1);
        var cell = row.insertCell(-1);
        
        cell.innerHTML = sortable[i][1]["name"];
        cell = row.insertCell(-1);
        cell.innerHTML = sortable[i][1]["position"];
        cell = row.insertCell(-1);
        cell.innerHTML = sortable[i][1]["team"];
        cell = row.insertCell(-1);
        cell.innerHTML = (100*sortable[i][1]["count"]/(lineups.length-1)).toFixed(1) + "%";
    }
}

// move team from one list to the other
function moveTeam(from, to){
    var to_list = document.getElementById(to);
    var from_list = document.getElementById(from);
    var option = from_list.options[from_list.selectedIndex];
    var new_option = document.createElement("option");
    new_option.innerHTML = option.innerHTML;
    new_option.value = option.value;
    to_list.appendChild(new_option);
    from_list.remove(from_list.selectedIndex);
}