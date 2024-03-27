

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

});

// Prepare the site for the selected DFS site
function prepareForSite(){
    fillPlayersTable();
    initializeLineupsTable();
    fillHighlightTable();
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

    for(let i = 0; i < sorted_data.length; i++){
        var row = table.insertRow(-1);
        var cell = row.insertCell(-1);
        cell.innerHTML = i + 1;
        cell = row.insertCell(-1);
        cell.innerHTML = sorted_data[i][0];
        cell = row.insertCell(-1);
        cell.innerHTML = sorted_data[i][1];
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

const delay = ms => new Promise(resolve => setTimeout(resolve, ms));

// Build lineups based on the constraints and the selected DFS site
async function buildLineups(){
    var site = document.getElementById("site-select").value.toLowerCase();
    var data = getInfoFromJSON("baseball_data.json");
    var constraints = getConstraints(site);
    var players = Object.keys(data);
    var number_of_lineups = document.getElementById("num-lineups").value;
    
    for(let i = 0, p = Promise.resolve(); i < number_of_lineups; i++){
        p = p.then(() => delay(10))
            .then(()=>buildOneLineup(site, constraints, players, data));
    }
}

// return constraints based on the selected DFS site
function getConstraints(site){
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
                "draftkings-salary":{"max":50000}
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
                "fanduel-salary":{"max":35000}
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
                "yahoo-salary":{"max":200}
            };
    }
}

// Convert players to variables for the solver
function convertPlayersToVariables(site, players, data){
    for(let player of players){
        var info = data[player];
        if(info[site + "-position"].includes("/")){
            var position = info[site + "-position"].split("/");
            var use_pos = position[Math.floor(Math.random() * position.length)];
        }else{
            var use_pos = info[site + "-position"];
        }
        if(site == "fanduel"){
            if(["C", "1B"].includes(use_pos)) use_pos = "C/1B";
        }
        data[player][site + "-fpts"] = randomizeProjection(info[site + "-fpts"], info["SD"]);
        data[player][use_pos] = 1;
        data[player][site + "-position"] = use_pos;
        data[player]["Players"] = 1;
    }
    return data;

}

// Randomize the projection based on the standard deviation
function randomizeProjection(projection, sd){
    return Number((Number(projection) + (Math.random() * sd) + (Math.random() * sd) + (Math.random() * sd)-1.5 * sd).toFixed(1));
}

// Build one lineup based on the constraints and the players
function buildOneLineup(site, constraints, players, data){
    var variables = convertPlayersToVariables(site, players, data);
    var model = {
        "optimize": site + "-fpts",
        "opType": "max",
        "constraints": constraints,
        "variables": variables,
        "binaries": variables
    };
    require(['solver'], function(solver){
        var result = solver.Solve(model);
        var lineup = [];
        for(let player of players){
            if(result[player] > 0){
                lineup.push(player);
            }
        }
        addLineupToTable(lineup, variables);
    });

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

function addLineupToTable(result, players){
    var table = document.getElementById("lineups-table");
    var row = table.insertRow(-1);
    var lineupPlayers = [];
    for(let p of result){
        if(players[p] != undefined) lineupPlayers.push(players[p]);
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
        //buildOneLineup(site, getConstraints(site), Object.keys(players), players);
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
        for(let c of l.cells){
            csv += c.innerHTML.split("<br>")[1]
            if(c.cellIndex < l.length -1) csv += ",";
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