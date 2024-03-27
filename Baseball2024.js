

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
});

// Prepare the site for the selected DFS site
function prepareForSite(){
    fillPlayersTable();
    initializeLineupsTable();
    fillHighlightTable();
    initializeFunctions();
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
    while(table.rows.length > 1){
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
    var content_divs = document.getElementsByClassName("content");
    for(let div of content_divs){
        if(div.id.includes(content)){
            div.style.display = "block";
        }else{
            div.style.display = "none";
        }
    }
}

// Initialize the lineups table
function initializeLineupsTable(){
    var table = document.getElementById("lineups-table");
    var site = document.getElementById("site-select").value.toLowerCase();
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
    var header = table.insertRow(-1);
    var headers = ["Rank", "Player", label];

    // Remove all rows from the table
    while(table.rows.length > 1){
        table.deleteRow(-1);
    }

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
        if(player == "Tarik Skubal") console.log(data[player]);
        sortable.push([player, Number(data[player][highlight])]);
    }
    sortable.sort(function(a, b){
        return b[1] - a[1];
    });

    return sortable;
}