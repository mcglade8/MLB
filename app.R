#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
source(paste(getwd(),"/sourceCode/MLBFunctions.R", sep=""))
ui <- function(request){
  
  fluidPage(
    tags$head(
      tags$style(HTML("
      body, label,div{
        background-color: #141414;
        color: white;
      }
      .tab-pane{
        padding:1%;
      }
      table, td{
        border-radius: 5px;
        background-color: #2b3d33;
        color: white;
      }
      th{
        background: linear-gradient(white, #7b857f);
        color: black;
      }
      .btn, li{
        background: linear-gradient(#303030, black);
        color:white;
        font-weight: 700;
      }
      li:active{
        background: linear-gradient(#d4d4d4, #3d3d3d)
        color:white;
      }
      .row{
        margin-bottom: 10px;
      }
      .shiny-input-container {
        color: #474747;
      }
      #col1header{
        background: #2b3d33;
        width: fit-content;
        color: white;
        border: 3px blue solid;
        margin-left:50px;
        padding: 10px
        
      }
                      "))
    ),
    titlePanel("Lineup Builder"),
     tabsetPanel(
      tabPanel(
        "Projections",
        fluidRow(fileInput("dksalaries", "DK Salaries Import")),
        fluidRow(actionButton("getData", "Get Projections")),
        fluidRow(
        DT::dataTableOutput("projections")
        )
      ),
      tabPanel("Parameters",
               numericInput("build_lineups", "Number of Lineups",  20),
               selectInput("bans", "Bans", c(oprojections$Name, unique(oprojections$Team)), multiple=TRUE),
               selectInput("locks", "Locks", c(oprojections$Name, unique(oprojections$Team)), multiple=TRUE),
               sliderInput("min_salary", "Min Salary", value = 47000, min = 40000, max=50000),
               checkboxInput("limit_player_pool", "Limit player pool to selection?", value=FALSE)
               
               
      ),
      tabPanel("Team Summary",
               DT::dataTableOutput("teamSummary")
      ),
      tabPanel("Pitcher Summary",
               DT::dataTableOutput("pitcherSummary")
      ),
      tabPanel("My Player Pool",
               actionButton("save", "Save Player Pool"),
                         actionButton("clearPool", "Clear Pool"),
                         DT::dataTableOutput("playerPool")
      ),
      tabPanel("Lineups",
               fluidRow(column(4, tags$h1(id="col1header", "New Lineups")), column(4, fileInput("dkentries", "DK Entries (previous build)"))),
               fluidRow(column(4, actionButton("build", "New Build")),
                         column(4, actionButton("lineupEditor", "Edit DKEntries with Last Build"))),
               fluidRow(column(4, downloadButton("lineupDownload", "Download Lineups for Initial Upload")),
                        column(4, downloadButton("editDownload", "Download Updated DKEntries"))),
               fluidRow(
                 column(3, uiOutput("lineups1")),
                 column(3, uiOutput("lineups2")),
                 column(3, uiOutput("lineups3")),
                 column(3, uiOutput("lineups4"))
               )
      ),
      tabPanel("Ownership",
               DT::dataTableOutput("Ownership")
      ),
      tabPanel("Pitcher Lines",
               DT::dataTableOutput("pitcherLines")
      ),
      tabPanel("Batter Lines",
               DT::dataTableOutput("batterLines")
      ),
      tabPanel("Review Last Night",
               fileInput("contestData", "Contest Data"),
                 #textInput("contestID", "Contest ID #:", value="148253613"),
                 actionButton("compare", "Compare Build to Last Night"),
                 textOutput("comparison"),
                 DT::dataTableOutput("bestLineups")
               
      )
    ),
    tags$script(HTML("
    /*
    // hide table elements I don't need
    var projectionsTable = document.getElementsByTagName('table')[0];
    //var projectionsTable= document.getElementById('DataTables_Table_0');
    console.log(projectionsTable);
    var projectionsHeads = projectionsTable.firstElementChild.firstElementChild.getElementsByTagName('th');
    var projectionsRows = projectionsTable.getElementsByTagName('tr');
    
    var keep_c = ['Name', 'Team', 'Position']
    var trunc_c = ['DKfpts', 'SD', 'HRs', 'TopPct', 'Own']
    var kth = [];
    
    for(let thisTH of projectionsHeads){
      let al = thisTH.ariaLabel;
      let found = false;
      for(let kc of keep_c){
        if(al.search(kc)>-1){
          kth[kth.length] = 'k';
          found = true;
        }
      }
      if(!found) for(let tc of trunc_c){
        if(al.search(tc)>-1){
          kth[kth.length] = 't';
          found = true;
        }
      }
      if(!found) kth[kth.length] = 'h';
      
    }
    
    for(let row of projectionsRows){
    let tds = row.getElementsByTagName('td');
      for(let i = 0; i < kth.length;i++){
        if(kth[i]=='t'){
          tds[i].innerHTML = tds[i].innerHTML.substring(0,4);
        }
        if(kth[i]=='h'){
          tds[i].style.visibility = 'hidden';
        }
      }
    }
   */
   
    "))
  )
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  library(shiny)
  library(DT)
  library(shinyFeedback)
  library(dplyr)
  
  isBuilding <- FALSE
  getBuiltLineups() # assigns dfs blList1, blList2, blList3 and builtLineups
  getPlayerPool() # assigns df playerPoolSaved
  getts()
  getps()
  
  ##### define functions for server #####
  {
    
    
    
    addLineupsToTab <- function(c){ 
      
      thisList <- get(sub(" ", "",paste0("blList", c)))
      plot_output_list <- list()
      output[[sub(" ","",paste0("lineups",c))]] <- renderUI({
        for(i in 1:length(thisList)){
          tablename <- paste("tablename", c, i, sep = "")
          plot_output_list[[i]] <- tableOutput(tablename)
        }
        
        do.call(tagList, plot_output_list)
      })
      
      for (i in 1:length(thisList)) {
        local({
          tablename <- paste("tablename", c, i, sep = "")
          output[[tablename]] <- renderTable({
            thisList[[i]]
          })
        })
      }
      
    }
    
    renderTeamSummary <- function(output, teamSummary){
      #cat("teamsummary\n")
      output$teamSummary <- DT::renderDataTable(
        if(tsSaved[[1, 2]] == "cleared"){
          datatable( data = teamSummary(),editable = TRUE, filter="top")
        }else{
          datatable(data=teamSummary()
                    ,editable = TRUE
                    ,selection= list(mode='multiple', selected = c(1:nrow(teamSummary()))[teamSummary()$Team %in% tsSaved$Team], target = 'row', selectable = TRUE)
                    
          )
        }
      )
    }
    
    renderPitcherSummary <- function(output, pitcherSummary){
      #cat("pitchsummary\n")
      
      output$pitcherSummary <- DT::renderDataTable(
        if(psSaved[[1, 2]] == "cleared"){
          datatable( data = pitcherSummary(),editable = TRUE, filter="top")
        }else{
          datatable(data=pitcherSummary()
                    ,editable = TRUE
                    ,selection= list(mode='multiple', selected = c(1:nrow(pitcherSummary()))[pitcherSummary()$Name %in% psSaved$Name], target = 'row', selectable = TRUE)
                    
          )
        }
      )
    }
    
    renderProjections <- function(projections, teamSummary, pitcherSummary){
      #cat("hello")
      teamSelects <- c(1:nrow(projections()))[(projections()$Team %in% teamSummary()[input$teamSummary_rows_selected,]$Team) & !(projections()$Position == "P")]
      #cat('1')
      #omitPitchers <- c(1:nrow(projections()))[projections()$Position == "P"]
      #cat('2')
      #teamSelects <- setdiff(teamSelects, omitPitchers)
      #cat('3')
      pitcherSelects <- c(1:nrow(projections()))[projections()$Name %in% pitcherSummary()[input$pitcherSummary_rows_selected,]$Name]
      #cat('4')
      dtSelect <- unique(c(teamSelects,pitcherSelects))
      #cat('5')
      if(length(dtSelect)==0) {
        dtSelect <- NULL
      }
      #cat('6')
      if(playerPoolSaved[[1, 2]] == "cleared"){
        #cat('cleared')
        thisDT <- datatable( data = projections(),editable = TRUE, filter="top"
                             , selection=list(mode='multiple',selected = dtSelect, target='row', selectable=TRUE))
      }else{ #cat('else')
        thisDT <- datatable( data = projections(),
                             editable = TRUE
                             , filter="top"
                             ,selection= list(mode='multiple'
                                              , selected = unique(
                                                c(c(1:nrow(projections()))[projections()$ID %in% playerPoolSaved$ID], dtSelect)
                                              )
                                              , target = 'row', selectable = TRUE)
        )
      }#)
      return(thisDT)
    }
    
    renderOwnership <- function(output){
      output$Ownership <- DT::renderDT(datatable(data=ownership))
    }
    
    renderBL <- function(output){
      output$batterLines <- DT::renderDT(datatable(data=batter_lines, editable=TRUE))
    }
    
    renderPL <- function(output){
      output$pitcherLines <- DT::renderDT(datatable(data=pitcher_lines, editable=TRUE))
    }
    
    renderPlayerPool <- function(output, playerPool){
      output$playerPool <- DT::renderDataTable(
        playerPool()
      )
    }
    
    comparison <- function(output){
      cd <- read.csv(input$contestData$datapath)
      req(cd)
      ext <- tools::file_ext(cd$datapath)
      
      #shiny::validate(need(ext=="csv", "Upload contest data from DK first"))
      
      contestData <- getContestData(cd)
      #View(contestData)
      cost <- contestData[[1]]
      revenue <- contestData[[2]]
      roi <- contestData[[3]]
      best_lineups <- contestData[[4]]
      output$comparison <- renderText(paste("cost: ",cost,"\nrevenue: ", revenue, "\nroi: ", roi, sep=""))
      output$bestLineups <- DT::renderDT(datatable(data=best_lineups))
    }
    
    provideFeedback <- function(msg, id = NULL) {
      showNotification(msg, id = id, duration = NULL, closeButton = FALSE)
    }
    
  }
  output$projections <-  DT::renderDT(renderProjections(projections, teamSummary, pitcherSummary))
  
  # Reactive Expression caches this result for use throughout server function; more computationally efficient and easier to debug
  projections <- reactive({oprojections})
  teamSummary <- reactive({team_summary}) 
  pitcherSummary <- reactive({pitcher_summary})
  Ownership <- reactive({ownership})
  playerPool <- reactive({unique(as.data.frame(rbind(projections()[input$projections_rows_selected,])))})
  pitcherLines <- reactive({pitcher_lines})
  batterLines <- reactive({batter_lines})
  contestID <- reactive({})
  textOutput <- reactive({})
  bestLineups <- reactive({best_lineups})
  # blocks <- reactive({input$locks})
  # bbans <- reactive({input$bans})
  # bminSal <- reactive({input$min_salary})
  # bnumLineups <- reactive({input$build_lineups})
  
  ##### Render tabs to start #####
  
  output$projections <-  DT::renderDT(renderProjections(projections, teamSummary, pitcherSummary))
  
  renderOwnership(output)
  renderPlayerPool(output, playerPool)
  renderTeamSummary(output, teamSummary)
  renderPitcherSummary(output, pitcherSummary)
  
  renderBL(output)
  renderPL(output)
  
  observeEvent(input$compare, {
    id <- provideFeedback("Analyzing results...")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)
    comparison(output)
  })
  
  
  observeEvent(input$lineupEditor, {
    entries <- read.csv(input$dkentries$datapath)
    ext <- as.character(tools::file_ext(input$dkentries$datapath))
    #cat('entries exists: ', !is.null(entries))
    #cat('\next: ', ext)
    req(entries)
    #cat(ext)
    #shiny::validate(need(ext=="csv", "Upload DKEntries.csv from DK first"))
    #cat('here')
    
    id <- provideFeedback("Editing lineups... Ensure you built the same number of lineups as you previously entered.")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)
    #cat('hi')
    
    lineupEditor(plineups = entries, final_export = final_export)
    
    
  })
  
  output$lineupDownload <- downloadHandler(
    filename=function(){"DKLineups.csv"}
    , content=function(file){
      write.csv(final_export, file, row.names=FALSE)
    }, contentType='text/csv')
  
  output$editDownload <- downloadHandler(filename=function(){"DKEntries.csv"}, content=function(file){write.csv(final_export, file, row.names=FALSE)}, contentType='text/csv')
  
  observeEvent(input$getData,{
    dkSalaries <- read.csv(input$dksalaries$datapath)
    #cat('hi');
    id <- provideFeedback("Updating data...")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)
    ext <- as.character(tools::file_ext(dkSalaries$datapath))
    req(dkSalaries)
    #shiny::validate(need(ext=="csv", "Upload DKSalaries.csv from DK first"))
    
    getData(dkSalaries)
  })
  
  ##### Build lineups #####
  observeEvent(input$build, {
    id <- provideFeedback("Building new lineups...")
    on.exit(removeNotification(id), add = TRUE)
    Sys.sleep(1)
    if(input$limit_player_pool) playerPool <- projections()[input$projections_rows_selected,]$Name else playerPool <-projections()$Name
    
    build_lineups <- input$build_lineups
    batter_lines <- batterLines()
    pitcher_lines <- pitcherLines()
    bans <- input$bans
    locks <- input$locks
    min_salary <- input$min_salary
    
    processing(build_lineups, oprojections, c_strength, b1_strength, b2_strength, b3_strength, ss_strength, of_strength, bans, locks, batter_lines, pitcher_lines, playerPool)
    getBuiltLineups()
    addLineupsToTab(1)
    addLineupsToTab(2)
    addLineupsToTab(3)
    addLineupsToTab(4)
  })
  
  output$feedback <- reactive({"Ready"})
  
  observeEvent(input$save, {
    wd <- getwd()
    write.csv(playerPool(), file = paste(wd, "/savedpool.csv", sep=""))
    getPlayerPool(wd)
    write.csv(pitcherSummary()[input$pitcherSummary_rows_selected,], file = paste(wd, "/ps.csv", sep=""))
    getps(wd)
    write.csv(teamSummary()[input$teamSummary_rows_selected,], file =  paste(wd, "/ts.csv", sep=""))
    getts(wd)
  })
  
  observeEvent(input$clearPool, {
    wd <- getwd()
    initializeSaves(wd)
    getPlayerPool(wd)
    getts(wd)
    getps(wd)
    
    projections <- reactive({oprojections})
    teamSummary <- reactive({team_summary}) 
    pitcherSummary <- reactive({pitcher_summary})
  })
  
  
  
  ##### Defines the dynamically generated "Lineups" tab #####
  
  
  observe({
    addLineupsToTab(1)
    addLineupsToTab(2)
    addLineupsToTab(3)
    addLineupsToTab(4)
  })
  
  
  
  
  
  
  
  ##### edit cells #####
  
  proxyProj=dataTableProxy('projections')
  observeEvent(input$projections_cell_edit,{
    info=input$projections_cell_edit
    #str(info)
    oprojections<<-editData(oprojections,info)
    replaceData(proxyProj,oprojections)
  })
  
  proxyPitch=dataTableProxy('pitcherSummary')
  observeEvent(input$pitcherSummary_cell_edit,{
    info=input$pitcherSummary_cell_edit
    #str(info)
    pitcher_summary<<-editData(pitcher_summary,info)
    replaceData(proxyPitch,pitcher_summary)
  })
  
  proxyBat=dataTableProxy('batterLines')
  observeEvent(input$batterLines_cell_edit,{
    info=input$batterLines_cell_edit
    #str(info)
    batter_lines<<-editData(batter_lines,info)
    replaceData(proxyBat,batter_lines)
  })
  
  proxyPL=dataTableProxy('pitcherLines')
  observeEvent(input$pitcherLines_cell_edit,{
    info=input$pitcherLines_cell_edit
    #str(info)
    pitcher_lines<<-editData(pitcher_lines,info)
    replaceData(proxyPL,pitcher_lines)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)#, enableBookmarking = "server")
