#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinyFeedback)

isBuilding <- FALSE

tryCatch(
  expr={
    playerPoolSaved <-read.csv("C:/Users/jorda/OneDrive/Documents/MLB DFS 2023/savedpool.csv")
    
    #playerPoolSaved <-read.csv("savedpool.csv")
    }
  , error=function(e){
    playerPoolSaved <- "cleared"
    }
  )
tryCatch(
  expr={
    builtLineups <-read.csv("C:/Users/jorda/Downloads/DKLineupsMLB.csv")
    
    #playerPoolSaved <-read.csv("savedpool.csv")
  }
  , error=function(e){
    builtLineups <- "cleared"
  }
)

# Define UI for application that draws a histogram
ui <- function(request){
  
  fluidPage(
    titlePanel("Lineup Builder"),
    
    textOutput("feedback"),
    actionButton("build", "Build"),

  #downloadButton(outputId = "downloadData", label = "Download"),
  actionButton("save", "Save"),
  actionButton("clearPool", "Clear Pool"),
  tabsetPanel(
    tabPanel(
      "Projections",
      mainPanel(DT::dataTableOutput("projections"))
    ),
    tabPanel("Parameters",
             numericInput("build_lineups", "Number of Lineups",  20),
             selectInput("bans", "Bans", c(oprojections$Name, unique(oprojections$Team)), multiple=TRUE),
             selectInput("locks", "Locks", c(oprojections$Name, unique(oprojections$Team)), multiple=TRUE),
             sliderInput("min_salary", "Min Salary", value = 47000, min = 40000, max=50000),
             sliderInput("bs_size", "Big Stack Size", value = 5, min=2, max=5),
             checkboxInput("allow_batters_against_pitcher", "Allow batters against pitcher?", value=FALSE),
             numericInput("assume_abs", "Default # of ABs per Batter", 4),
             #selectInput("five_stacks", "Big Stack Teams", unique(oprojections$Team), multiple=TRUE),
             sliderInput("bo_limit", "Max sum of batting order", value=40, min=20, max=80)
             
          
    ),
    tabPanel("Team Summary",
             mainPanel(DT::dataTableOutput("teamSummary"))
    ),
    tabPanel("Pitcher Summary",
             mainPanel(DT::dataTableOutput("pitcherSummary"))
    ),
    tabPanel("My Player Pool",
             mainPanel(DT::dataTableOutput("playerPool"))
    ),
    tabPanel("Lineups",
             
           wellPanel(
             fileInput(
               inputId = "files",
               label = "Choose cvs files",
               accept = c('text/csv',
                          'text/comma-separated-values,text/plain',
                          '.csv'),
               multiple = TRUE
             )
           ),
          column(5, uiOutput("tables"))
    ),
    tabPanel("Ownership",
             mainPanel(DT::dataTableOutput("Ownership"))
    )
  )

)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Reactive Expression caches this result for use throughout server function; more computationally efficient and easier to debug
  projections <- reactive({oprojections})
  teamSummary <- reactive({team_summary}) 
  pitcherSummary <- reactive({pitcher_summary})
  playerPool <- reactive({data.frame()})
  Ownership <- reactive({ownership})
  
  output$Ownership <- DT::renderDT(datatable(data=ownership))

  selectedPlayers <- reactive({
    unique(as.data.frame(rbind(projections()[input$projections_rows_selected,]
                 , projections()[projections()$Team %in% teamSummary()[input$teamSummary_rows_selected,]$Team,]
                 , projections()[projections()$Name %in% pitcherSummary()[input$pitcherSummary_rows_selected,]$Name,])
    ))
  })
  

  
  output$projections <- DT::renderDT(
    if(playerPoolSaved[[1, 2]] == "cleared"){
      datatable( data = projections(),editable = "all", filter="top")
    }else{
    datatable( data = projections(),
               editable = "all"
               , filter="top"
               ,selection= list(mode='multiple', selected = c(1:nrow(projections()))[projections()$ID %in% playerPoolSaved$ID], target = 'row', selectable = TRUE)
               
               )
    }
    )
  output$playerPool <- DT::renderDataTable(
    if(playerPoolSaved[[1, 2]] == "cleared" && nrow(selectedPlayers())==0){
      playerPool()
    }else{
      selectedPlayers()
      
    }
  )
  
  output$teamSummary <- DT::renderDataTable(
    datatable(data=teamSummary())
  )
  
  output$pitcherSummary <- DT::renderDataTable(
    datatable(data=pitcherSummary())
  )
  
  observeEvent(input$build, {
    output$feedback <- renderText("Building...")
    
    poolprojections <- selectedPlayers()
    build_lineups <- input$build_lineups
    bans <- input$bans
    locks <- input$locks
    five_stacks <- teamSummary()[input$teamSummary_rows_selected,]$Team
    min_salary <- input$min_salary
    bs_size <- input$bs_size
    allow_batters_against_pitcher <- input$allow_batters_against_pitcher
    assume_abs <- input$assume_abs
    bo_limit <- input$bo_limit
    processing(build_lineups, poolprojections, c_strength, b1_strength, b2_strength, b3_strength, ss_strength, of_strength, bans, locks, five_stacks, min_salary, bs_size, allow_batters_against_pitcher, batter_lines, assume_abs, pitcher_lines, bo_limit)
    write.csv(selectedPlayers(), file = "C:/Users/jorda/OneDrive/Documents/MLB DFS 2023/savedpool.csv")
    output$feedback <- renderText("Done")
    
  })

  feedback <- reactive({"Ready"})
  
  observeEvent(input$save, {
    write.csv(selectedPlayers(), file = "C:/Users/jorda/OneDrive/Documents/MLB DFS 2023/savedpool.csv")
  })
  
  observeEvent(input$clearPool, {
    projections <- reactive({oprojections})
    teamSummary <- reactive({team_summary}) 
    pitcherSummary <- reactive({pitcher_summary})
    write.csv("cleared", file = "C:/Users/jorda/OneDrive/Documents/MLB DFS 2023/savedpool.csv")
  })

  observe({
    if (!is.null(input$files)) {
      max_table = length(input$files[, 1])
      
      lst <- list()
      for (i in 1:length(input$files[, 1])) {
        lst[[i]] <-
          read.csv(
            input$files[[i, 'datapath']],
            sep = ",",
            header = F,
            skip = 4,
            dec = "."
          )
      }
      
      output$tables <- renderUI({
        plot_output_list <- lapply(1:max_table, function(i) {
          tablename <- paste("tablename", i, sep = "")
          tableOutput(tablename)
        })
        do.call(tagList, plot_output_list)
      })
      
      for (i in 1:max_table) {
        local({
          my_i <- i
          tablename <- paste("tablename", my_i, sep = "")
          output[[tablename]] <- renderTable({
            lst[[my_i]]
          })
        })
      }
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "server")
