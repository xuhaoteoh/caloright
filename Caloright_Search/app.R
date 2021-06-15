
library(tidyverse)

shinyApp(
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "shinythemes",
      tabPanel("Food Search",
               sidebarPanel(
                 tags$h3("Input:"),
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 checkboxInput("header", "Header", TRUE),
                 textInput("FoodName", "Please enter the name of the food:", "general"),
                 tags$h5("This application shows your the nutrients and calories of your food."),
                 actionButton("search", "Search", class = "btn-primary"),
                 #plotOutput("plot"),
                 #tableOutput("table"),
               ),
               mainPanel(
                 h5('The food that is selected is'),
                 verbatimTextOutput("FoodName_txtout"),
                 tabsetPanel(
                   tabPanel("Full File Table",
                            tableOutput("contents")),
                   tabPanel("Nutrition Table",
                            h4("Table"),
                            tableOutput("table"),
                            h4("Verbatim text output"),
                            #verbatimTextOutput("txtout"),

                   ),
                   tabPanel("Food Description",
                            actionButton("searchwiki", "Search Wiki", class = "btn-primary"),),
                   tabPanel("Nutrition Distribution Graph",
                            plotOutput("plot"))
                 )
               )
      ),
      tabPanel("Navbar 2", "This panel is intentionally left blank"),
      tabPanel("Navbar 3", "This panel is intentionally left blank")
    )
  ),
  server = function(input, output) {
    
    fileName <- reactiveValues(data = NULL)
    
    output$contents <- renderTable({

      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      fileName <- read.csv(inFile$datapath, header = input$header)
      head(fileName)
    })
    
    output$FoodName_txtout <- renderText({
      input$FoodName
    })
    
    v <- reactiveValues(data = NULL)
    
    observeEvent(input$search, {
      v$data <- head(cars, 4)
    })
    
    observeEvent(input$searchwiki, {
      if (is.null(input$FoodName)) return()
      message("Opening Wikipedia search for \"", input$FoodName, "\" in browser")
      browseURL(paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(input$FoodName)))
    })
    
    output$table <- renderTable({
      if (is.null(fileName)) return()
      inFile <- input$file1
      fileName <- read.csv(inFile$datapath, header = input$header)
      fileName[which(fileName$name==input$FoodName), ]
    })
    
    output$plot <- renderPlot({
      if (is.null(input$FoodName)) return()
      inFile <- input$file1
      fileName <- read.csv(inFile$datapath, header = input$header)
      selected<- fileName[which(fileName$name==input$FoodName), ]
      if (is.null(selected)) return()
      library(fmsb)
      selected[is.na(selected)] <- 0
      selected <- selected[, colSums(selected != 0) > 0]
      #define Min-Max normalization function
      #min_max_norm <- function(x) {
      #  (x - min(x)) / (max(x) - min(x))
      #}
      #apply Min-Max normalization to first four columns in iris dataset
      selected <- as.data.frame(lapply(selected[2:ncol(selected)], min_max_norm))
      #radarchart(selected[,4:ncol(selected)])
      radarchart(selected[,4:8])
    })
  }
)