
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
      
      df2 <- data.frame(t(selected[-1]))
      
      colnames(df2) <- "count"
      df2[is.na(df2)] <- 0
      
      df2 <- cbind(category = rownames(df2), df2)
      rownames(df2) <- 1:nrow(df2)
      
      
      data <- df2[c(-1,-2,-3,-4),]
      
      
      # load library
      library(ggplot2)
      
      # Compute percentages
      
      data$count = as.numeric(as.character(data$count))
      data <- data[order(data$count,decreasing = TRUE),]
      data <- data[(1:6),]
      data$fraction = data$count / sum(data$count)
      
      # Compute the cumulative percentages (top of each rectangle)
      data$ymax = cumsum(data$fraction)
      
      # Compute the bottom of each rectangle
      data$ymin = c(0, head(data$ymax, n=-1))
      
      # Make the plot
      ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
        geom_rect() +
        coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
        xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
        labs(title = paste("Top Six Componets in " , input$FoodName))
    })
  }
)