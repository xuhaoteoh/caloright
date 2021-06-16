#Testing

library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)

# Define UI
ui <- fluidPage(
  shinythemes::themeSelector(),
  #theme = shinytheme("cyborg"),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    
    "Caloright",
    tabPanel("Introduction",
             div(img(src="1.png"),style="text-align: center;"),
             br(),
             div(img(src="2.png"),style="text-align: center;"),
             br(),
             div(img(src="3.png"),style="text-align: center;"),
    ),
    tabPanel("Calories & Nutrition",
             sidebarPanel(
               tags$h3("Input:"),
               numericInput('weight', 'Weight (kg):', 55, min = 40, max = 300, step = 0.1),
               numericInput('height', 'Height (cm):', 150, min = 80, max = 200, step = 0.1),
               numericInput('age', 'Age:', 20, min = 1, max = 100, step = 1),
               selectInput('gender','Gender:',
                           c("Male" = "male",
                             "Female" = "female")),
               selectInput('activity','Activity level',
                           c("Sedentary (Little or no exercise, desk job)" = "sedentary",
                             "Lightly Active (Exercise 1-2 days per week)" = "lightly_active",
                             "Moderately Active (Exercise 3-5 days per week)" = "moderately_active",
                             "Very Active (Exercise 6-7 days per week)" = "very_active",
                             "Athlete (Exercise 2x per day) " = "extra_active")),
               selectInput('goal','What is your goal?',
                           c("Lose Weight" = "lose_weight",
                             "Maintain Weight" = "maintain_weight",
                             "Gain Weight" = "gain_weight")),
               h5("Press submit after input and selection."),
               submitButton('Submit')
             ), # sidebarPanel
             mainPanel(
               h1('How much calories shall you take?'),
               
               h4('Your Basal Metabolic Rate (BMR) is '),
               verbatimTextOutput("BMRCalculation"), #change according to output
               h4('Your Total Daily Energy Expenditure (TDEE) is '),
               verbatimTextOutput("TDEECalculation"), #change according to output, TDEE is the "multiplier" variable in the RMD file.
               h4('To achieve your goal, the amount of calories you shall take is'),
               verbatimTextOutput("GoalCalculation"), #change according to output
               h4('Your nutritions shall be distributed as below'),
               plotOutput("GoalNutrients") #change according to output
             ) # mainPanel
             
    )# Navbar 1, tabPanel
    ,
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
               textInput("FoodName", "Please enter the name of the food:", "Rice"),
               tags$h5("This application shows your the nutrients and calories of your food."),
               submitButton('Search'),
               #actionButton("search", "Search", class = "btn-primary"),
               
             ),
             mainPanel(
               h5('The food that is selected is'),
               verbatimTextOutput("FoodName_txtout"),
               tabsetPanel(
                 tabPanel("Full File Table",
                          tableOutput("contents")),
                 tabPanel("Nutrition Table",
                          h4("Search Result Table"),
                          tableOutput("table"),
                 ),
                 #tabPanel("Food Description",
                 #submitButton('Search'),
                 #uiOutput("url"),
                 #htmlOutput("frame"),),
                 #actionButton("searchwiki", "Search Wiki", class = "btn-primary"),),
                 #submitButton("searchwiki"),),
                 tabPanel("Nutrition Distribution Graph",
                          uiOutput("url"),
                          plotOutput("plot"))
               )
             )
    )#Navbar 2, tabPanel
    , 
    tabPanel("Food with the Highest Nutritional Value",
             h4(tags$b("The outcome of descriptive analysis is to find out the most recommended for each category : Carbohydrate,Protein & Fat.")),
             
             h4("The food with highest carbohydrate is"),
             tableOutput("highest_carbs"),
             plotOutput("highest_carbs_donut"),
             
             h4("The food with highest protein is"),
             tableOutput("highest_protein"),
             plotOutput("highest_protein_donut"),
             
             h4("The food with highest fat is"),
             tableOutput("highest_fat"),
             plotOutput("highest_fat_donut")
    ) #Navbar 2, tabPanel 
  ) # navbarPage
)# fluidPage

BMR <- function(gender,weight, height, age) {
  if (gender == 'male'){
    return(66 + (13.7*weight)+ 5*(height) - (6.8*age))
  }
  else if (gender == 'female'){
    return(655 +(9.6*weight)+1.8*(height)-(4.7*age))
  }
}
TDEE <- function(activity,b) {
  if(activity == 'sedentary'){
    b*1.2
  }else if(activity == 'lightly_active'){
    b*1.375
  }else if(activity == 'moderately_active'){
    b * 1.55
  }else if(activity == 'very_active'){
    b *1.725
  }else if(activity == 'extra_active'){
    b*1.9
  }
}

GOAL_CAL <- function(goal,t) {
  if(goal == 'lose_weight'){
    t*0.9
  }else if(goal == 'maintain_weight'){
    t*1
  }else{
    t*1.1
  }
}

GOAL_NUTRIENTS <- function(goal,gc) {
  if(goal == 'lose_weight'){
    
    carbs_cal = (40/100)*gc
    protein_cal = (30/100)*gc
    fat_cal = (30/100)*gc
    
    data <- data.frame(
      category=c("Carbohydrates", "Protein", "Fat"),
      count=c(carbs_cal,protein_cal,fat_cal)
    )
    
    # Compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute percentage
    data$percentage <- data$fraction*100
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, " (", data$percentage, "%)", "\n Calories: ", data$count)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette="Pastel1") +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
  }else if(goal == 'maintain_weight'){
    
    carbs_cal = (50/100)*gc
    protein_cal = (30/100)*gc
    fat_cal = (20/100)*gc
    
    data <- data.frame(
      category=c("Carbohydrates", "Protein", "Fat"),
      count=c(carbs_cal,protein_cal,fat_cal)
    )
    
    # Compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute percentage
    data$percentage <- data$fraction*100
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, " (", data$percentage, "%)", "\n Calories: ", data$count)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette="Pastel1") +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
    # return("50% Carbohydrates, 30% Protein, 20% Fat")
  }else if(goal == 'gain_weight'){
    
    carbs_cal = (55/100)*gc
    protein_cal = (30/100)*gc
    fat_cal = (15/100)*gc
    
    data <- data.frame(
      category=c("Carbohydrates", "Protein", "Fat"),
      count=c(carbs_cal,protein_cal,fat_cal)
    )
    
    # Compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # Compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # Compute percentage
    data$percentage <- data$fraction*100
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$category, " (", data$percentage, "%)", "\n Calories: ", data$count)
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() +
      geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
      scale_fill_brewer(palette="Pastel1") +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
    
    #return("55% Carbohydrates, 30% Protein, 15% Fat")
  }
  
}

DONUT_GRAPH <- function(data_x){
  df3 <- data.frame(
    category=c("Carbohydrates", "Protein","Fat"),
    count=c(data_x$carbs_per_100gram,data_x$protein_per_100gram,
            data_x$fat_per_100gram)
  )
  
  # Compute fraction
  df3$fraction <- df3$count / sum(df3$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  df3$ymax <- cumsum(df3$fraction)
  
  # Compute percentage
  df3$percentage <- round(df3$fraction*100,digits=2)
  
  # Compute the bottom of each rectangle
  df3$ymin <- c(0, head(df3$ymax, n=-1))
  
  # Compute label position
  df3$labelPosition <- (df3$ymax + df3$ymin) / 2
  
  # Compute a good label
  df3$label <- paste0(
    df3$category, " (", df3$percentage, "%)", 
    "\n Value per 100g: \n", df3$count)
  
  # Make the plot
  ggplot(df3, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=5) +
    scale_fill_brewer(palette="Pastel1") +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none")
}

options(shiny.error = browser)

# Define server function  
server <- function(input, output) {
  library(ggplot2)
  bmr <- reactive({BMR(input$gender,input$weight,input$height,input$age)})
  output$BMRCalculation <- renderPrint(bmr())
  
  tdee <- reactive({TDEE(input$activity,bmr())})
  output$TDEECalculation <- renderPrint(tdee())
  
  goal_cal <- reactive({GOAL_CAL(input$goal,tdee())})
  output$GoalCalculation <- renderPrint(goal_cal())
  
  output$GoalNutrients <- renderPlot(GOAL_NUTRIENTS(input$goal,goal_cal()),bg="transparent")
  
  fileName <- reactiveValues(data = NULL)
  
  output$contents <- renderTable({
    
#    inFile <- input$file1
#    
#    if (is.null(inFile)){
#      return(NULL)
#    }else{
#      fileName <- read.csv(inFile$datapath, header = input$header)
#    }
    fileName <- read.csv("D:/Teoh/Documents/R Programming/nutrients_cleaned.csv",stringsAsFactors = FALSE)
    head(fileName)
  })
  
  output$FoodName_txtout <- renderText({
    input$FoodName
  })
  
  v <- reactiveValues(data = NULL)
  
  
  #observeEvent(input$searchwiki, {
  #  if (is.null(input$FoodName)) return()
  #  message("Opening Wikipedia search for \"", input$FoodName, "\" in browser")
  #  browseURL(paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(input$FoodName)))
  #})
  
  output$url <-renderUI(a(href=paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(input$FoodName)),"Show WikiPedia Page in your Browser",target="_blank"))
  
  output$frame <- renderUI({
    test <- paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(input$FoodName))
    my_test <- tags$iframe(src=test, height=600, width=535)
    print(my_test)
    my_test
  })
  
  output$table <- renderTable({
    if (is.null(fileName)) return()
    #inFile <- input$file1
    #fileName <- read.csv(inFile$datapath, header = input$header)
    fileName <- read.csv("D:/Teoh/Documents/R Programming/nutrients_cleaned.csv",stringsAsFactors = FALSE)
    fileName[which(fileName$Food==input$FoodName), ]
  })
  
  output$plot <- renderPlot({
    if (is.null(input$FoodName)) return()
    #inFile <- input$file1
    #fileName <- read.csv(inFile$datapath, header = input$header)
    fileName <- read.csv("D:/Teoh/Documents/R Programming/nutrients_cleaned.csv",stringsAsFactors = FALSE)
    selected<- fileName[which(fileName$Food==input$FoodName), ]
    if (is.null(selected)) return()
    
    df2 <- data.frame(t(selected[-1]))
    
    colnames(df2) <- "count"
    df2[is.na(df2)] <- 0
    
    df2 <- cbind(category = rownames(df2), df2)
    rownames(df2) <- 1:nrow(df2)
    
    
    data <- df2[c(-1,-2,-3,-6,-9),]
    
    
    # Compute percentages
    
    data$count = as.numeric(as.character(data$count))
    data <- data[order(data$count,decreasing = TRUE),]
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
      labs(title = paste("Top Six Componets in ",input$FoodName)) + 
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())
  })
  my_nurients_data = read.csv("D:/Teoh/Documents/R Programming/nutrients_cleaned.csv",stringsAsFactors = FALSE)
  
  #nutrients_per_gram
  nutrients_per_100gram = my_nurients_data %>% mutate(
    carbs_per_100gram = 100*Carbs/Grams,
    protein_per_100gram = 100*Protein/Grams,
    fat_per_100gram = 100*Fat/Grams) %>% select(
      Food,Grams,Calories,Carbs,Protein,Fat,carbs_per_100gram,
      protein_per_100gram,fat_per_100gram)
  
  max_nutrients = nutrients_per_100gram
  max_carbs = max(max_nutrients$carbs_per_100gram)
  
  highest_carbs= max_nutrients %>% filter(
    carbs_per_100gram == max_carbs
  ) %>% select(
    Food,Carbs,Protein,Fat,
    carbs_per_100gram,protein_per_100gram,fat_per_100gram)
  #output$highest_carbs <- renderPrint(highest_carbs) # food with max carbs
  
  output$highest_carbs <- renderTable(highest_carbs)
  
  highest_nut_carbs <- data.frame(
    category=c("Carbohydrates", "Protein"),
    count=c(highest_carbs$carbs_per_100gram,highest_carbs$protein_per_100gram)
  )
  
  # Compute fraction
  highest_nut_carbs$fraction <- highest_nut_carbs$count / sum(highest_nut_carbs$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  highest_nut_carbs$ymax <- cumsum(highest_nut_carbs$fraction)
  
  # Compute percentage
  highest_nut_carbs$percentage <- round(highest_nut_carbs$fraction*100,digits=2)
  
  # Compute the bottom of each rectangle
  highest_nut_carbs$ymin <- c(0, head(highest_nut_carbs$ymax, n=-1))
  
  # Compute label position
  highest_nut_carbs$labelPosition <- (highest_nut_carbs$ymax + highest_nut_carbs$ymin) / 2
  
  # Compute a good label
  highest_nut_carbs$label <- paste0(
    highest_nut_carbs$category, " (", highest_nut_carbs$percentage, "%)", 
    "\n Value per 100g: ", highest_nut_carbs$count)
  
  # Make the plot
  highest_carbs_donutplot <- ggplot(highest_nut_carbs, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
    scale_fill_brewer(palette="Pastel1") +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(legend.position = "none")
  
  output$highest_carbs_donut <- renderPlot(highest_carbs_donutplot)
  
  max_protein = max(max_nutrients$protein_per_100gram)
  highest_protein= max_nutrients %>% filter(
    protein_per_100gram == max_protein
  ) %>% select(
    Food,Carbs,Protein,Fat,
    carbs_per_100gram,protein_per_100gram,fat_per_100gram)
  highest_protein <- head(highest_protein,1)
  
  output$highest_protein <- renderTable(highest_protein) # food with max protein
  
  output$highest_protein_donut <- renderPlot(DONUT_GRAPH(highest_protein))
  
  max_fat = max(max_nutrients$fat_per_100gram)
  highest_fat= max_nutrients %>% filter(
    fat_per_100gram == max_fat
  ) %>% select(
    Food,Carbs,Protein,Fat,
    carbs_per_100gram,protein_per_100gram,fat_per_100gram)
  highest_fat <- head(highest_fat,1)
  output$highest_fat <- renderTable(highest_fat) # food with max fat
  output$highest_fat_donut <- renderPlot(DONUT_GRAPH(highest_fat))
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
