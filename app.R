library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)

# Define UI
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  navbarPage(
    
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
               h4(tags$b("Input:")),
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
               h5("Please press calculate after input and selection."),
               submitButton('Calculate')
             ), # sidebarPanel
             mainPanel(
               h3(tags$b('How much calories (kcal) shall you take?')),
               
               h4('Your', tags$b("Basal Metabolic Rate (BMR)"),'is '),
               verbatimTextOutput("BMRCalculation"), #change according to output
               tags$h4(tags$style(HTML("
                            #BMRCalculation {
                              font-size: 15px;
                            }
                            "))),
               br(),
               h4('Your', tags$b("Total Daily Energy Expenditure (TDEE)"), 'is'),
               verbatimTextOutput("TDEECalculation"), #change according to output, TDEE is the "multiplier" variable in the RMD file.
               tags$h4(tags$style(HTML("
                            #TDEECalculation {
                              font-size: 15px;
                            }
                            "))),
               br(),
               h4(tags$b('To achieve your goal, the amount of calories you shall take is')),
               verbatimTextOutput("GoalCalculation"), #change according to output
               tags$h4(tags$style(HTML("
                            #GoalCalculation {
                              font-size: 15px;
                            }
                            "))),
               br(),
               h4(tags$b('Your nutritions shall be distributed as below:')),
               plotOutput("GoalNutrients") #change according to output
             ) # mainPanel
             
    )# Navbar 1, tabPanel
    ,
    tabPanel("Food Search",
             sidebarPanel(
               h4("This application shows the",tags$b("nutrients"), "and", tags$b("calories"), "of your food."),
               #tags$h3("Input:"),
               #                fileInput("file1", "Choose CSV File",
               #                          accept = c(
               #                            "text/csv",
               #                            "text/comma-separated-values,text/plain",
               #                            ".csv")
               #                ),
               tags$hr(),
               #                checkboxInput("header", "Header", TRUE),
               tags$h4("Please choose a food category before search."),
               br(),
               selectInput('category','Category:',
                           c("All_Category" = "All_Category",
                             "Dairy products" = "Dairy_products",
                             "Fats, Oils, Shortenings" = "Fats_Oils_Shortenings",
                             "Meat, Poultry" = "Meat_Poultry",
                             "Fish, Seafood" = "Fish_Seafood",
                             "Vegetables A-E" = "Vegetables_A_E",
                             "Vegetables F-P" = "Vegetables_F_P",
                             "Vegetables R-Z" = "Vegetables_R_Z",
                             "Fruits A-F" = "Fruits_A_F",
                             "Fruits G-P" = "Fruits_G_P",
                             "Fruits R-Z" = "Fruits_R_Z",
                             "Breads, Cereal, Fastfood, Grains" = "BCFG",
                             "Soups" = "Soups",
                             "Desserts, Sweets" = "Desserts_Sweets",
                             "Jams, Jellies" = "Jams_Jellies",
                             "Seeds and Nuts" = "Seeds_and_Nuts",
                             "Drinks, Alcohol, Beverages" = "Drinks_Alcohol_Beverages"
                             ),
                           selected= "All_Category",
               ),
               submitButton('Submit'),
               br(),
               selectInput('product','Product:',
                           choices = NULL,
               ),
               submitButton('Seach'),
               #tags$hr(),
               #textInput("FoodName", "Please enter the name of the food:", "Rice"),
               
               #submitButton('Search'),
               #actionButton("search", "Search", class = "btn-primary"),
               
             ),
             mainPanel(
               h4('The food that is selected is'),
               verbatimTextOutput("FoodName_txtout"),
               tabsetPanel(
                 tabPanel("Table By Category",
                          tableOutput("contents")),
                 #tabPanel("Search Result",
                 #         h4("Search Result Table"),
                 #         tableOutput("table"),
                 #),
                 #tabPanel("Food Description",
                 #submitButton('Search'),
                 #uiOutput("url"),
                 #htmlOutput("frame"),),
                 #actionButton("searchwiki", "Search Wiki", class = "btn-primary"),),
                 #submitButton("searchwiki"),),
                 tabPanel("Selected Product & Nutrition Distribution Graph",
                          uiOutput("url"),
                          tableOutput("table"),
                          plotOutput("plot"))
               )
             )
    )#Navbar 2, tabPanel
    , 
    tabPanel("Food with the Highest Nutritional Value",
             h4(tags$b("The outcome of descriptive analysis is to find out the most recommended for each category: Carbohydrate,Protein & Fat.")),
             
             h4("The food with the highest carbohydrate content is"),
             tableOutput("highest_carbs"),
             plotOutput("highest_carbs_donut"),
             
             h4("The food with the highest protein content is"),
             tableOutput("highest_protein"),
             plotOutput("highest_protein_donut"),
             
             h4("The food with the highest fat content is"),
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
    count=c(data_x$Carbs_per_100gram,data_x$Protein_per_100gram,
            data_x$Fat_per_100gram)
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
server <- function(input, output,session) {
  library(ggplot2)
  bmr <- reactive({BMR(input$gender,input$weight,input$height,input$age)})
  output$BMRCalculation <- renderPrint(bmr())
  
  tdee <- reactive({TDEE(input$activity,bmr())})
  output$TDEECalculation <- renderPrint(tdee())
  
  goal_cal <- reactive({GOAL_CAL(input$goal,tdee())})
  output$GoalCalculation <- renderPrint(goal_cal())
  
  output$GoalNutrients <- renderPlot(GOAL_NUTRIENTS(input$goal,goal_cal()),bg="transparent")
  
  fileName <- reactiveValues(data = NULL)
  FoodNameSelected <- reactiveValues(data = NULL)
  
  output$contents <- renderTable({
    
    #    inFile <- input$file1
    #    
    #    if (is.null(inFile)){
    #      return(NULL)
    #    }else{
    #      fileName <- read.csv(inFile$datapath, header = input$header)
    #    }
    
    fileName <- read.csv("nutrients_cleaned.csv",stringsAsFactors = FALSE)
    
    if (input$category == "All_Category"){
      
    }else if (input$category =="Dairy_products"){
      fileName <- filter(fileName,  Category == "Dairy products")
    }else if (input$category =="Fats_Oils_Shortenings"){
      fileName <- filter(fileName,  Category == "Fats, Oils, Shortenings")
    }else if (input$category =="Meat_Poultry"){
      fileName <- filter(fileName,  Category == "Meat, Poultry")
    }else if (input$category =="Fish_Seafood"){
      fileName <- filter(fileName,  Category == "Fish, Seafood")
    }else if (input$category =="Vegetables_A_E"){
      fileName <- filter(fileName,  Category == "Vegetables A-E")
    }else if (input$category =="Vegetables_F_P"){
      fileName <- filter(fileName,  Category == "Vegetables F-P")
    }else if (input$category =="Vegetables_R_Z"){
      fileName <- filter(fileName,  Category == "Vegetables R-Z")
    }else if (input$category =="Fruits_A_F"){
      fileName <- filter(fileName,  Category == "Fruits A-F")
    }else if (input$category =="Fruits_G_P"){
      fileName <- filter(fileName,  Category == "Fruits G-P")
    }else if (input$category =="Fruits_R_Z"){
      fileName <- filter(fileName,  Category == "Fruits R-Z")
    }else if (input$category =="BCFG"){
      fileName <- filter(fileName,  Category == "Breads, cereals, fastfood,grains")
    }else if (input$category =="Soups"){
      fileName <- filter(fileName,  Category == "Soups")
    }else if (input$category =="Desserts_Sweets"){
      fileName <- filter(fileName,  Category == "Desserts, sweets")
    }else if (input$category =="Jams_Jellies"){
      fileName <- filter(fileName,  Category == "Jams, Jellies")
    }else if (input$category =="Seeds_and_Nuts"){
      fileName <- filter(fileName,  Category == "Seeds and Nuts")
    }else if (input$category =="Drinks_Alcohol_Beverages"){
      fileName <- filter(fileName,  Category == "Drinks,Alcohol, Beverages")
    }
    
    
    fileName
  })
  
  observe({
    fileName <- read.csv("nutrients_cleaned.csv",stringsAsFactors = FALSE)
    
    
    if (input$category == "All_Category"){
      
    }else if (input$category =="Dairy_products"){
      fileName <- filter(fileName,  Category == "Dairy products")
    }else if (input$category =="Fats_Oils_Shortenings"){
      fileName <- filter(fileName,  Category == "Fats, Oils, Shortenings")
    }else if (input$category =="Meat_Poultry"){
      fileName <- filter(fileName,  Category == "Meat, Poultry")
    }else if (input$category =="Fish_Seafood"){
      fileName <- filter(fileName,  Category == "Fish, Seafood")
    }else if (input$category =="Vegetables_A_E"){
      fileName <- filter(fileName,  Category == "Vegetables A-E")
    }else if (input$category =="Vegetables_F_P"){
      fileName <- filter(fileName,  Category == "Vegetables F-P")
    }else if (input$category =="Vegetables_R_Z"){
      fileName <- filter(fileName,  Category == "Vegetables R-Z")
    }else if (input$category =="Fruits_A_F"){
      fileName <- filter(fileName,  Category == "Fruits A-F")
    }else if (input$category =="Fruits_G_P"){
      fileName <- filter(fileName,  Category == "Fruits G-P")
    }else if (input$category =="Fruits_R_Z"){
      fileName <- filter(fileName,  Category == "Fruits R-Z")
    }else if (input$category =="BCFG"){
      fileName <- filter(fileName,  Category == "Breads, cereals, fastfood,grains")
    }else if (input$category =="Soups"){
      fileName <- filter(fileName,  Category == "Soups")
    }else if (input$category =="Desserts_Sweets"){
      fileName <- filter(fileName,  Category == "Desserts, sweets")
    }else if (input$category =="Jams_Jellies"){
      fileName <- filter(fileName,  Category == "Jams, Jellies")
    }else if (input$category =="Seeds_and_Nuts"){
      fileName <- filter(fileName,  Category == "Seeds and Nuts")
    }else if (input$category =="Drinks_Alcohol_Beverages"){
      fileName <- filter(fileName,  Category == "Drinks,Alcohol, Beverages")
    }
    
    for(atmp in fileName['Food']) { avector <- atmp }
    updateSelectInput(session, "product",
                      choices = avector,
                      selected = tail(avector, 1)
    )
  })
  
  output$FoodName_txtout <- renderText({
    #input$FoodName
    input$product
  })
  
  v <- reactiveValues(data = NULL)
  
  
  #observeEvent(input$searchwiki, {
  #  if (is.null(input$FoodName)) return()
  #  message("Opening Wikipedia search for \"", input$FoodName, "\" in browser")
  #  browseURL(paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(input$FoodName)))
  #})
  
  output$url <-renderUI({
    if (is.null(input$product)) return()
    a(href=paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(input$product)),"Show WikiPedia Page in your Browser",target="_blank")
  })
  
  output$frame <- renderUI({
    if (is.null(input$product)) return()
    test <- paste0("https://en.wikipedia.org/w/index.php?search=", URLencode(input$product))
    my_test <- tags$iframe(src=test, height=600, width=535)
    print(my_test)
    my_test
  })
  
  output$table <- renderTable({
    if (is.null(input$product)) return()
    #inFile <- input$file1
    #fileName <- read.csv(inFile$datapath, header = input$header)
    fileName <- read.csv("nutrients_cleaned.csv",stringsAsFactors = FALSE)
    fileName[which(fileName$Food==input$product), ]
    
  },bordered=TRUE,align="c")
  
  output$plot <- renderPlot({
    if (is.null(input$product)) return()
    #inFile <- input$file1
    #fileName <- read.csv(inFile$datapath, header = input$header)
    fileName <- read.csv("nutrients_cleaned.csv",stringsAsFactors = FALSE)
    selected<- fileName[which(fileName$Food==input$product), ]
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
      labs(title = paste("The Compositions of Nutrients in",input$FoodName)) + 
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid  = element_blank())
  })
  my_nurients_data = read.csv("nutrients_cleaned.csv",stringsAsFactors = FALSE)
  
  #nutrients_per_gram
  nutrients_per_100gram = my_nurients_data %>% mutate(
    Carbs_per_100gram = 100*Carbs/Grams,
    Protein_per_100gram = 100*Protein/Grams,
    Fat_per_100gram = 100*Fat/Grams) %>% select(
      Food,Grams,Calories,Carbs,Protein,Fat,Carbs_per_100gram,
      Protein_per_100gram,Fat_per_100gram)
  
  max_nutrients = nutrients_per_100gram
  max_carbs = max(max_nutrients$Carbs_per_100gram)
  
  highest_carbs= max_nutrients %>% filter(
    Carbs_per_100gram == max_carbs
  ) %>% select(
    Food,Calories,Carbs,Protein,Fat,
    Carbs_per_100gram,Protein_per_100gram,Fat_per_100gram)
  #output$highest_carbs <- renderPrint(highest_carbs) # food with max carbs
  
  output$highest_carbs <- renderTable(highest_carbs,bordered=TRUE,align="c")
  
  highest_nut_carbs <- data.frame(
    category=c("Carbohydrates", "Protein"),
    count=c(highest_carbs$Carbs_per_100gram,highest_carbs$Protein_per_100gram)
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
  
  max_protein = max(max_nutrients$Protein_per_100gram)
  highest_protein= max_nutrients %>% filter(
    Protein_per_100gram == max_protein
  ) %>% select(
    Food,Calories,Carbs,Protein,Fat,
    Carbs_per_100gram,Protein_per_100gram,Fat_per_100gram)
  highest_protein <- head(highest_protein,1)
  
  output$highest_protein <- renderTable(highest_protein,bordered=TRUE,align="c") # food with max protein
  
  output$highest_protein_donut <- renderPlot(DONUT_GRAPH(highest_protein))
  
  max_fat = max(max_nutrients$Fat_per_100gram)
  highest_fat= max_nutrients %>% filter(
    Fat_per_100gram == max_fat
  ) %>% select(
    Food,Calories,Carbs,Protein,Fat,
    Carbs_per_100gram,Protein_per_100gram,Fat_per_100gram)
  highest_fat <- head(highest_fat,1)
  output$highest_fat <- renderTable(highest_fat,bordered=TRUE,align="c") # food with max fat
  output$highest_fat_donut <- renderPlot(DONUT_GRAPH(highest_fat))
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
