#Testing

library(shiny)
library(shinythemes)
library(ggplot2)

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


options(shiny.error = browser)

# Define server function  
server <- function(input, output) {
  bmr <- reactive({BMR(input$gender,input$weight,input$height,input$age)})
  output$BMRCalculation <- renderPrint(bmr())
  
  tdee <- reactive({TDEE(input$activity,bmr())})
  output$TDEECalculation <- renderPrint(tdee())
  
  goal_cal <- reactive({GOAL_CAL(input$goal,tdee())})
  output$GoalCalculation <- renderPrint(goal_cal())
  
  output$GoalNutrients <- renderPlot(GOAL_NUTRIENTS(input$goal,goal_cal()),bg="transparent")
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
