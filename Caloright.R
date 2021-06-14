####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Caloright",
                  tabPanel("Calories & Nutrition",
                           sidebarPanel(
                             tags$h3("Input:"),
                             numericInput("weight", "Weight (kg):", ""),
                             numericInput("height", "Height (cm):", ""),
                             numericInput("age", "Age:", ""),
                             selectInput("gender","Gender:",
                                         c("Male" = "male",
                                           "Female" = "female")),
                             selectInput("activity","Activity level",
                                         c("Sedentery" = "sedentery",
                                           "Lightly Active" = "lightly_active",
                                           "Moderately Active" = "moderately_active",
                                           "Very Active" = "very_active",
                                           "Extra Active" = "extra_active")),
                             selectInput("goal","What is your goal?",
                                         c("Lose Weight" = "lose_weight",
                                           "Maintain Weight" = "maintain_weight",
                                           "Increase Weight" = "increase_weight")),
                           ), # sidebarPanel
                           mainPanel(
                             h1("How much calories shall you take?"),
                             
                             h4("Your Basic Metabolic Rate (BMR) is "),
                             verbatimTextOutput("BMR"), #change according to output
                             h4("Your Total Daily Energy Expenditure (TDEE) is "),
                             verbatimTextOutput("TDEE"), #change according to output, TDEE is the "multiplier" variable in the RMD file.
                             h4("To achieve your goal, the amount of calories you shall take is"),
                             verbatimTextOutput(" "), #change according to output
                             h4("Your nutritions shall be distributed as below"),
                             verbatimTextOutput(" "), #change according to output
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Food Search", 
                           
                           sidebarPanel( #Need to Edit
                             tags$h3("Input:"),
                             textInput("FoodName", "Please enter the name of the food:"),
                             # can be changed to drop down select input too.
                             h5("This application shows your the nutrients and calories of your food.")
                           ),
                           mainPanel(
                             h5('The food that is selected is'),
                             verbatimTextOutput(" "), #display the nutritents and calories
                             verbatimTextOutput(" ") #show that it is high in what nutrients
                           )),
                  tabPanel("Food Reccomendation",
                           flowLayout(
                             h5("This tab shows the reccommended food.")
                           ))
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
