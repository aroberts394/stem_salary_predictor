# ======================= #
# Author: Tony Roberts
# Course: PUBH 7462
# Project: STEM Professional Salary Predictor
# Description: Shiny app ui function for STEM salary predictor
# ======================= #

library(shiny)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # shinythemes::themeSelector(),
    theme = shinytheme("slate"),
    
    # Application title
    titlePanel("STEM Professional Salary Predictor"),

    # Sidebar with a slider input for gender
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = 'gender',
                label = "Enter gender:",
                levels(highered$GENDER)
                ),
            numericInput(
                inputId = 'age',
                label = 'Enter Age:',
                30,
                min = 1
            ),
            numericInput(
                inputId = 'children',
                label = 'No. of Children:',
                1,
                min = 0
            ),
            selectInput(
                inputId = 'degree',
                label = 'Type of degree:',
                levels(highered$DGRDG)
            ),
            selectInput(
                inputId = 'retired',
                label = "Previously retired?",
                levels(highered$FTPRET)
            ),
            selectInput(
                inputId = 'job_code',
                label = "Type of professional:",
                levels(highered$NOCPRMG)
            ),
            actionButton(
                inputId = "",
                label = "Predict",
                icon = icon("refresh"))
        ),
        
        # Show a table of the user inputs
    mainPanel(
        tabsetPanel(
            tabPanel(
                tags$h4("User Inputs:"),
                icon = icon("table"),
                title = "Prediction:",
                tableOutput('table'),
                tags$h4("Based on your inputs, the predicted Salary Range is: ", icon("dollar")),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$br(),
                tags$h4("Cost of Living Index (COLI) by state"),
                leafletOutput("usmap"),
                p()
            ),
            tabPanel(
                icon = icon("book"),
                title = "About",
                tags$div()
            )
            
        )
       
    )
))
)
