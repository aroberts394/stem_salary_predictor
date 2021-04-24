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
            numericInput(
                inputId = 'age',
                label = 'Enter Age:',
                30,
                min = 1
            ),
            selectInput(
                inputId = 'gender',
                label = "Enter gender:",
                levels(highered_simple$GENDER)
                ),
            selectInput(
                inputId = 'race',
                label = "Enter race/ethnicity:",
                levels(highered_simple$RACETH)
            ),
            selectInput(
                inputId = 'children',
                label = "Enter number of children:",
                levels(highered_simple$CHTOT)
            ),
            selectInput(
                inputId = 'workhours',
                label = 'Hours worked per week:',
                levels(highered_simple$HRSWKGR)
            ),
            selectInput(
                inputId = 'empsize',
                label = 'Employer size:',
                levels(highered_simple$EMSIZE)
            ),
            selectInput(
                inputId = 'empsec',
                label = 'Employer sector',
                levels(highered_simple$EMSEC)
            ),
            selectInput(
                inputId = 'work_related',
                label = 'How related to your degree:',
                levels(highered_simple$OCEDRLP)
            ),
            selectInput(
                inputId = 'profession',
                label = "Type of professional:",
                levels(highered_simple$NOCPRMG)
            ),
            selectInput(
                inputId = 'years',
                label = "Years since graduation:",
                levels(highered_simple$YEARS_SINCE_GRAD)
            ),
            actionButton(
                inputId = "",
                label = "Predict",
                icon = icon("refresh"))
        ),
        
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
