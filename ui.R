# ======================= #
# Author: Tony Roberts
# Course: PUBH 7462
# Project: STEM Professional Salary Predictor
# Description: Shiny app ui function for STEM salary predictor
# ======================= #

library(shiny)
library(shinythemes)
library(markdown)


# Define UI for application
navbarPage(title = "STEM Professional Salary Predictor", 
           theme = shinytheme("slate"),
           tabPanel(
               title = "Prediction",
               icon = icon("cog"),
               sidebarLayout(
                   sidebarPanel(
                       helpText("Select relevant inputs below and click on the Predict button to generate salary predictions."),
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
                            label = 'How related is the job to your degree:',
                            levels(highered_simple$OCEDRLP)
                            ),
                        selectInput(
                            inputId = 'profession',
                            label = "Type of profession:",
                            levels(highered_simple$NOCPRMG)
                            ),
                        selectInput(
                            inputId = 'years',
                            label = "Years since graduation:",
                            levels(highered_simple$YEARS_SINCE_GRAD)
                            ),
                        actionButton(
                            inputId = 'predict',
                            label = "Predict",
                            icon = icon("refresh"))
                       
                       ), # close sideBarPanel
                   
                   mainPanel(
                       tags$h4("User Inputs:"),
                       title = "Prediction:",
                       tableOutput('inputs'),
                       tags$br(),
                       tags$br(),
                       # tags$h4("Based on your inputs, the predicted Salary Range is: ", icon("dollar")),
                       tags$h4(textOutput('salary')),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$br(),
                       tags$h4("Cost of Living Index (COLI) by state"),
                       leafletOutput("usmap"),
                        # p()
                       
                       ) # close mainPanel
                
            ), # close sideBarLayout
                
          ), # close tabPanel
          
          # format data table
          tags$head(tags$style(HTML(
              "
                  .dataTables_length label,
                  .dataTables_filter label,
                  .dataTables_info {
                      color: white!important;
                      }

                  .paginate_button {
                      background: white!important;
                  }

                  thead {
                      color: white;
                  }
              button, input, optgroup, select, 
              textarea {color: black!important}

                  "))),
          
          tabPanel(
              title = "Table", 
              icon = icon("table"),
              downloadButton('downloadData', 'Download data'),
              DT::DTOutput("table"),
              tags$div()
          ), # close TabPanel
           
          tabPanel(
               title = "About", 
               icon = icon("book"),
               tags$div()
           ) # close TabPanel
          
    
) # end navbarPage

