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
                         inputId = 'degree',
                         label = 'Highest degree received',
                         levels(highered_simple$DGRDG)
                       ),
                       selectInput(
                           inputId = 'profession',
                           label = "Type of job:",
                           levels(highered_simple$NOCPRMG)
                           ),
                        selectInput(
                            inputId = 'work_related',
                            label = 'How related is the job to your degree:',
                            levels(highered_simple$OCEDRLP)
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
                       tags$h4("Cost of Living Index (COLI) by U.S. State"),
                       leafletOutput("usmap"),
                       tags$br(),
                       tags$h4(textOutput('meancoli')),
                       tags$h4(textOutput('mediancoli'))
                        # p()
                       
                       ) # close mainPanel
                
            ), # close sideBarLayout
                
          ), # close prediction tabPanel
          
          
          # EDA Tab Panel
          tabPanel(
              title = "EDA",
              icon = icon("chart-bar"),
              tags$h2("Data Explorer"),
              sidebarLayout(
                  sidebarPanel(
                      
                      sliderInput("sampleSize", "Plot sample size (n)", 
                                  min = 1, 
                                  max = nrow(df),
                                  value = min(1000, nrow(df)), 
                                  step = nrow(df) / 50, round = 0),
                      radioButtons("sampleType", 
                                   "Plot sample type",
                                   choices = list("Random n" = "random", "First n" = "first")),
                      numericInput("sampleSeed", 
                                   "Sample seed", value = 1),
                      
                      selectInput("x", 
                                  "X", 
                                  names(df)),
                      selectInput("y", 
                                  "Y", 
                                  c("None", names(df)), 
                                  names(df)[[2]]),
                      
                      # only allow non-numeric variables for color
                      selectInput("color", "Color", c("None", names(df)[not_numeric])),
                      
                      p("Jitter and smoothing are only available when two numeric variables are selected."),
                      checkboxInput("jitter", "Jitter"),
                      checkboxInput("smooth", "Smooth")
                      
                  ), # close sidebarPanel
                  
                  mainPanel(
                      
                      plotOutput('plot')
                      
                  ) # close mainPanel
                  
              ) # close sidebarLayout
              
          ), # close EDA tabPanel
          
         
          # data table tab section
          tabPanel(
              title = "Table", 
              icon = icon("table"),
              tags$h2("Data Table"),
              downloadButton('downloadData', 'Download data'),
              DT::DTOutput("table"),
              
              # format data table
              tags$head(
                  tags$style(
                      HTML(
                          ".dataTables_length label,
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
                      textarea {color: black!important}"
                      )))
              
          ), # close data table TabPanel
          
          # about tab section 
          tabPanel(
               title = "About", 
               icon = icon("info"),
               tags$h2("Predicting Salaries of STEM Professionals"),
               tags$br(),
               tags$div(
                   style = "font-size: 20px",
                   tags$p("This Shiny application was developed to predict salaries of science and engineering professionals. According to an article on Payscale.com, it stated that in 2019, women earned 79 cents for every dollar men made. This model is intended to predict a salary range based on a 95% confidence interval given a variety of demographic, employer and individual characteristics. The motivation for this Shiny app is to investigate what qualities of an individual have the most influence on salary in the STEM field and understand how these qualities are related."),
                   tags$br(),
                   tags$p("Prediction of salaries was achieved by creating a regression model using survey data on scientists and engineers. The survey data was curated by the University of Minnesota IPUMS Higher Ed. IPUMS Higher Ed disseminates data from the Scientists and Engineers Statistical Data System (SESTAT), the leading surveys for studying the science and engineering (STEM) workforce in the United States. Data from the National Surveys of College Graduates (NSCG), Recent College Graduates (NSRCG) and Doctorate Recipients (SDR) are integrated from 1993 to the present. The IPUMS Higher ED website provides open access to the survey data up until 2013."),
                   tags$br(),
                   tags$p("In order to provide predictions of salaries adjusted for inflation and to transform older salary data to present values, Consumer Provider Index (CPI) inflation data from the U.S. Bureau of Labor Statistics' CPI Inflation Calculator was obtained. The CPI inflation calculator uses the Consumer Price Index for All Urban Consumers (CPI-U) U.S. city average series for all items, not seasonally adjusted. This data represents changes in the prices of all goods and services purchased for consumption by urban households. The salaries from the surveys were then adjusted using the CPI index as of March 2021."),
                   tags$br(),
                   tags$p(strong("Note:"), "This prediction is currently based on a relatively simple linear regression model. Further work would be required to evaluate other machine learning algorithms and model selection techniques to increase salary prediction accuracy and determine reliable variable importance measures."),
                   tags$p("Also, additional adjustments will need to be incorporated in the model to account for bias in the underlying data, gender and ethnic salary disparities, and cost of living differences."),
                   tags$br(),
                   tags$br()
                   ), # close div tag

               # data sources section
               tags$div(
                   style = "font-size: 20px",
                   tags$h2("Data Sources"),
                   tags$ul(
                       tags$li(
                           tags$a("IPUMS Higher ED", href = "https://ipums.org/projects/ipums-higher-ed")
                           ),
                       tags$li(
                           tags$a("U.S. Bureau of Labor Statistics CPI Calculator", href = "https://www.bls.gov/data/inflation_calculator.htm")
                           ),
                       tags$li(tags$a("Advisor Smith City Cost of Living Index", href = "https://advisorsmith.com/data/coli/")),
                       ) # close ul tag
                        
                   ) # close div tag
               
           ) # close about TabPanel
          
    
) # end navbarPage

