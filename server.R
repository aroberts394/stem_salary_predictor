# ======================= #
# Author: Tony Roberts
# Course: PUBH 7462
# Project: STEM Professional Salary Predictor
# Description: Shiny app server function for STEM salary predictor
# ======================= #

library(shiny)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    user_inputs <- reactive({
        # get user inputs
        Age <- as.integer(input$age)
        Gender <- as.character(input$gender)
        Children <- as.integer(input$children)
        Degree <- as.character(input$degree)
        Retired <- as.character(input$retired)
        Profession <- as.character(input$job_code)
        
        # combine inputs into table
        input_table <- cbind.data.frame(Age, Gender, Children, Degree, Retired, Profession)
        input_table <- as.data.frame(input_table)
       
    })

    output$table <- renderTable({
       user_inputs()
    })
    
    output$usmap <- renderLeaflet({
        pal <- colorQuantile("YlOrRd", domain = states_merge$cost_index)
        
        m <- 
            # leaflet(states_sf) %>%
            leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-96, 37.8, 4)
            # addTiles()
        
        labels <- sprintf(
            "<strong>%s</strong><br/> COLI index: %g", states_merge$NAME, states_merge$cost_index) %>% 
            lapply(htmltools::HTML)
        
        m <- m %>% 
            addPolygons(
            data = states_merge,
            fillColor = ~pal(states_merge$cost_index),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 5,
                color = "#667",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
            label = labels,
            labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")
        )
        
        m
    })
    
})
