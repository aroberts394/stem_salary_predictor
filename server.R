#
# This is the server logic of a Shiny web application. 
#

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
        pal <- colorQuantile("YlOrRd", domain = states_sf$cost_index)
        
        m <- leaflet(states_sf) %>%
            setView(-96, 37.8, 4) %>%
            addTiles()
        
        m <- m %>% addPolygons(
            fillColor = ~pal(cost_index),
            weight = 2,
            opacity = 1,
            color = "white",
            dashArray = "3",
            fillOpacity = 0.7,
            highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE)
        )
        
        m
    })
    
})
