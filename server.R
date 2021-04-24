# ======================= #
# Author: Tony Roberts
# Course: PUBH 7462
# Project: STEM Professional Salary Predictor
# Description: Shiny app server function for STEM salary predictor
# ======================= #

library(shiny)
library(leaflet)

# Define server logic 
shinyServer(function(input, output) {
    
    # get all user inputs
    user_inputs <- reactive({
        Age <- as.integer(input$age)
        Gender <- as.character(input$gender)
        Race <- as.character(input$race)
        Children <- as.character(input$children)
        Hours <- as.character(input$workhours)
        EmpSize <- as.character(input$empsize)
        EmpSector <- as.character(input$empsec)
        WorkRelated <- as.character(input$work_related)
        Profession <- as.character(input$profession)
        Years <- as.character(input$years)
        
        # combine inputs into table
        input_table <- cbind.data.frame(Age, Gender, Race, Children, Hours, EmpSize, EmpSector, 
                                        WorkRelated, Profession, Years)
        input_table <- as.data.frame(input_table)
       
    }) # end user input table
    
    # render table based on user inputs
    output$inputs <- renderTable({
       user_inputs()
    }) # end render table
    
    # create a dataframe of inputs for prediction
    data <- reactive({
        data.frame(AGE = input$age,
                   GENDER = input$gender,
                   RACETH = input$race,
                   CHTOT = input$children,
                   HRSWKGR = input$workhours,
                   EMSIZE = input$empsize,
                   EMSEC = input$empsec,
                   OCEDRLP = input$work_related,
                   NOCPRMG = input$profession,
                   YEARS_SINCE_GRAD = input$years
                   )
    }) #  end create  data frame
    
    # make predictions
    pred1 <- eventReactive(input$predict, {
        p <- round(as.numeric(predict(lm_simple_fit, new_data = data(), type = "conf_int")), 2)
        p[1]
    }) # end predictions
    
    # make predictions
    pred2 <- eventReactive(input$predict, {
        p <- round(as.numeric(predict(lm_simple_fit, new_data = data(), type = "conf_int")), 2)
        p[2]
    }) # end predictions
    
    # render predicted salary text
    output$salary <- renderText(
        paste('Based on your inputs, the predicted salary range is: $', pred1(),' - $', pred2(), sep = '')
        ) # end render predicted salary text

    # render leaflet map
    output$usmap <- renderLeaflet({
        pal <- colorQuantile("YlOrRd", domain = states_merge$cost_index)
        
        m <- 
            leaflet() %>%
            addProviderTiles("CartoDB.Positron") %>%
            setView(-96, 37.8, 4)

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
    }) # end leaflet map plot
    
    # render highered raw data table
    output$table <- DT::renderDT({
        highered_simple %>%
            DT::datatable(options = list(
                scrollX = TRUE,
                paginate = T
            ))
    }) # close renderDT
    
    # allow download of data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("higherED-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(highered_simple, file)
        }
    ) # close downloadHandler
    
}) # close shinyServer
