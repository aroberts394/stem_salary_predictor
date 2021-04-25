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
        bins <- c(0, 30, 60, 80, 90, 100, 120, 140, 150, 160)
        pal <- colorBin("YlOrRd", domain = states_merge$cost_index, bins = bins)
        
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
        m %>% addLegend(pal = pal, values = -states_merge$cost_index, opacity = 0.7, title = NULL,
                        position = "bottomright")
    }) # end leaflet map plot
    
    
    # calculate overall mean cost of living index
    output$meancoli <- renderText(
        paste("The average cost of living index is: ", round(mean_index,2), sep = "")
    )

    # calculate overall median cost of living index
    output$mediancoli <- renderText(
        paste("The median cost of living index is: ", round(median_index, 2), sep = "")
    )
    
    
    # get new dataset sample for plotting
    idx <- reactive({
        if (input$sampleType == "first") {
            1:input$sampleSize
        } else {
            set.seed(input$sampleSeed)
            sample(nrow(raw_df), input$sampleSize)
        }
    })
    df <- reactive(raw_df[idx(), , drop = FALSE])
    
    # Get head of selected data
    output$snippet <- renderPrint({
        head(df(), n = 15)
    })
    
    # get plot type
    # * 2: both numeric variables
    # * 1: one numeric, one non-numeric variable
    # * 0: both non-numeric variables
    # * -1: only one variable provided
    plot_type <- reactive({
        if (input$y != "None")
            is.numeric(raw_df[[input$x]]) + is.numeric(raw_df[[input$y]])
        else
            -1
    })
    
    # Create EDA plot
    output$plot <- renderPlot({
        if (plot_type() == 2) {
            # both numeric variables: scatterplot
            # also allow for color, jitter & smoothing
            p <- ggplot(df(), aes_string(x = input$x, y = input$y))
            
            if (input$jitter)
                p <- p + geom_jitter(alpha = 0.5)
            else
                p <- p + geom_point(alpha = 0.5)
            
            if (input$smooth)
                p <- p + geom_smooth()
            
            # color change
            if (input$color != "None")
                p <- p + aes_string(color = input$color)
        } else if (plot_type() == 1) {
            # one numeric var, one character var: boxplot
            # allow color, don't allow jitter or smoothing
            p <- p <- ggplot(df(), aes_string(x = input$x, y = input$y)) + 
                geom_boxplot()
            
            # fill change
            if (input$color != "None")
                p <- p + aes_string(fill = input$color)
        } else if (plot_type() == 0) {
            # two character variables: heatmap
            # don't allow color, jitter or smoothing
            temp_df <- reactive(df()[, c(input$x, input$y), drop = FALSE] %>%
                                    group_by(across()) %>%
                                    summarize(count = n())
            )
            p <- ggplot(temp_df(), 
                        mapping = aes_string(x = input$x, y = input$y, fill = "count")) +
                geom_tile() +
                scale_fill_gradient(low = "#e7e7fd", high = "#1111dd")
        } else {
            # only one variable: univariate plot
            # allow color, don't allow jitter or smoothing
            p <- ggplot(df(), aes_string(x = input$x))
            
            if (is.numeric(raw_df[[input$x]]))
                p <- p + geom_histogram()
            else
                p <- p + geom_bar()
            
            # fill change
            if (input$color != "None")
                p <- p + aes_string(fill = input$color)
        }
        
        # add title
        if (plot_type() >= 0) {
            p <- p + labs(title = paste(input$y, "vs.", input$x))
        } else {
            p <- p + labs(title = paste("Distribution of", input$x))
        }
        
        # add styling
        p <- p + 
            theme_bw() +
            theme(plot.title = element_text(size = rel(1.8), face = "bold", hjust = 0.5),
                  axis.title = element_text(size = rel(1.5)),
                  axis.text = element_text(size = rel(1.2)),
                  legend.text = element_text(size = rel(1.2)))
        
        print(p)
        
    }, height=750)

    
    
    # render highered raw data table
    output$table <- DT::renderDT({
        raw_df %>%
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
