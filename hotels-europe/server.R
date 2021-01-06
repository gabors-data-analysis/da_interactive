########################################################################
#
# CEU "CEU DA Interactive Visualization
# Server functions DEV
# Benedek PASZTOR 2021-01-04 - 
#
########################################################################

library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(skimr)
library(ggpubr)


## Theme Config
ggplot_theme <- theme_classic


shinyServer(function(input, output) {
    data <- read.csv(paste0('data/hotels.csv')) %>% 
        mutate(date = as.factor(strptime(paste0(year, '-', month, '-', 1), "%Y-%m-%d")))
    
    ## DESCRIBE DATA FOR A CITY
    ### Pick a city
    output$desc_sel_city <- renderUI({
        selectInput(inputId = "desc_sel_city",
                    label = "Select a city:",
                    choices = unique(data$city),
                    multiple = FALSE,
                    selectize = TRUE,
                    selected = "Amsterdam")
    })
    
    ### Pick a date
    output$desc_sel_date <- renderUI({
            selectInput(inputId = "desc_sel_date",
                        label = "Select a date:",
                        choices = unique(data$date),
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = c("2017-12-01"))
    })


    ### Pick three variables (default: price, distance, stars)
    output$desc_sel_three_variables <-renderUI({
        selectInput(inputId = "desc_sel_three_variables",
                    label = "Select variables:",
                    choices = names(data),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("price", "distance", "stars"))
    })
    

    ### Filter extreme values
    #### TODO:
    
    #### Show a histogram
    output$desc_histogram <- renderPlotly({
        if(is.null(input$desc_sel_three_variables)){return()}
        
        data_ <- data %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(input$desc_sel_three_variables) %>% 
            pivot_longer(input$desc_sel_three_variables)
        
        p <- ggplot(data_,
               aes(x = value)) +
            geom_histogram(position = "dodge") +
            facet_grid(. ~ name, scales = "free") +
            xlab("\n \n Value bins") +
            ylab("Pieces of bookings") +
            theme(axis.title.y = element_text(hjust=-2.5)) +
            ggplot_theme()
            
        return(ggplotly(p))
    })
    
    #### Show basic descriptive stats table:  (mean, sd, median, min, max, p5, p95)
    output$desc_summary <- renderPrint({
        data_ <- data %>% 
            select(input$desc_sel_three_variables)
        
        skimr::skim_without_charts(data_)
    })
    
    #########################################################################################
    ## COMPARE TWO CITIES
    ### Pick a city
    output$comp_sel_cities <- renderUI({
        selectInput(inputId = "comp_sel_cities",
                    label = "Select two cities:",
                    choices = unique(data$city),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("Amsterdam", "Budapest"))
    })
    
    ### Pick a date
    output$comp_sel_date <- renderUI({
            selectInput(inputId = "comp_sel_date",
                        label = "Select a date:",
                        choices = unique(data$date),
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = c("2017-12-01"))
    })


    ### Pick three variables (default: price, distance, stars)
    output$comp_sel_three_variables <-renderUI({
        selectInput(inputId = "comp_sel_three_variables",
                    label = "Select variables:",
                    choices = names(data),
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("price", "distance", "stars"))
    })

    
    ### Filter extreme values
    #### TODO:
    
    
    
    #### Density kernel graph
    output$comp_density <- renderPlotly({
        if(is.null(input$comp_sel_three_variables) || is.null(input$comp_sel_cities) || is.null(input$comp_sel_date)){return()}
       
        data_ <- data %>% 
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>% 
            select(c("city", input$comp_sel_three_variables)) %>% 
            pivot_longer(input$comp_sel_three_variables)
        
        p <- ggplot(data_,
               aes(x = value,
                   color = city,
                   fill = city)) +
            geom_density(position = "dodge",
                         na.rm = T,
                         alpha = 0.2) +
            facet_grid(. ~ name, scales = "free") +
            xlab("\n \n Value range") +
            ylab("Density [-]") +
            theme(axis.title.y = element_text(hjust=-2.5)) +
            ggplot_theme()
            
        return(ggplotly(p))
    })
    
    #### Show basic descriptive stats table:  (mean, sd, median, min, max, p5, p95)
    output$comp_summary <- renderPrint({
        data %>% 
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>% 
            select(c("city", input$desc_sel_three_variables)) %>% 
            group_by(city) %>% 
            skimr::skim_without_charts()
        
    })
    
    #########################################################################################
    
    #### SCATTERPLOTS & CORRELATION
    
    ### Pick a city
    output$corr_sel_city <- renderUI({
        selectInput(inputId = "corr_sel_city",
                    label = "Select two cities:",
                    choices = unique(data$city),
                    multiple = FALSE,
                    selectize = TRUE,
                    selected = "Amsterdam")
    })
    
    ### Pick a date
    output$corr_sel_date <- renderUI({
            selectInput(inputId = "corr_sel_date",
                        label = "Select a date:",
                        choices = unique(data$date),
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = c("2017-12-01"))
    })


    
    ### Filter extreme values
    #### TODO:
    
    
    ### X selector
    output$corr_sel_x <- renderUI({
        selectInput(inputId = "corr_sel_x",
                    label = strong("X axis:"), 
                    choices = names(data),
                    selected = "distance")
    })
    
    
    output$corr_sel_y <- renderUI({
        selectInput(inputId = "corr_sel_y",
                    label = strong("Y axis:"), 
                    choices = names(data),
                    selected = "price")
    })
    
    
    
    output$corr_trendline_type <- renderUI({
        selectInput(inputId = "corr_trendline_type",
                    label = strong("Select trendline type:"),
                    choices = list("Linear" = "linear",
                                   "Lowess" = "lowess",
                                   "Quadratic" = "quadratic",
                                   "Cubic" = "cubic"),
                    selected = "linear"
                    )
    })
   
    output$corr_sel_x_ff <- renderUI({
        radioButtons(inputId = "corr_sel_x_ff",
                     label = "Log transformation of X axis:",
                     choices = c("Yes", "No"),
                     selected = "No")
    }) 
    
    output$corr_sel_y_ff <- renderUI({
        radioButtons(inputId = "corr_sel_y_ff",
                     label = "Log transformation of Y axis:",
                     choices = c("Yes", "No"),
                     selected = "No")
    })
    
    
    ### Plots
    output$corr_scatterplot <- renderPlotly({
        if(is.null(input$corr_sel_x) || is.null(input$corr_sel_y)){return()}
        
        data_ <- data %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_x, input$corr_sel_y)
       
        data_[input$corr_sel_x] <- ifelse(input$corr_sel_x_ff == "Yes", log(data_[input$corr_sel_x]), data[input$corr_sel_x])
        data_[input$corr_sel_y] <- ifelse(input$corr_sel_y_ff == "Yes", log(data_[input$corr_sel_y]), data[input$corr_sel_y])
        
        
        if(input$corr_trendline_type == 'linear')
            {trendline_type <- geom_smooth(method = "lm", formula = y ~ x)}
        if(input$corr_trendline_type == 'lowess')
          {trendline_type <- geom_smooth(method = "loess")}
        if(input$corr_trendline_type == 'quadratic')
            {trendline_type <- geom_smooth(method = "lm", formula = y ~ x + I(x^2))}
        if(input$corr_trendline_type == 'cubic')
            {trendline_type <- geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3))}

        x <- input$corr_sel_x
        y <- input$corr_sel_y

        p <- ggplot(data_, 
                    aes(get(x), 
                        get(y))) +
            geom_point(alpha = 0.3) +
            labs(x = x, y = y) + 
            trendline_type +
            ggplot_theme()
        
        
        return(ggplotly(p))
    })
    
    output$corr_corr <- renderPrint({
        if(!is.numeric(data[[input$corr_sel_x]]) || !is.numeric(data[[input$corr_sel_y]])){return("Correlation only makes sense with numeric variables")}
        
        data_ <- data %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_x, input$corr_sel_y)
        
        cor(data_[[input$corr_sel_x]], data_[[input$corr_sel_y]])
        
        })
        
})
