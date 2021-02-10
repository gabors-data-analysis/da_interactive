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
library(cowplot)
library(modelsummary)
library(stringr)
library(shinyWidgets)


mean_na <- function(x) mean(x, na.rm = TRUE)
max_na <- function(x) max(x, na.rm = TRUE)
min_na <- function(x) min(x, na.rm = TRUE)
sd_na <- function(x) sd(x, na.rm = TRUE)


roundUp <- function(x,m) m*ceiling(x / m)

my.order <- c('integer', 'factor', 'character')


## Theme Config
source("theme_bg.R")

theme_gg <- theme_bg

`%notin%` <- function(x,y)!('%in%'(x,y))

shinyServer(function(input, output) {
    data <- read.csv(paste0('data/hotels.csv')) %>% 
        mutate(date = as.factor(strptime(paste0(year, '-', month, '-', 1), "%Y-%m-%d"))) %>%
        mutate(weekend = as.factor(weekend),
               holiday = as.factor(holiday),
               scarce_room = as.factor(scarce_room),
               offer = as.factor(offer)) %>%
        select(-center1label, -center2label) %>% 
        select(sapply(., class) %>% .[order(match(., my.order))] %>% names) 
    
    data_reg <- data %>% mutate(ln_price = log(price),
                                ln_distance = log(distance),
                                ln_rating_reviewcount = log(rating_reviewcount),
                                ln_ratingta_count = log(ratingta_count))
        
    

    
    ##################### DESCRIBE DATA FOR A CITY ############################
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
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
        
        selectInput(inputId = "desc_sel_three_variables",
                    label = "Select numeric variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("price", "distance", "stars", "offer_cat"))
    })
    
    
    ### Pick factor variables (default: price, distance, stars)
    output$desc_sel_three_variables_factor <-renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, F, T)], c("date"))
        
        selectInput(inputId = "desc_sel_three_variables_factor",
                    label = "Select factor variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("offer_cat"))
    })

    ### Filter extreme values
    output$desc_filter_check <- renderUI({
        checkboxInput("desc_filter_check",
                      strong("Tick this if you would like to filter values"), 
                      FALSE)
    })
    
    
    # Render the sliders
    output$desc_filters <- renderUI({

        data_ <- data %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(c(input$desc_sel_three_variables, input$desc_sel_three_variables_factor))
        
        xAxisGroup <- unique(names(data_))
        
        # First, create a list of sliders each with a different name
        filtering_function <- function(i) {
            inputName <- paste0("desc_filter_", xAxisGroup[i])
            column_name <- xAxisGroup[i]
            
            if(class(data[[column_name]]) != 'factor'){
                inputpanel <- sliderInput(inputName,
                                          xAxisGroup[i],
                                          min=min(data_[column_name], na.rm = T),
                                          max=max(data_[column_name], na.rm = T),
                                          value=c(min(data_[column_name], na.rm = T), max(data_[column_name], na.rm = T)))
            }
            else{
                inputpanel <- selectInput(inputName,
                            label = xAxisGroup[i],
                            choices = unique(as.character(data_[[column_name]])),
                            multiple = TRUE,
                            selectize = TRUE,
                            selected = unique(as.character(data_[[column_name]])))
            }
            
            return(conditionalPanel(condition = "input.desc_filter_check == 1", inputpanel))
        }
        
        sliders <- lapply(1:length(xAxisGroup), filtering_function)
        
        # Create a tagList of sliders (this is important)
        do.call(tagList, sliders)
    })
    
    desc_reactive_table <- reactive({
        req(input$desc_sel_date)
        ### Filtering
        data_ <- data
        
        if(input$desc_filter_check == TRUE){
            for(i in input$desc_sel_three_variables){
                filter_obj <- eval(parse(text = paste0("input$desc_filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) >= filter_obj[1] & get(i) <= filter_obj[2])
            }
            for(i in input$desc_sel_three_variables_factor){
                filter_obj <- eval(parse(text = paste0("input$desc_filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) %in% filter_obj)
            }
        }
        ## Filtering ends
        
        data_
    })

    output$desc_histogram <- renderPlot({
        req(desc_reactive_table())

        data_ <- desc_reactive_table()
        
        
        # input$desc_sel_city <- 'Amsterdam'
        # input$desc_sel_date <- '2017-11-01'
        # thisID <- 'price'
        # input$desc_sel_three_variables <- c('price', 'distance')
        # input$desc_sel_three_variables_factor <- c()
        # data_ <- data
        #         
        data_c <- data %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(c(c("city"), input$desc_sel_three_variables, input$desc_sel_three_variables_factor)) %>% 
            drop_na() %>% 
            filter_all(all_vars(!is.infinite(.)))
        
        
        data_ <- data_ %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(c(c("city"), input$desc_sel_three_variables, input$desc_sel_three_variables_factor)) %>% 
            drop_na() %>% 
            filter_all(all_vars(!is.infinite(.)))
        
        

        plotting_function <- function(thisID){
            data__ <- data_ 
            
            stars_like_treatment <- unique(c(c('stars', 'nnights', 'ratingta'), names(data[sapply(data, function(x) length(unique(x))< 4)])))
            
            if(class(data_[[thisID]]) == 'factor' | thisID %in% stars_like_treatment){
                color_vector <- ifelse(thisID %in% c('stars', 'nnights', 'ratingta'), c(color[1], color[2], color[3]), c(color[2], color[3], color[4]))
                
                p <- data__ %>% 
                    ggplot(aes(x = get(thisID),
                               fill = city,
                               color = city)) +
                    geom_bar(aes(y = (..count..)/sum(..count..))) + 
                    coord_flip() +
                    scale_fill_manual(values=color_vector) +
                    scale_color_manual(values = color.outline) +
                    ggtitle(paste("Variable name:  ", thisID)) +
                    scale_y_continuous("Percentage", labels = scales::percent_format(accuracy = 1L)) +
                    xlab("") + 
                    theme_gg() +
                    theme(legend.position = "none")
            }
            
            else{
                condition_ <- max(data_c %>% select(thisID)) > 500
                bins <- 20
                is_it_rating <- thisID == 'rating'
                
                p <- data__ %>% 
                    ggplot(aes(x = get(thisID),
                               y = (..count..)/sum(..count..),
                               fill = city,
                               color = city)) +
                    geom_histogram(bins = bins, 
                                   binwidth = ifelse(is_it_rating,
                                                     0.5,
                                                     roundUp((max(data_c %>% select(thisID))), ifelse(condition_, 1000, bins)) / bins),
                                   boundary = 0, 
                                   alpha = 1, 
                                   show.legend = T, 
                                   closed = 'left') +
                    scale_fill_manual(values=c(color[1], color[2], color[3])) +
                    scale_color_manual(values = color.outline) +
                    scale_y_continuous("Percentage", labels = scales::percent_format(accuracy = 1L)) +
                    xlab("\n \n Value bin") +
                    ggtitle(paste("Variable name:  ", thisID)) +
                    scale_x_continuous(limits = c(0,
                                                  ifelse(is_it_rating,
                                                         5,
                                                         roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10) + ifelse(condition_, 1000, 10) / bins))),
                                       breaks = seq(from = ifelse(is_it_rating,
                                                                  1,
                                                                  0),
                                                    to = ifelse(is_it_rating,
                                                                5,
                                                                roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10)) + ifelse(condition_, 1000, 10) / bins),
                                                    by = ifelse(is_it_rating,
                                                                0.5,
                                                                roundUp((max(data_c %>% select(thisID)) - min(data_c %>% select(thisID))), ifelse(condition_, 1000, 10) / bins))),
                                       expand = c(0, 0)) +
                    theme_gg() +
                    theme(legend.position = "none") +
                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
                
                # p

                q <- ggplot_build(p) 
                
                
                q$layout$panel_params[[1]]$x$breaks <- round(q$data[[1]][['xmin']][c(T, T)], digits = ifelse(mean(q$layout$panel_params[[1]]$x$breaks) > 100, 0, 
                                                                                                                 ifelse(mean(q$layout$panel_params[[1]]$x$breaks) > 10, 1, 2)))
                q$layout$panel_params[[1]]$x$breaks   
                # q
                
                p <- ggplot_gtable(q)
                
                # p
                }
                
            return(p)
        }
        

        myPlots <- lapply(c(input$desc_sel_three_variables, input$desc_sel_three_variables_factor), plotting_function)
        

        p <- plot_grid(plotlist = myPlots, nrow = 1)
        
        return(p)
    })
    
    #### Show basic descriptive stats table:  (mean, sd, median, min, max, p5, p95)
    output$desc_summary <- renderTable({
        req(desc_reactive_table())
        data_ <- desc_reactive_table()
        
        ## Filtering ends
        data_ <- data_ %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>%
            select(input$desc_sel_three_variables) 
        
        ds <- datasummary(as.formula(paste0(str_c(eval(paste0('data_$', names(data_))), collapse = "+"), "~ mean_na + sd_na + min_na + max_na")),   data_, output = 'data.frame')
        
        ds[[1]] <- NULL
        row.names(ds) <- names(data_)
        stats <- names(ds)
        b <- as.data.frame(t(ds)) %>% 
            mutate_all(as.character) %>% 
            mutate_all(as.numeric) %>% 
            mutate_all(function(x) round(x, digits = ifelse(x > 100, 0, ifelse(x > 10, 1, 2)))) %>% 
            mutate(stats = stats) %>% 
            select(stats, everything())
    
        b
    })
    
    output$desc_summary_factor <- renderTable({
        req(desc_reactive_table())
        data_ <- desc_reactive_table()
        
        ## Filtering ends
        data_ <- data_ %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(input$desc_sel_three_variables_factor) %>% 
            skim()
        
        for(i in 1:length(names(data_))){
            splitted <- strsplit(names(data_)[i], "[.]")
            names(data_)[i] <- splitted[[1]][length(splitted[[1]])]
        }
        rownames <- data_['skim_variable'][[1]]
        
        data_ <- data_ %>% select(-skim_type, -skim_variable, -n_missing, -complete_rate, -ordered)
        row.names(data_) <- rownames
        
        as.data.frame(t(data_)) %>%  
            mutate(statistic = row.names(.)) %>% 
            select(statistic, everything())
        
    })
    
    
    ########################### COMPARE TWO CITIES #########################################
    output$comp_sel_cities <- renderUI({
        selectizeInput(inputId = "comp_sel_cities",
                    label = "Select two cities:",
                    choices = unique(data$city),
                    multiple = TRUE,
                    selected = c("Amsterdam", "Budapest"),
                    options = list(maxItems = 2))
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
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, T, F)], c("month", "year"))
        
        selectInput(inputId = "comp_sel_three_variables",
                    label = "Select numeric variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("price", "distance", "stars", "offer_cat"))
    })
    
    
    ### Pick factor variables (default: price, distance, stars)
    output$comp_sel_three_variables_factor <-renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, F, T)], c("date"))
        
        selectInput(inputId = "comp_sel_three_variables_factor",
                    label = "Select factor variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    selectize = TRUE,
                    selected = c("offer_cat"))
    })
    
    ### Filter extreme values
    output$comp_filter_check <- renderUI({
        checkboxInput("comp_filter_check",
                      strong("Tick this if you would like to filter values"), 
                      FALSE)
    })
    
    
    # Render the sliders
    output$comp_filters <- renderUI({
        data_ <- data %>% 
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>% 
            select(c(input$comp_sel_three_variables, input$comp_sel_three_variables_factor))
        
        xAxisGroup <- unique(names(data_))
        
        # First, create a list of sliders each with a different name
        filtering_function <- function(i) {
            inputName <- paste0("comp_filter_", xAxisGroup[i])
            column_name <- xAxisGroup[i]
            
            if(class(data[[column_name]]) != 'factor'){
                inputpanel <- sliderInput(inputName,
                                          xAxisGroup[i],
                                          min=min(data_[column_name], na.rm = T),
                                          max=max(data_[column_name], na.rm = T),
                                          value=c(min(data_[column_name], na.rm = T), max(data_[column_name], na.rm = T)))
            }
            else{
                inputpanel <- selectInput(inputName,
                                          label = xAxisGroup[i],
                                          choices = unique(as.character(data_[[column_name]])),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = unique(as.character(data_[[column_name]])))
            }
            
            return(conditionalPanel(condition = "input.comp_filter_check == 1", inputpanel))
        }
        
        sliders <- lapply(1:length(xAxisGroup), filtering_function)
        
        
        
        # Create a tagList of sliders (this is important)
        do.call(tagList, sliders)
    })
    
    
    comp_reactive_table <- reactive({
        req(input$comp_sel_date)
        ### Filtering
        data_ <- data
        
        if(input$comp_filter_check == TRUE){
            for(i in input$comp_sel_three_variables){
                filter_obj <- eval(parse(text = paste0("input$comp_filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) >= filter_obj[1] & get(i) <= filter_obj[2])
            }
            for(i in input$comp_sel_three_variables_factor){
                filter_obj <- eval(parse(text = paste0("input$comp_filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) %in% filter_obj)
            }
        }
        ## Filtering ends
        
        data_
    })
    
    output$comp_histogram <- renderPlot({
        req(comp_reactive_table)
   
        data_ <- comp_reactive_table()


        data_c <- data %>%
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>%
            select(c(c("city"), input$comp_sel_three_variables, input$comp_sel_three_variables_factor)) %>%
            drop_na() %>%
            filter_all(all_vars(!is.infinite(.)))


        data_ <- data_ %>%
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>%
            select(c(c("city"), input$comp_sel_three_variables, input$comp_sel_three_variables_factor)) %>%
            drop_na() %>%
            filter_all(all_vars(!is.infinite(.)))


        plotting_function <- function(thisID, index){
            data__ <- data_ %>%
                # select(city, thisID) %>%
                drop_na()

            show_legend <- ifelse(index == length(c(input$comp_sel_three_variables, input$comp_sel_three_variables_factor)),
                                  T,
                                  F)
            
            stars_like_treatment <- unique(c(c('stars', 'nnights'), names(data[sapply(data, function(x) length(unique(x))< 4)])))

            if(class(data_[[thisID]]) == 'factor' | thisID %in% stars_like_treatment){
                p <- ggplot(data__, 
                            aes(x = get(thisID),
                               fill = city,
                               color = city)) +
                    geom_bar(data = subset(data__, city == input$comp_sel_cities[1]),
                             aes(y = (..count..)/sum(..count..)),
                             position = 'identity',
                             alpha = 0.3,
                             size = 1.05,
                             show.legend = show_legend) +
                    geom_bar(data = subset(data__, city == input$comp_sel_cities[2]),
                             aes(y = (..count..)/sum(..count..)),
                             position = 'identity',
                             alpha = 0.3,
                             size = 0.95,
                             show.legend = show_legend) +
                    coord_flip() +
                    scale_fill_manual(values=c(color[1], color[2], color[3])) +
                    scale_color_manual(values = c(color[1], color[2], color[3])) +
                    ggtitle(paste("Variable name:  ", thisID)) +
                    scale_y_continuous("Percentage", labels = scales::percent_format(accuracy = 1L)) +
                    xlab("") +
                    theme_gg() +
                    theme(legend.position = "right",
                          legend.text = element_text(size = 18),
                          legend.title = element_blank())
            }

            else{
                condition_ <- max(data_c %>% select(thisID)) > 1000
                bins <- 20
                is_it_rating <- thisID == 'rating'
                
                p <- ggplot(data__, aes(x = get(thisID),
                                y =  (..count..)/sum(..count..),
                                fill = city,
                                color = city)) +
                    geom_histogram(data = subset(data__, city == input$comp_sel_cities[1]),
                                   bins = 20,
                                   binwidth = ifelse(is_it_rating,
                                                      0.5,
                                                      roundUp((max(data_c %>% select(thisID))), ifelse(condition_, 1000, bins)) / bins),
                                   position = 'identity',  
                                   boundary = 0, 
                                   alpha = 0.3,
                                   show.legend = show_legend,
                                   size = 1.05,
                                   closed = 'left') +
                    
                    geom_histogram(data = subset(data__, city == input$comp_sel_cities[2]),
                                   bins = 20,
                                   size = 0.95,
                                   binwidth = ifelse(is_it_rating,
                                                     0.5,
                                                     roundUp((max(data_c %>% select(thisID))), ifelse(condition_, 1000, bins)) / bins),
                                   position = 'identity',  
                                   boundary = 0, 
                                   alpha = 0.3,
                                   show.legend = show_legend,
                                   closed = 'left') +
                    scale_fill_manual(values=c(color[1], color[2], color[3])) +
                    scale_color_manual(values = c(color[1], color[2], color[3])) +
                    scale_y_continuous("Percentage", labels = scales::percent_format(accuracy = 1L)) +
                    xlab("\n \n Value bin") +
                    ggtitle(paste("Variable name:  ", thisID)) +
                    scale_x_continuous(limits = c(0,
                                                  ifelse(is_it_rating,
                                                         5,
                                                         roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10) + ifelse(condition_, 1000, 10) / bins))),
                                       breaks = seq(from = ifelse(is_it_rating,
                                                                  1,
                                                                  0),
                                                    to = ifelse(is_it_rating,
                                                                5,
                                                                roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10)) + ifelse(condition_, 1000, 10) / bins),
                                                    by = ifelse(is_it_rating,
                                                                0.5,
                                                                roundUp((max(data_c %>% select(thisID)) - min(data_c %>% select(thisID))), ifelse(condition_, 1000, 10) / bins))),
                                       expand = c(0, 0)) +            theme_bg() +
                    # theme(legend.position = "none") +
                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

                q <- ggplot_build(p)

                q$data[[1]] <- q$data[[1]] %>% mutate_all(~replace(., is.na(.), 0))
                # q$data[[1]] <- q$data[[1]][order(q$data[[1]]$xmin),]
                # q$data[[1]]$xmin <- replace(sort(q$data[[1]]$xmin, na.last = T), 1, 0)
                q$layout$panel_params[[1]]$x$breaks <- sort(q$data[[1]]$xmin)[c(T, F)]
                q$layout$panel_params[[1]]$x$breaks <- replace(q$layout$panel_params[[1]]$x$breaks, 1, 0)
                q$layout$panel_params[[1]]$x$breaks <- round(q$layout$panel_params[[1]]$x$breaks, digits = ifelse(mean(q$layout$panel_params[[1]]$x$breaks) > 100, 0,
                                                                                                                ifelse(mean(q$layout$panel_params[[1]]$x$breaks) > 10, 1,
                                                                                                                       ifelse(q$layout$panel_params[[1]]$x$breaks == 0, 0, 2))))


                p <- ggplot_gtable(q)
            }

            return(p)
        }


        myPlots <- mapply(plotting_function, c(input$comp_sel_three_variables, input$comp_sel_three_variables_factor),
                          seq_along(c(input$comp_sel_three_variables, input$comp_sel_three_variables_factor)))


        p <- plot_grid(plotlist = myPlots, nrow = 1)

        return(p)
    })
    # 
    output$comp_city_1 <- renderText({input$comp_sel_cities[1]})
    output$comp_city_2 <- renderText({input$comp_sel_cities[2]})
    
    
    output$comp_summary_1 <- renderTable({
        req(comp_reactive_table())
        data_ <- comp_reactive_table()
        
        ## Filtering ends
        data_ <- data_ %>% 
            filter(city == input$comp_sel_cities[1] & date == input$comp_sel_date) %>%
            select(input$comp_sel_three_variables) 
        
        ds <- datasummary(as.formula(paste0(str_c(eval(paste0('data_$', names(data_))), collapse = "+"), "~ mean_na + sd_na + min_na + max_na")),   data_, output = 'data.frame')
        
        ds[[1]] <- NULL
        row.names(ds) <- names(data_)
        stats <- names(ds)
        b <- as.data.frame(t(ds)) %>% 
            mutate_all(as.character) %>% 
            mutate_all(as.numeric) %>% 
            mutate_all(function(x) round(x, digits = ifelse(x > 100, 0, ifelse(x > 10, 1, 2)))) %>% 
            mutate(stats = stats) %>% 
            select(stats, everything())
        
        b
    })
    
    output$comp_summary_factor_1 <- renderTable({
        req(comp_reactive_table())
        data_ <- comp_reactive_table()
        
        ## Filtering ends
        data_ <- data_ %>% 
            filter(city == input$comp_sel_cities[1] & date == input$comp_sel_date) %>% 
            select(input$comp_sel_three_variables_factor) %>% 
            skim()
        
        for(i in 1:length(names(data_))){
            splitted <- strsplit(names(data_)[i], "[.]")
            names(data_)[i] <- splitted[[1]][length(splitted[[1]])]
        }
        rownames <- data_['skim_variable'][[1]]
        
        data_ <- data_ %>% select(-skim_type, -skim_variable, -n_missing, -complete_rate, -ordered)
        row.names(data_) <- rownames
        
        as.data.frame(t(data_)) %>%  
            mutate(statistic = row.names(.)) %>% 
            select(statistic, everything())
    })
    
    output$comp_summary_2 <- renderTable({
        req(comp_reactive_table())
        data_ <- comp_reactive_table()
        
        ## Filtering ends
        data_ <- data_ %>% 
            filter(city == input$comp_sel_cities[2] & date == input$comp_sel_date) %>%
            select(input$comp_sel_three_variables) 
        
        ds <- datasummary(as.formula(paste0(str_c(eval(paste0('data_$', names(data_))), collapse = "+"), "~ mean_na + sd_na + min_na + max_na")),   data_, output = 'data.frame')
        
        ds[[1]] <- NULL
        row.names(ds) <- names(data_)
        stats <- names(ds)
        b <- as.data.frame(t(ds)) %>% 
            mutate_all(as.character) %>% 
            mutate_all(as.numeric) %>% 
            mutate_all(function(x) round(x, digits = ifelse(x > 100, 0, ifelse(x > 10, 1, 2)))) %>% 
            mutate(stats = stats) %>% 
            select(stats, everything())
        
        b
    })
    
    output$comp_summary_factor_2 <- renderTable({
        req(comp_reactive_table())
        data_ <- comp_reactive_table()
        
        ## Filtering ends
        data_ <- data_ %>% 
            filter(city == input$comp_sel_cities[2] & date == input$comp_sel_date) %>% 
            select(input$comp_sel_three_variables_factor) %>% 
            skim()
        
        for(i in 1:length(names(data_))){
            splitted <- strsplit(names(data_)[i], "[.]")
            names(data_)[i] <- splitted[[1]][length(splitted[[1]])]
        }
        rownames <- data_['skim_variable'][[1]]
        
        data_ <- data_ %>% select(-skim_type, -skim_variable, -n_missing, -complete_rate, -ordered)
        row.names(data_) <- rownames
        
        as.data.frame(t(data_)) %>%  
            mutate(statistic = row.names(.)) %>% 
            select(statistic, everything())
        
    })
    
    ########################### SCATTERPLOTS & CORRELATION #########################################
    #### SCATTERPLOTS & CORRELATION
    
    ### Pick a city
    output$corr_sel_city <- renderUI({
        selectInput(inputId = "corr_sel_city",
                    label = "Select a city:",
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


    
    # Render the sliders
    output$corr_filters <- renderUI({
        data_ <- data %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(c(input$comp_sel_three_variables, input$comp_sel_three_variables_factor))
        
        xAxisGroup <- unique(names(data_))
        
        # First, create a list of sliders each with a different name
        filtering_function <- function(i) {
            inputName <- paste0("corr_filter_", xAxisGroup[i])
            column_name <- xAxisGroup[i]
            
            if(class(data[[column_name]]) != 'factor'){
                inputpanel <- sliderInput(inputName,
                                          xAxisGroup[i],
                                          min=min(data_[column_name], na.rm = T),
                                          max=max(data_[column_name], na.rm = T),
                                          value=c(min(data_[column_name], na.rm = T), max(data_[column_name], na.rm = T)))
            }
            else{
                inputpanel <- selectInput(inputName,
                                          label = xAxisGroup[i],
                                          choices = unique(as.character(data_[[column_name]])),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = unique(as.character(data_[[column_name]])))
            }
            
            return(conditionalPanel(condition = "input.corr_filter_check == 1", inputpanel))
        }
        
        sliders <- lapply(1:length(xAxisGroup), filtering_function)
        
        
        
        # Create a tagList of sliders (this is important)
        do.call(tagList, sliders)
    })
    
    
    
    ### Filter extreme values
    output$corr_filter_check <- renderUI({
        checkboxInput("corr_filter_check",
                      strong("Tick this if you would like to filter values"), 
                      FALSE)
        })

    output$corr_sel_x_filter <- renderUI({
        data_ <- data %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_x, input$corr_sel_y)
        
        # data_[input$corr_sel_x] <- ifelse(input$corr_sel_x_ff == "Yes", log(data_[input$corr_sel_x]), data[input$corr_sel_x])
        # data_[input$corr_sel_y] <- ifelse(input$corr_sel_y_ff == "Yes", log(data_[input$corr_sel_y]), data[input$corr_sel_y])
        
        conditionalPanel(condition = "input.corr_filter_check == 1",                   
                         if(class(data[[input$corr_sel_x]]) != 'factor'){
                             inputpanel <- sliderInput('corr_sel_x_filter',
                                                       'X axis filter',
                                                       min=min(data_[input$corr_sel_x], na.rm = T),
                                                       max=max(data_[input$corr_sel_x], na.rm = T),
                                                       value=c(min(data_[input$corr_sel_x], na.rm = T), max(data_[input$corr_sel_x], na.rm = T)))
                         }
                         else{
                             inputpanel <- selectInput('corr_sel_x_filter',
                                                       label = 'X axis filter',
                                                       choices = unique(as.character(data_[[input$corr_sel_x]])),
                                                       multiple = TRUE,
                                                       selectize = TRUE,
                                                       selected = unique(as.character(data_[[input$corr_sel_x]])))
                         })
    })
    
    
    output$corr_sel_y_filter <- renderUI({
        data_ <- data %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_y, input$corr_sel_y)
        # 
        # data_[input$corr_sel_y] <- ifelse(input$corr_sel_y_ff == "Yes", log(data_[input$corr_sel_y]), data[input$corr_sel_y])
        # data_[input$corr_sel_y] <- ifelse(input$corr_sel_y_ff == "Yes", log(data_[input$corr_sel_y]), data[input$corr_sel_y])
        
        conditionalPanel(condition = "input.corr_filter_check == 1",                   
                         if(class(data[[input$corr_sel_y]]) != 'factor'){
                             inputpanel <- sliderInput('corr_sel_y_filter',
                                                       'Y axis filter',
                                                       min=min(data_[input$corr_sel_y], na.rm = T),
                                                       max=max(data_[input$corr_sel_y], na.rm = T),
                                                       value=c(min(data_[input$corr_sel_y], na.rm = T), max(data_[input$corr_sel_y], na.rm = T)))
                         }
                         else{
                             inputpanel <- selectInput('corr_sel_y_filter',
                                                       label = 'Y axis filter',
                                                       choices = unique(as.character(data_[[input$corr_sel_y]])),
                                                       multiple = TRUE,
                                                       selectize = TRUE,
                                                       selected = unique(as.character(data_[[input$corr_sel_y]])))
                         })
    })
     
    
    ### X selector
    output$corr_sel_x <- renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
        
        selectInput(inputId = "corr_sel_x",
                    label = strong("X axis:"), 
                    choices = selectable_names,
                    selected = "distance")
    })
    
    
    output$corr_sel_y <- renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
        
        selectInput(inputId = "corr_sel_y",
                    label = strong("Y axis:"), 
                    choices = selectable_names,
                    selected = "price")
    })
    
    
    
    output$corr_trendline_type <- renderUI({
        radioButtons(inputId = "corr_trendline_type",
                    label = strong("Select trendline type:"),
                    choices = list("Linear" = "linear",
                                   "Lowess" = "lowess",
                                   "Quadratic" = "quadratic",
                                   "Cubic" = "cubic"),
                    selected = "linear",
                    inline = T
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
    output$corr_scatterplot <- renderPlot({
        if(is.null(input$corr_sel_x) || is.null(input$corr_sel_y)){return()}
        
        data_ <- data %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_x, input$corr_sel_y)
        # 
        # data_[input$corr_sel_x] <- ifelse(input$corr_sel_x_ff == "Yes", log(data_[input$corr_sel_x]), data[input$corr_sel_x])
        # data_[input$corr_sel_y] <- ifelse(input$corr_sel_y_ff == "Yes", log(data_[input$corr_sel_y]), data[input$corr_sel_y])
        # 
        # data_ %>% filter(get(input$corr_sel_x) < 5)
        
        ### Filtering
        if(input$corr_filter_check == TRUE){
            data_ <- data_ %>% 
                filter(get(input$corr_sel_x) >= input$corr_sel_x_filter[1] & get(input$corr_sel_x) <= input$corr_sel_x_filter[2]) %>% 
                filter(get(input$corr_sel_y) >= input$corr_sel_y_filter[1] & get(input$corr_sel_y) <= input$corr_sel_y_filter[2])
        }
        
        if(input$corr_trendline_type == 'linear')
            {trendline_type <- geom_smooth(method = "lm", formula = y ~ x, color = color[4])}
        if(input$corr_trendline_type == 'lowess')
          {trendline_type <- geom_smooth(method = "loess", color = color[4])}
        if(input$corr_trendline_type == 'quadratic')
            {trendline_type <- geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = color[4])}
        if(input$corr_trendline_type == 'cubic')
            {trendline_type <- geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = color[4])}
        

        x <- input$corr_sel_x
        y <- input$corr_sel_y

        p <- ggplot(data_, 
                    aes(get(x), 
                        get(y),
                        color = color[1],
                        fill = color[1])) +
            geom_point(size = 3,
                          alpha = 0.3,
                       show_legend = F,
                       shape = 4) +
            scale_fill_manual(values=c(color[1], color[2], color[3])) +
            scale_color_manual(values = c(color[1], color[2], color[3])) +
            {if(input$corr_sel_x_ff == "Yes") scale_x_log10()} +
            {if(input$corr_sel_y_ff == "Yes") scale_y_log10()} +
            labs(x = x, y = y) + 
            trendline_type +
            theme_gg() +
            theme(aspect.ratio = 0.8) +
            theme(legend.position = "none")
            
        
        
        return(p)
    })
    
    output$corr_corr <- renderUI({
        if(!is.numeric(data[[input$corr_sel_x]]) || !is.numeric(data[[input$corr_sel_y]])){return("Correlation only makes sense with numeric variables")}
        
        data_ <- data %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_x, input$corr_sel_y)
        
        
        ### Filtering
        if(input$corr_filter_check == TRUE){
            data_ <- data_ %>% 
                filter(get(input$corr_sel_x) >= input$corr_sel_x_filter[1] & get(input$corr_sel_x) <= input$corr_sel_x_filter[2]) %>% 
                filter(get(input$corr_sel_y) >= input$corr_sel_y_filter[1] & get(input$corr_sel_y) <= input$corr_sel_y_filter[2])
        }
        
        HTML(paste0("<h3> Correlation: </h3> <b> <h2> ", round(cor(data_[[input$corr_sel_x]], data_[[input$corr_sel_y]]), digits = 2), "</b> </h2>"))
        
        })
 



########################### MULTIVARIATE REGRESSION #########################################
#### MULTIVARIATE REGRESSION

### Pick a city
output$reg_sel_city <- renderUI({
    selectInput(inputId = "reg_sel_city",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Amsterdam")
})

### Pick a date
output$reg_sel_date <- renderUI({
    selectInput(inputId = "reg_sel_date",
                label = "Select a date:",
                choices = unique(data_reg$date),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-12-01"))
})


### Pick a date
output$reg_sel_dependent <- renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
    
    selectInput(inputId = "reg_sel_dependent",
                label = "Select a dependent variable:",
                choices = selectable_names,
                multiple = FALSE,
                selectize = TRUE,
                selected = c("price"))
})



### Filter extreme values
output$reg_filter_check <- renderUI({
    checkboxInput("reg_filter_check",
                  strong("Tick this if you would like to filter values"), 
                  FALSE)
})


### Pick three variables (default: price, distance, stars)
output$reg_sel_three_variables_A <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", input$reg_sel_dependent))
    
    pickerInput(inputId = "reg_sel_three_variables_A",
                label = "Select numeric variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "offer_cat"))
})



### Pick factor variables (default: price, distance, stars)
output$reg_sel_three_variables_factor_A <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date"))
    
    pickerInput(inputId = "reg_sel_three_variables_factor_A",
                label = "Select factor variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("offer_cat"))
})





### Pick three variables (default: price, distance, stars)
output$reg_sel_three_variables_B <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", input$reg_sel_dependent))
    
    pickerInput(inputId = "reg_sel_three_variables_B",
                label = "Select numeric variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "offer_cat"))
})



### Pick factor variables (default: price, distance, stars)
output$reg_sel_three_variables_factor_B <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date"))
    
    pickerInput(inputId = "reg_sel_three_variables_factor_B",
                label = "Select factor variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("offer_cat"))
})




### Pick three variables (default: price, distance, stars)
output$reg_sel_three_variables_C <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", input$reg_sel_dependent))
    
    pickerInput(inputId = "reg_sel_three_variables_C",
                label = "Select numeric variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "offer_cat"))
})



### Pick factor variables (default: price, distance, stars)
output$reg_sel_three_variables_factor_C <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date"))
    
    pickerInput(inputId = "reg_sel_three_variables_factor_C",
                label = "Select factor variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("offer_cat"))
})



# Render the sliders
output$reg_filters <- renderUI({
    data_ <- data_reg %>% 
        filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(input$reg_sel_three_variables))
    
    xAxisGroup <- unique(names(data_))
    
    # First, create a list of sliders each with a different name
    filtering_function <- function(i) {
        inputName <- paste0("reg_filter_", xAxisGroup[i])
        column_name <- xAxisGroup[i]
        
        if(class(data[[column_name]]) != 'factor'){
            inputpanel <- sliderInput(inputName,
                                      xAxisGroup[i],
                                      min=min(data_[column_name], na.rm = T),
                                      max=max(data_[column_name], na.rm = T),
                                      value=c(min(data_[column_name], na.rm = T), max(data_[column_name], na.rm = T)))
        }
        else{
            inputpanel <- selectInput(inputName,
                                      label = xAxisGroup[i],
                                      choices = unique(as.character(data_[[column_name]])),
                                      multiple = TRUE,
                                      selectize = TRUE,
                                      selected = unique(as.character(data_[[column_name]])))
        }
        
        return(conditionalPanel(condition = "input.reg_filter_check == 1", inputpanel))
    }
    
    sliders <- lapply(1:length(xAxisGroup), filtering_function)
    
    # Create a tagList of sliders (this is important)
    do.call(tagList, sliders)
})


### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$reg_reg_table_A <- renderTable({
    data_ <- data_reg %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
    select(c(c("city"), input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, input$reg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    

    variables_A <- intersect(c(input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A), names(data_))
    f_A <- as.formula(paste(input$reg_sel_dependent, paste(variables_A, collapse = " + "), sep = " ~ "))

    reg_A <<- lm(f_A, data=data_)

    coeffs_A <<- reg_A$coefficients
    confint_A <- confint(reg_A, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    # unname(coeffs_A)
    
    df_A <- data.frame("Name" = c(names(coeffs_A)),
                "Value" = paste0(round(unname(coeffs_A), 2), ifelse(sign(confint_A[, 1]) == sign(confint_A[, 2]), "*", "")),
               "CI 95" = c(paste0(as.character(round(confint_A[, 1], digits = 2)), " - ", as.character(round(confint_A[, 2], digits = 2))))) %>% 
        mutate(across(where(is.factor), as.character))

    df_A[1, 2] = round(unname(coeffs_A), 2)[1]
    
    df_A
})


output$reg_r_2_reg_A <- renderUI({
    data_ <- data_reg %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, input$reg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables_A <- intersect(c(input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A), names(data_))
    f_A <- as.formula(paste(input$reg_sel_dependent, paste(variables_A, collapse = " + "), sep = " ~ "))
    
    reg_A <<- lm(f_A, data=data_)
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_A)$r.squared, digits = 2), "</b> </h4>"))
 
})



output$reg_reg_table_B <- renderTable({
    data_ <- data_reg %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city", input$reg_sel_dependent), input$reg_sel_three_variables_B, input$reg_sel_three_variables_factor_B, input$reg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables_B <- intersect(c(input$reg_sel_three_variables_B, input$reg_sel_three_variables_factor_B), names(data_))
    f_B <- as.formula(paste(input$reg_sel_dependent, paste(variables_B, collapse = " + "), sep = " ~ "))
    
    reg_B <<- lm(f_B, data=data_)
    
    coeffs_B <- reg_B$coefficients
    confint_B <- confint(reg_B, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    df_B <- data.frame("Name" = c(names(coeffs_B)), 
                       "Value" = paste0(round(unname(coeffs_B), 2), ifelse(sign(confint_B[, 1]) == sign(confint_B[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_B[, 1]), digits = 2), " - ", as.character(round(confint_B[, 2], digits = 2))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_B[1, 2] = round(unname(coeffs_B), 2)[1]
    
    df_B
})



output$reg_r_2_reg_B <- renderUI({
    data_ <- data_reg %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_B, input$reg_sel_three_variables_factor_B, input$reg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables_B <- intersect(c(input$reg_sel_three_variables_B, input$reg_sel_three_variables_factor_B), names(data_))
    f_B <- as.formula(paste(input$reg_sel_dependent, paste(variables_B, collapse = " + "), sep = " ~ "))
    
    reg_B <<- lm(f_B, data=data_)
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_B)$r.squared, digits = 2), "</b> </h4>"))
    
})



output$reg_reg_table_C <- renderTable({
    data_ <- data_reg %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_C, input$reg_sel_three_variables_factor_C, input$reg_sel_dependent)) %>% 
        drop_na() %>% 
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables_C <- intersect(c(input$reg_sel_three_variables_C, input$reg_sel_three_variables_factor_C), names(data_))
    f_C <- as.formula(paste(input$reg_sel_dependent, paste(variables_C, collapse = " + "), sep = " ~ "))
    
    
    reg_C <- lm(f_C, data=data_)
    
    coeffs_C <<- reg_C$coefficients
    confint_C   <- confint(reg_C, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    df_C <- data.frame("Name" = c(names(coeffs_C)), 
                       "Value" = paste0(round(unname(coeffs_C), 2), ifelse(sign(confint_C[, 1]) == sign(confint_C[, 2]), "*", "")),
                       "CI 95" = c( paste0(as.character(round(confint_C[, 1], digits = 2)), " - ", as.character(round(confint_C[, 2], digits = 2))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_C[1, 2] = round(unname(coeffs_C), 2)[1]
    
    df_C
})



output$reg_r_2_reg_C <- renderUI({
    data_ <- data_reg %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_C, input$reg_sel_three_variables_factor_C, input$reg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables_C <- intersect(c(input$reg_sel_three_variables_C, input$reg_sel_three_variables_factor_C), names(data_))
    f_C <- as.formula(paste(input$reg_sel_dependent, paste(variables_C, collapse = " + "), sep = " ~ "))
    
    reg_C <<- lm(f_C, data=data_)
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_C)$r.squared, digits = 2), "</b> </h4>"))
    
})





########################### COMPARING REGRESSIONS #########################################



### Pick three variables (default: price, distance, stars)
output$compreg_sel_three_variables <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", input$compreg_sel_dependent))
    
    pickerInput(inputId = "compreg_sel_three_variables",
                label = "Select numeric variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "offer_cat"))
})



### Pick factor variables (default: price, distance, stars)
output$compreg_sel_three_variables_factor <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date"))
    
    pickerInput(inputId = "compreg_sel_three_variables_factor",
                label = "Select factor variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("offer_cat"))
})




### Pick a date
output$compreg_sel_dependent <- renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
    
    selectInput(inputId = "compreg_sel_dependent",
                label = "Select a dependent variable:",
                choices = selectable_names,
                multiple = FALSE,
                selectize = TRUE,
                selected = c("price"))
})





### Pick a city
output$compreg_sel_city_A <- renderUI({
    selectInput(inputId = "compreg_sel_city_A",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Amsterdam")
})

### Pick a date
output$compreg_sel_date_A <- renderUI({
    selectInput(inputId = "compreg_sel_date_A",
                label = "Select a date:",
                choices = unique(data_reg$date),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-12-01"))
})




### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$compreg_reg_table_A <- renderTable({
    data_ <- data_reg %>% filter(city == input$compreg_sel_city_A & date == input$compreg_sel_date_A) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables <- intersect(c(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor), names(data_))
    f_A <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg_A <<- lm(f_A, data=data_)
    
    coeffs_A <<- reg_A$coefficients
    confint_A <- confint(reg_A, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    # unname(coeffs_A)
    
    df_A <- data.frame("Name" = c(names(coeffs_A)),
                       "Value" = paste0(round(unname(coeffs_A), 2), ifelse(sign(confint_A[, 1]) == sign(confint_A[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_A[, 1], digits = 2)), " - ", as.character(round(confint_A[, 2], digits = 2))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_A[1, 2] = round(unname(coeffs_A), 2)[1]
    
    df_A
})


output$compreg_r_2_reg_A <- renderUI({
    data_ <- data_reg %>% filter(city == input$compreg_sel_city_A & date == input$compreg_sel_date_A) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables <- intersect(c(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor), names(data_))
    f_A <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg_A <<- lm(f_A, data=data_)
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_A)$r.squared, digits = 2), "</b> </h4>"))
    
})



### Pick a city
output$compreg_sel_city_B <- renderUI({
    selectInput(inputId = "compreg_sel_city_B",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Amsterdam")
})

### Pick a date
output$compreg_sel_date_B <- renderUI({
    selectInput(inputId = "compreg_sel_date_B",
                label = "Select a date:",
                choices = unique(data_reg$date),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-12-01"))
})




### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$compreg_reg_table_B <- renderTable({
    data_ <- data_reg %>% filter(city == input$compreg_sel_city_B & date == input$compreg_sel_date_B) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables <- intersect(c(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor), names(data_))
    f_B <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg_B <<- lm(f_B, data=data_)
    
    coeffs_B <<- reg_B$coefficients
    confint_B <- confint(reg_B, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    # unname(coeffs_B)
    
    df_B <- data.frame("Name" = c(names(coeffs_B)),
                       "Value" = paste0(round(unname(coeffs_B), 2), ifelse(sign(confint_B[, 1]) == sign(confint_B[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_B[, 1], digits = 2)), " - ", as.character(round(confint_B[, 2], digits = 2))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_B[1, 2] = round(unname(coeffs_B), 2)[1]
    
    df_B
})


output$compreg_r_2_reg_B <- renderUI({
    data_ <- data_reg %>% filter(city == input$compreg_sel_city_B & date == input$compreg_sel_date_B) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables <- intersect(c(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor), names(data_))
    f_B <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg_B <<- lm(f_B, data=data_)
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_B)$r.squared, digits = 2), "</b> </h4>"))
    
})



### Pick a city
output$compreg_sel_city_C <- renderUI({
    selectInput(inputId = "compreg_sel_city_C",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Amsterdam")
})

### Pick a date
output$compreg_sel_date_C <- renderUI({
    selectInput(inputId = "compreg_sel_date_C",
                label = "Select a date:",
                choices = unique(data_reg$date),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-12-01"))
})




### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$compreg_reg_table_C <- renderTable({
    data_ <- data_reg %>% filter(city == input$compreg_sel_city_C & date == input$compreg_sel_date_C) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables <- intersect(c(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor), names(data_))
    f_C <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg_C <<- lm(f_C, data=data_)
    
    coeffs_C <<- reg_C$coefficients
    confint_C <- confint(reg_C, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    # unname(coeffs_C)
    
    df_C <- data.frame("Name" = c(names(coeffs_C)),
                       "Value" = paste0(round(unname(coeffs_C), 2), ifelse(sign(confint_C[, 1]) == sign(confint_C[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_C[, 1], digits = 2)), " - ", as.character(round(confint_C[, 2], digits = 2))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_C[1, 2] = round(unname(coeffs_C), 2)[1]
    
    df_C
})


output$compreg_r_2_reg_C <- renderUI({
    data_ <- data_reg %>% filter(city == input$compreg_sel_city_C & date == input$compreg_sel_date_C) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    variables <- intersect(c(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor), names(data_))
    f_C <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg_C <<- lm(f_C, data=data_)
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_C)$r.squared, digits = 2), "</b> </h4>"))
    
})















output$about <- renderUI({
    HTML("Version 0.2. Created by <a href = 'https://www.linkedin.com/in/benedek-pasztor/'> Benedek Pásztor</a>, 
    <a href = 'https://www.linkedin.com/in/bekesgabor/'> Gábor Békés </a> 
    and <a href = 'https://www.linkedin.com/in/gabor-kezdi-28951640/'> Gábor Kézdi </a>,
         based on Békés-Kézdi: Data Analysis for Business, Economics, and Policy, 
         Cambridge University Press, 2021.  <a href = 'http://gabors-data-analysis.com/'> http://gabors-data-analysis.com/ </a>
         The project is supported by CEU Executive MBA program <a href = 'https://emba.ceu.edu/'> https://emba.ceu.edu </a>")
    
})


})



