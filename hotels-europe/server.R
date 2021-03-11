
# Based on code for the textbook "Data Analysis for Business, Economics, and Policy"
# Cambridge University Press, 2021
# by Gábor Békés and Gábor Kézdi
# https://gabors-data-analysis.com/
# https://github.com/gabors-data-analysis/da_case_studies

########################################################################
#
# CEU DA Interactive Visualization
# Server functions
# Benedek PASZTOR
#
########################################################################

library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(skimr)
# library(ggpubr)
library(cowplot)
library(modelsummary)
library(stringr)
# library(shinyWidgets)
# library(shinyAce)
library(plyr)
library(stringi)


mean_na <- function(x) mean(x, na.rm = TRUE)
max_na <- function(x) max(x, na.rm = TRUE)
min_na <- function(x) min(x, na.rm = TRUE)
sd_na <- function(x) sd(x, na.rm = TRUE)


roundUp <- function(x,m) m*ceiling(x / m)
rsq <- function (x, y) cor(x, y) ^ 2


roundUp_n <- function(x,to=10)
{
    to*(x%/%to + as.logical(x%%to))
}

my.order <- c('integer', 'factor', 'character')


background_hex <- "#f2e6d9"

## Theme Config
source("theme_bg.R")

theme_gg <- theme_bg

`%notin%` <- function(x,y)!('%in%'(x,y))



District_basing <- function(data_){
    if('District' %in% names(data_)){
        factor_0 <- (data_ %>% 
                       group_by(District) %>% 
                       dplyr::summarise(n = n()) %>% 
                       mutate(District = as.character(District)) %>% 
                       arrange(n) %>% 
                       tail(1)['District'])[[1]]
        
        data_ <- data_ %>% mutate(District = relevel(District, factor_0))
        return(data_)
    }
    else{return(data_)}
}



split_interaction_terms <- function(inter_){
    ret <- strsplit(inter_, "\\*")[[1]]
    return(ret)
}

check_if_no_selection <- function(list_of_inputs){
  k <- 0
  for(i in list_of_inputs){
    if(is.null(i) | length(i) == 0){
      k <- k + 1
    }
  }
  if(k == length(list_of_inputs)){
    return(FALSE)
  }
  else{
    return(TRUE)
  }
}

selection_check <- function(list_of_inputs){
  return(
    validate(
      need(check_if_no_selection(list_of_inputs), "Please double check your filtering options.")
    ))
} 

selection_check_reg <- function(list_of_inputs){
  return(
    validate(
      need(check_if_no_selection(list_of_inputs), "Please select at least one RHS variable")
    ))
}


### DATA PREP OF THE ORIGINAL DATA -- saved as hotels_clean.csv
# data <- read.csv(paste0('data/hotels.csv'), encoding = "UTF-8") %>%
#   mutate(weekend_str = ifelse(weekend == 1, '_WEEKEND', '_WEEKDAY')) %>%
#   # mutate(neighbourhood = as.factor(enc2utf8(as.character(neighbourhood)))) %>%
#     mutate(date = as.factor(paste0(strptime(paste0(year, '-', month, '-', 1), "%Y-%m-%d"), weekend_str))) %>%
#     mutate(date = factor(as.character(date), sort(levels(date)))) %>% 
#   filter(nnights == 1) %>% 
#   # mutate(city_actual_2 = ifelse(city_actual  == TRUE, "Yes", "No")) %>%
#   # mutate(city_actual = city_actual_2) %>%
#     select(date, city, price, distance, distance_alter, rating_reviewcount, rating, stars, offer_cat,
#            accommodation_type, city_actual, neighbourhood) %>%
#     filter(stars >= 2) %>% 
#     mutate(accommodation_type_2 = ifelse(accommodation_type %in% c("Apartment", "Vacation home Condo"),
#                                        "Apartment",
#                                        ifelse(accommodation_type %in% c("Guest House", "Bed and breakfast", "Inn", "Apart-hotel", "Pension"),
#                                               "Guest House",
#                                               ifelse(accommodation_type %in% c("Hotel"),
#                                                      "Hotel",
#                                               "Other"))),
#            accommodation_type = as.factor(accommodation_type_2)) %>%
#     select(-accommodation_type_2) %>%
#     filter(accommodation_type %in% c("Hotel", "Guest House", "Apartment")) %>%
#     mutate(rating = rating,
#            stars = as.factor(stars)) %>%
#     mutate(offer_cat =  str_extract(offer_cat, "[^ ]+")) %>%
#     # mutate(offer_cat = as.factor(offer_cat)) %>%
#     mutate(offer_cat = ifelse(is.na(offer_cat), '0%', offer_cat)) %>%
#     mutate(offer_cat = ifelse(offer_cat %in% c("0%", "1-15%"), "Regular (base)",
#                               ifelse(offer_cat %in% c("15-50%"), "Discount",
#                                                       "Sale"))) %>%
#     mutate(offer_cat = as.factor(offer_cat)) %>%
#     mutate(city_actual = relevel(as.factor(ifelse(as.character(city_actual) == as.character(city), "Yes", "No")), "Yes")) %>%
#     mutate(offer_cat = relevel(offer_cat, "Regular (base)"),
#            accommodation_type = relevel(accommodation_type, "Hotel"),
#            stars = as.factor(relevel(stars, 3))) %>%
#     plyr::rename(c("rating_reviewcount" = "N_rating_review",
#                    "offer_cat" = "Offer_Cat",
#                    "neighbourhood" = "District",
#                    "accommodation_type" = "Type")) %>%
# select(sapply(., class) %>% .[order(match(., my.order))] %>% names)

# data2 <- data %>% filter(date %in% c('2017-11-01_WEEKDAY', '2017-11-01_WEEKEND', '2017-12-01_WEEKDAY', '2018-12-01_WEEKEND', '2018-02-01_WEEKDAY', '2018-02-01_WEEKEND', '2018-06-01_WEEKDAY', '2018-06-01_WEEKEND'))
# 
# summary(data2$date)
# write.table(data2, 'data/hotels_clean.csv', sep = ',', dec = '.', row.names = F)


shinyServer(function(input, output, session) {
  
    data <- read.csv('data/hotels_clean.csv') %>% 
      mutate(stars = as.factor(stars))
    
    
    data_reg <- data %>% mutate(ln_price = log(price),
                                ln_distance = log(distance),
                                `ln_N_rating_review` = log(`N_rating_review`)) %>%
        select(date, price, ln_price, N_rating_review, ln_N_rating_review, distance, ln_distance, distance_alter, Offer_Cat, stars, Type, city_actual, District, city,rating, city_actual) %>% 
      mutate(ln_price = round(ifelse(ln_price %in% c(-Inf, Inf), 0, ifelse(is.na(ln_price), 0, ln_price)), digits = 1),
             ln_distance = round(ifelse(ln_distance %in% c(-Inf, Inf), 0, ifelse(is.na(ln_distance), 0, ln_distance)), digits = 1),
             ln_N_rating_review = round(ifelse(ln_N_rating_review %in% c(-Inf, Inf), 0, ifelse(is.na(ln_N_rating_review), 0, ln_N_rating_review)), digits = 1)) 
    

    
    dataframe_length_check <- function(){
      return(
        validate(
          need(dim(reactive_data())[1] > 0, "Please double check your filtering options.")
        ))
    } 
    
    dataframe_length_check_reg <- function(){
      return(
        validate(
          need(dim(reactive_data_reg())[1] > 1, "Please double check your filtering options.")
        ))
    } 
    
    
    ### Filter extreme values
    output$filter_check <- renderUI({
        checkboxInput("filter_check",
                      strong("Tick this if you would like to filter values"), 
                      FALSE)
    })
    
    loading <- function(validation_vars){
        return(
        validate(
            need(check_if_no_selection(validation_vars) > 0, "Loading ...") # display custom message in need
        ))
    }
    
   
    selected_list <- list()
    
    
    data_re <- reactive({
      if(input$tabs == 'desc'){
        loading(input$desc_sel_city)
        data_ <- data %>% filter(city == input$desc_sel_city,
                                 date == input$desc_sel_date)
      }
      else if(input$tabs == 'comp'){
        loading(input$comp_sel_cities)
        data_ <- data %>% filter(city %in% input$comp_sel_cities,
                                 date == input$comp_sel_date)
      }
      else if(input$tabs == 'corr'){
        loading(input$corr_sel_city)
        data_ <- data %>% filter(city == input$corr_sel_city,
                                 date == input$corr_sel_date)
      }
      else if(input$tabs == 'reg'){
        loading(input$reg_sel_city)
        data_ <- data_reg %>% filter(city == input$reg_sel_city,
                                     date == input$reg_sel_date)
      }
      else if(input$tabs == 'compreg'){
        loading(input$compreg_sel_city_A)
        data_ <- data_reg %>% filter(city %in% c(input$compreg_sel_city_A, input$compreg_sel_city_B, input$compreg_sel_city_C),
                                     date %in% c(input$compreg_sel_date_A, input$compreg_sel_date_B, input$compreg_sel_date_C))
      }
      else if(input$tabs == 'pred'){
        loading(input$pred_sel_city)
        data_ <- data_reg %>% filter(city == input$pred_sel_city,
                                     date == input$pred_sel_date)
      }
      else{data_ <- data}
      
      
      data_ <- data_ %>%
        select(setdiff(names(data_), c("month", "year", "hotel_id", "",  "weekend",
                                       "holiday", "city", "country", "offer", "ratingta", "ratingta_count", "date")))
      data_
    })
    
    # for (inputName in colnames(data_re()){
    #   data_ <- data_re()
    #   if(class(data[[column_name]]) != 'factor'){
    #     selected_list[[inputName]] <<- c(min(data_[column_name], na.rm = T), max(data_[column_name], na.rm = T))
    #   }
    #   else{
    #     selected_list[[inputName]] <<-  unique(as.character(data_[[column_name]]))
    #   }
    #   
    # }
    # data_re()
    # 
    # observe({
    #   d.selected <<- input$d
    # })
    # 
    
    # Render the sliders
    output$filters <- renderUI({
        data_ <- data_re()
        
        
        xAxisGroup <- unique(names(data_))
        
        # First, create a list of sliders each with a different name
        filtering_function <- function(i) {
            inputName <- paste0("filter_", xAxisGroup[i])
            column_name <- xAxisGroup[i]
            
            
            
            if(class(data[[column_name]]) != 'factor'){
               selected_list[[inputName]] <<- c(min(data_[column_name], na.rm = T), max(data_[column_name], na.rm = T))
                
                inputpanel <- sliderInput(inputName,
                                          xAxisGroup[i],
                                          min=min(data_[column_name], na.rm = T),
                                          max=max(data_[column_name], na.rm = T),
                                          value=selected_list[[inputName]])
            }
            else{
                data_$stars <- ordered(data_$stars, levels = c("2", "2.5", "3", "3.5", "4", "4.5", "5") )
                selected_list[[inputName]] <<-  unique(as.character(data_[[column_name]]))
                
                inputpanel <- pickerInput(inputName,
                                          label = xAxisGroup[i],
                                          choices = sort(unique(as.character(data_[[column_name]]))),
                                          multiple = TRUE,
                                          selected = selected_list[[inputName]])
            }
            
            return(conditionalPanel(condition = "input.filter_check == 1", inputpanel))
        }
        
        sliders <- lapply(1:length(xAxisGroup), filtering_function)
        
        
        
        # Create a tagList of sliders (this is important)
        do.call(tagList, sliders)
    })
    
    reactive_data <- reactive({
        
        data_ <- data %>%
            select(setdiff(names(data), c("month", "year", "hotel_id", "",  "weekend",
                                          "holiday", "country", "offer", "ratingta", "ratingta_count")))
        
        names_to_filter <- setdiff(names(data_), c("city", "date"))
        
        data_ <- data
        
        for(i in names_to_filter){
            if(class(data[[i]]) != 'factor'){
                filter_obj <- eval(parse(text = paste0("input$filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) >= filter_obj[1] & get(i) <= filter_obj[2])
            }
            else{
                filter_obj <- eval(parse(text = paste0("input$filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) %in% filter_obj)
            }
        }
    
        data_$stars <- ordered(data_$stars, levels = c("2", "2.5", "3", "3.5", "4", "4.5", "5") )
        
        data_ %>% drop_na()
    })
    
    
    reactive_data_reg <- reactive({
        data_ <- data_reg %>%
            select(setdiff(names(data_reg), c("month", "year", "hotel_id", "",  "weekend",
                                          "holiday", "country", "offer", "ratingta", "ratingta_count")))
        
        names_to_filter <- setdiff(names(data_), c("city", "date"))
        
        data_ <- data_reg
        
        
        for(i in names_to_filter){
            if(class(data_[[i]]) != 'factor'){
                filter_obj <- eval(parse(text = paste0("input$filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) >= filter_obj[1]) %>% 
                  filter(get(i) <= filter_obj[2])
            }
            else{
                filter_obj <- eval(parse(text = paste0("input$filter_", i)))
                data_ <- data_ %>%
                    filter(get(i) %in% filter_obj)
            }
        }
        
        for(i in c("price", "distance")){
          if(length(unique(data_[[i]])) == 1){
            data_[i] <- NA
          }
        }
        
        data_ %>% drop_na()
    })
    
    
    ##################### DESCRIBE DATA FOR A CITY ############################
    ## DESCRIBE DATA FOR A CITY
    ### Pick a city
    output$desc_sel_city <- renderUI({
        selectInput(inputId = "desc_sel_city",
                    label = "Select a city:",
                    choices = unique(data$city),
                    multiple = FALSE,
                    selectize = TRUE,
                    selected = "Vienna")
    })
    
    ### Pick a date
    output$desc_sel_date <- renderUI({
            selectInput(inputId = "desc_sel_date",
                        label = "Select a date:",
                        choices = sort(unique(data$date)),
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = c("2017-11-11_WEEKDAY"))
    })


    ### Pick three variables (default: price, distance, stars)
    output$desc_sel_three_variables <-renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, T, F)], c("month", "year", "hotel_id", "",  "weekend", "holiday", "city", "country", "offer", "ratingta", "ratingta_count"))
        
        selectizeInput(inputId = "desc_sel_three_variables",
                    label = "Select quantitative variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    selected = c("price", "distance"),
                    options = list(maxItems = 3))
    })
    
    
    ### Pick factor variables (default: price, distance, stars)
    output$desc_sel_three_variables_factor <-renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, F, T)], c("month", "year", "hotel_id", "weekend", "holiday", "city", "country", "offer", "date"))
        
        selectizeInput(inputId = "desc_sel_three_variables_factor",
                    label = "Select categorical variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    selected = c("stars"),
                    options = list(maxItems = 3))
    })

    
    

    desc_plotting_function <- function(thisID, data_, data_c){
        data__ <- data_ %>% 
            filter_all(all_vars(!is.infinite(.))) %>% 
            drop_na()
        
        
        stars_like_treatment <- unique(c(c('stars', 'nnights', 'ratingta'), names(data[sapply(data, function(x) length(unique(x))< 4)])))
        
        if(class(data_[[thisID]]) == 'factor' | thisID %in% stars_like_treatment){
            color_vector <- c(color[2], color[3], color[4])
            
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
                theme(legend.position = "none") +
                theme(aspect.ratio = 0.8)
                
        }
        
        else{
    
            condition_ <- max(data_c %>% select(thisID)) >= 500
            bins <- 20
            is_it_rating <- thisID == 'rating'
            
            
            if(is_it_rating == T){
            p <- data__ %>%
                drop_na() %>% 
                ggplot(aes(x = get(thisID),
                           y = (..count..)/sum(..count..),
                           fill = city,
                           color = city)) +
                geom_histogram(bins = bins, 
                               binwidth = 0.5,
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
                                                     roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10) + ifelse(condition_, 500, 10) / bins))),
                                   breaks = seq(from = ifelse(is_it_rating,
                                                              1,
                                                              0),
                                                to = ifelse(is_it_rating,
                                                            5,
                                                            roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10)) + ifelse(condition_, 500, 10) / bins),
                                                by = ifelse(is_it_rating,
                                                            0.5,
                                                            roundUp((max(data_c %>% select(thisID)) - min(data_c %>% select(thisID))), ifelse(condition_, 500, 10) / bins))),
                                   expand = c(0, 0)) +
                theme_gg() +
                theme(legend.position = "none") +
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                theme(aspect.ratio = 0.8) +
                theme(plot.background=element_rect(fill=background_hex))
            } 
            else{
                p <- data__ %>%
                    drop_na() %>% 
                    ggplot(aes(x = get(thisID),
                               y = (..count..)/sum(..count..),
                               fill = city,
                               color = city)) +
                    geom_histogram(binwidth = roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10)) / bins,
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
                                                  roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10) / bins)),
                                       breaks = seq(from = 0,
                                                    to = roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10)),
                                                    by = roundUp(max(data_c %>% select(thisID)), ifelse(condition_, 1000, 10)) / bins),
                                       expand = c(0, 0)) +
                    theme_gg() +
                    theme(legend.position = "none") +
                    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                    theme(aspect.ratio = 0.8) +
                    theme(plot.background=element_rect(fill=background_hex))  
                
                
            }
            
            q <- ggplot_build(p) 
            
            p <- ggplot_gtable(q)
        }
        
        return(p)
    }
    
    output$desc_histogram <- renderPlot({
        loading(c(input$desc_sel_city, input$desc_sel_date))
        dataframe_length_check()

      
        selection_check(c(input$desc_sel_three_variables))
        
        data_ <- reactive_data()
        
        data_c <- reactive_data() %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(c(c("city"), input$desc_sel_three_variables, input$desc_sel_three_variables_factor)) %>% 
            filter_all(all_vars(!is.infinite(.))) %>% 
            drop_na()

        
        
        data_ <- data_ %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(c(c("city"), input$desc_sel_three_variables, input$desc_sel_three_variables_factor)) %>% 
            drop_na() %>% 
            filter_all(all_vars(!is.infinite(.)))
        
        
        myPlots <- lapply(c(input$desc_sel_three_variables), desc_plotting_function, data_ = data_, data_c = data_c)
        
        
        p <- plot_grid(plotlist = myPlots, nrow = 1)
        
        return(p)
    }, bg = background_hex)

    
    output$desc_histogram_factor <- renderPlot({
      selection_check(c(input$desc_sel_three_variables_factor))
      
        loading(input$desc_sel_city)

        data_ <- reactive_data()
        

        data_c <- reactive_data() %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(c(c("city"), input$desc_sel_three_variables, input$desc_sel_three_variables_factor)) %>% 
            drop_na() %>% 
            filter_all(all_vars(!is.infinite(.)))
        
        
        data_ <- data_ %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
            select(c(c("city"), input$desc_sel_three_variables, input$desc_sel_three_variables_factor)) %>% 
            drop_na() %>% 
            filter_all(all_vars(!is.infinite(.)))
        
        factors_ <- setdiff(input$desc_sel_three_variables_factor, c('District'))
        myPlots <- lapply(factors_, desc_plotting_function, data_ = data_, data_c = data_c)
        
        
        p <- plot_grid(plotlist = myPlots, nrow = 1)
        
        return(p)
    }, bg = background_hex)
    
    
    output$desc_histogram_factor_district <- renderPlot({
      selection_check(c(input$desc_sel_three_variables_factor))
      loading(input$desc_sel_city)
      
      if ('District' %notin% input$desc_sel_three_variables_factor){
        return()
      }
      else{
    
      data_c <- reactive_data() %>% 
        filter(city == input$desc_sel_city & date == input$desc_sel_date) %>% 
        select(city, District)
      color_vector <- c(color[2], color[3], color[4])
      
      data_di <- data_c %>% 
        group_by(city, District) %>%
        dplyr::summarise(n = n()) %>% 
        dplyr::arrange(desc(n)) %>% 
        head(20)
      
      data_ci <- data_c %>% 
        group_by(city) %>% 
        dplyr::summarise(n_all = n())
        
      data_c <- data_di %>% merge(data_ci) %>% 
        mutate(n_perc = n / n_all)

      summary(data_c)
      p <- data_c %>% 
        ggplot(aes(x = reorder(District, n_perc),
                   y = n_perc,
                   fill = city,
                   color = city)) +
        geom_bar(stat = 'identity',
                 show.legend = F) + 
        coord_flip() +
        scale_fill_manual(values=color_vector) +
        scale_color_manual(values = color.outline) +
        ggtitle(paste("Variable name:  ", 'District')) +
        scale_y_continuous("Percentage", labels = scales::percent_format(accuracy = 1L)) +
        xlab("") +
        theme_gg() 
      
      
      p
      
      return(p)      
      }
      
    }, bg = background_hex)
    
        
    output$desc_nrows <- renderUI({
        loading(input$desc_sel_city)
      selection_check(c(input$desc_sel_three_variables, input$desc_sel_three_variables_factor))
      
        data_ <- reactive_data()

        data_ <- data_ %>% 
            filter(city == input$desc_sel_city & date == input$desc_sel_date) %>%
            select(input$desc_sel_three_variables, input$desc_sel_three_variables_factor) %>% 
          drop_na()
        
        HTML(paste0('Number of observations: ', nrow(data_)))
        
    })
    
    
    #### Show basic descriptive stats table:  (mean, sd, median, min, max, p5, p95)
    output$desc_summary <- renderTable({
        loading(input$desc_sel_city)
        selection_check(c(input$desc_sel_three_variables))
      
        data_ <- reactive_data()
    
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
            mutate(stats = sapply(str_split(stats, "_"), function(x) x[1])) %>% 
            select(stats, everything())        
        b
        
    })
    
    output$desc_summary_factor <- renderTable({
        loading(input$desc_sel_city)
        selection_check(c(input$desc_sel_three_variables_factor))
      
        data_ <- reactive_data()
        
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
        
        p = as.data.frame(t(data_)) %>%  
            mutate(statistic = row.names(.)) %>% 
            select(statistic, everything())
        
        p
    })
    
    
    ########################### COMPARE TWO CITIES #########################################
    output$comp_sel_cities <- renderUI({
        selectizeInput(inputId = "comp_sel_cities",
                    label = "Select two cities:",
                    choices = unique(data$city),
                    multiple = TRUE,
                    selected = c("Vienna", "Budapest"),
                    options = list(maxItems = 2))
    })
    
    ### Pick a date
    output$comp_sel_date <- renderUI({
        selectInput(inputId = "comp_sel_date",
                    label = "Select a date:",
                    choices = sort(unique(data$date)),
                    multiple = FALSE,
                    selectize = TRUE,
                    selected = c("2017-11-11_WEEKDAY"))
    })
    
    
    ### Pick three variables (default: price, distance, stars)
    output$comp_sel_three_variables <-renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, T, F)], c("month", "year", "hotel_id", "",  "weekend", "holiday", "city", "country", "offer", "ratingta", "ratingta_count"))
        
        selectizeInput(inputId = "comp_sel_three_variables",
                    label = "Select quantitative variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    options = list(maxItems = 3),
                    selected = c("price", "distance"))
    })
    
    
    ### Pick factor variables (default: price, distance, stars)
    output$comp_sel_three_variables_factor <-renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, F, T)], c("month", "year", "hotel_id", "weekend", "holiday", "city", "country", "offer", "date", "District"))
        
        selectizeInput(inputId = "comp_sel_three_variables_factor",
                    label = "Select categorical variables:",
                    choices = selectable_names,
                    multiple = TRUE,
                    options = list(maxItems = 3),
                    selected = c("stars"))
    })


    
    
    comp_plotting_function <-  function(thisID, data_, index, vars_all){
        data__ <- data_
            # select(city, thisID) %>%

        data_c <- reactive_data() %>%
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>%
            select(c(c("city"), vars_all)) %>%
            drop_na() %>%
            filter_all(all_vars(!is.infinite(.)))
        
        
        show_legend <- ifelse(index == length(vars_all),
                              T,
                              F)
        
        stars_like_treatment <- unique(c(c('stars', 'nnights'), names(data_[sapply(data_, function(x) length(unique(x))< 4)])))
        
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
                      legend.title = element_blank()) +
                theme(aspect.ratio = 0.8) +
                theme(plot.background=element_rect(fill=background_hex))
            
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
                theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
                theme(aspect.ratio = 0.8) +
                theme(plot.background=element_rect(fill=background_hex))
            
            
            q <- ggplot_build(p)
            
            q$data[[1]] <- q$data[[1]] %>% mutate_all(~replace(., is.na(.), 0))
            q$layout$panel_params[[1]]$x$breaks <- sort(q$data[[1]]$xmin)[c(T, F)]
            q$layout$panel_params[[1]]$x$breaks <- replace(q$layout$panel_params[[1]]$x$breaks, 1, 0)
            q$layout$panel_params[[1]]$x$breaks <- round(q$layout$panel_params[[1]]$x$breaks, digits = ifelse(mean(q$layout$panel_params[[1]]$x$breaks) > 100, 0,
                                                                                                              ifelse(mean(q$layout$panel_params[[1]]$x$breaks) > 10, 1,
                                                                                                                     ifelse(q$layout$panel_params[[1]]$x$breaks == 0, 0, 2))))
            
            
            p <- ggplot_gtable(q)
        }
    
        return(p)
    
    }
    
    # output$comp_histogram_factor_district <- renderPlot({
    #   selection_check(c(input$desc_sel_three_variables_factor))
    #   loading(input$desc_sel_city)
    #   
    #   if ('District' %notin% input$desc_sel_three_variables_factor){
    #     return()
    #   }
    #   else{
    #     
    #     data_c <- reactive_data() %>%
    #       filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>%
    #       select(District, city)
    #     
    #     color_vector <- c(color[2], color[3], color[4])
    #     
    #     data_di_1 <- data_c %>% 
    #       filter(city == input$comp_sel_cities[1]) %>% 
    #       group_by(city, District) %>%
    #       dplyr::summarise(n = n()) %>% 
    #       dplyr::arrange(desc(n)) %>% 
    #       head(20)
    #     
    #     data_di_2 <- data_c %>% 
    #       filter(city == input$comp_sel_cities[2]) %>% 
    #     group_by(city, District) %>%
    #       dplyr::summarise(n = n()) %>% 
    #       dplyr::arrange(desc(n)) %>% 
    #       head(20)
    #     
    #     data_ci <- data_c %>% 
    #       group_by(city) %>% 
    #       dplyr::summarise(n_all = n())
    #     
    #     data_c <- data_di_1 %>% merge(data_ci) %>% merge(data_di_2)
    #       mutate(n_perc = n / n_all)
    #     
    #     data__ <- data_c
    #     
    #     p <- ggplot(data__, aes(x = reorder(District, n_perc),
    #                  y = n_perc,
    #                  fill = city,
    #                  color = city)) +
    #       geom_bar(data = subset(data__, city == input$comp_sel_cities[1]),
    #                aes(y = n_perc),
    #                position = 'identity',
    #                alpha = 0.3,
    #                size = 1.05,
    #                show.legend = show_legend) +
    #       geom_bar(data = subset(data__, city == input$comp_sel_cities[2]),
    #                aes(y = n_perc),
    #                position = 'identity',
    #                alpha = 0.3,
    #                size = 0.95,
    #                show.legend = show_legend) +
    #       coord_flip() +
    #       scale_fill_manual(values=color_vector) +
    #       scale_color_manual(values = color.outline) +
    #       ggtitle(paste("Variable name:  ", 'District')) +
    #       scale_y_continuous("Percentage", labels = scales::percent_format(accuracy = 1L)) +
    #       theme(legend.position = "right",
    #             legend.text = element_text(size = 18),
    #             legend.title = element_blank()) +
    #       xlab("") +
    #       theme_gg() 
    #     
    #     
    #     p
    #     
    #     return(p)      
    #   }
    # })
    
    output$comp_histogram <- renderPlot({
        loading(input$comp_sel_cities)
       selection_check(c(input$comp_sel_three_variables))
      
        
        data_ <- reactive_data()
        
        data_ <- data_ %>%
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>%
            select(c(c("city"), input$comp_sel_three_variables)) %>%
            drop_na() %>%
            filter_all(all_vars(!is.infinite(.)))
        
        myPlots <- lapply(c(input$comp_sel_three_variables), comp_plotting_function, data_ = data_,
                          index = seq_along(input$comp_sel_three_variables), vars_all = input$comp_sel_three_variables)
        
        p <- plot_grid(plotlist = myPlots, nrow = 1)
        
        return(p)
    }, bg = background_hex)
    
    
    
    output$comp_histogram_factor <- renderPlot({
      
        loading(input$comp_sel_cities)
        selection_check(c(input$comp_sel_three_variables_factor))
      
        data_ <- reactive_data()
        
        data_ <- data_ %>%
            filter(city %in% input$comp_sel_cities & date == input$comp_sel_date) %>%
            select(c(c("city"), input$comp_sel_three_variables_factor)) %>%
            drop_na() %>%
            filter_all(all_vars(!is.infinite(.)))
        
        myPlots <- lapply(c(input$comp_sel_three_variables_factor), comp_plotting_function, data_ = data_,
                          index = seq_along(input$comp_sel_three_variables_factor), vars_all = input$comp_sel_three_variables_factor)
        
        p <- plot_grid(plotlist = myPlots, nrow = 1)
        
        return(p)
    }, bg = background_hex)
    
    
    
    output$comp_city_1 <- renderText({
        loading(input$comp_sel_cities)
        
        input$comp_sel_cities[1]})
    output$comp_city_2 <- renderText({
        loading(input$comp_sel_cities)
        
        input$comp_sel_cities[2]})
    
    
    output$comp_summary_1 <- renderTable({
        loading(input$comp_sel_cities)
        selection_check(c(input$comp_sel_three_variables))
      
        
        data_ <- reactive_data()

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
            mutate(stats = sapply(str_split(stats, "_"), function(x) x[1])) %>% 
            select(stats, everything())
        b
    })
    
    output$comp_nrows_1 <- renderUI({
        loading(input$comp_sel_cities)
        selection_check(c(input$comp_sel_three_variables, input$comp_sel_three_variables_factor))
      
        
        data_ <- reactive_data()
        
        data_ <- data_ %>% 
            filter(city == input$comp_sel_cities[1] & date == input$comp_sel_date)
        
        HTML(paste0('Number of observations: ', nrow(data_)))
    })        

    output$comp_nrows_2 <- renderUI({
        loading(input$comp_sel_cities)
        selection_check(c(input$comp_sel_three_variables, input$comp_sel_three_variables_factor))
      
        
        data_ <- reactive_data()
        
        data_ <- data_ %>% 
            filter(city == input$comp_sel_cities[2] & date == input$comp_sel_date)
        
        HTML(paste0('Number of observations: ', nrow(data_)))
    })    
    
    output$comp_summary_factor_1 <- renderTable({
        loading(input$comp_sel_cities)
      selection_check(c(input$comp_sel_three_variables_factor))
      
        
        data_ <- reactive_data()

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
        loading(input$comp_sel_cities)
        selection_check(c(input$comp_sel_three_variables))
      
        
        data_ <- reactive_data()

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
            mutate(stats = sapply(str_split(stats, "_"), function(x) x[1])) %>% 
            select(stats, everything())        
        b
    })
    
    output$comp_summary_factor_2 <- renderTable({
        loading(input$comp_sel_cities)
        selection_check(c(input$comp_sel_three_variables_factor))
      
        
        data_ <- reactive_data()

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
                    selected = "Vienna")
    })
    
    ### Pick a date
    output$corr_sel_date <- renderUI({
            selectInput(inputId = "corr_sel_date",
                        label = "Select a date:",
                        choices = sort(unique(data$date)),
                        multiple = FALSE,
                        selectize = TRUE,
                        selected = c("2017-11-11_WEEKDAY"))
    })


    
    output$corr_ci <- renderUI({
        checkboxInput("corr_ci", "Add 95% confidence interval", FALSE)
    })
    

    
    ### X selector
    output$corr_sel_x <- renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
        
        selectInput(inputId = "corr_sel_x",
                    label = strong("X: quantitative variable:"), 
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
    
    
    
    output$corr_regressionline_type <- renderUI({
        radioButtons(inputId = "corr_regressionline_type",
                    label = strong("Select regression line type:"),
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
    
    output$corr_sel_factor <-renderUI({
        selectable_names <- setdiff(names(data)[ifelse(sapply(data, is.factor) == F, F, T)], c("month", "year", "hotel_id", "weekend", "holiday", "city", "country", "offer", "date"))
        
        selectizeInput(inputId = "corr_sel_factor",
                       label = strong("X: categorical variable: "),
                       choices = selectable_names,
                       multiple = FALSE,
                       selected = c("stars"))
        })
    
    
    
    output$corr_nrows <- renderUI({
        loading(input$corr_sel_city)
      dataframe_length_check()
      
        if(is.null(input$corr_sel_x) || is.null(input$corr_sel_y)){return()}
        
        data_ <- reactive_data() %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_x, input$corr_sel_y)
        
        HTML(paste0('Number of observations: ', nrow(data_)))
        
    })
    
    ### Plots
    output$corr_scatterplot <- renderPlot({
        loading(input$corr_sel_city)
      dataframe_length_check()

        if(is.null(input$corr_sel_x) || is.null(input$corr_sel_y)){return()}
        
        data_ <- reactive_data() %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_x, input$corr_sel_y)
  
        
        if(input$corr_regressionline_type == 'linear')
            {regressionline_type <- geom_smooth(method = "lm", formula = y ~ x, color = color[4], se = input$corr_ci)}
        if(input$corr_regressionline_type == 'lowess')
          {regressionline_type <- geom_smooth(method = "loess", color = color[4], se = input$corr_ci)}
        if(input$corr_regressionline_type == 'quadratic')
            {regressionline_type <- geom_smooth(method = "lm", formula = y ~ x + I(x^2), color = color[4], se = input$corr_ci)}
        if(input$corr_regressionline_type == 'cubic')
            {regressionline_type <- geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = color[4], se = input$corr_ci)}
        

        x <- input$corr_sel_x
        y <- input$corr_sel_y

        p <- ggplot(data_, 
                    aes(get(x), 
                        get(y),
                        color = color[1],
                        fill = color[1])) +
            geom_point(size = 3,
                          alpha = 0.3,
                       show_legend = F) +
            scale_fill_manual(values=c(color[1], color[2], color[3])) +
            scale_color_manual(values = c(color[1], color[2], color[3])) +
            {if(input$corr_sel_x_ff == "Yes") scale_x_log10()} +
            {if(input$corr_sel_y_ff == "Yes") scale_y_log10()} +
            {if(input$corr_sel_x_ff == "Yes") xlab(paste0("Natural log ", input$corr_sel_x))} +
            {if(input$corr_sel_y_ff == "Yes") ylab(paste0("Natural log ", input$corr_sel_y))} +            
            {if(input$corr_sel_x_ff == "No") xlab(paste0("", input$corr_sel_x))} +
            {if(input$corr_sel_y_ff == "No") ylab(paste0("", input$corr_sel_y))} +            
            
                        regressionline_type +
            theme_gg() +
            theme(aspect.ratio = 0.8) +
            theme(legend.position = "none") +
            theme(plot.background=element_rect(fill=background_hex))
        
            
        
        
        return(p)
    }, bg = background_hex)
    
    
    output$corr_boxplot <- renderPlot({
        loading(input$corr_sel_city)
      dataframe_length_check()
      

        if(is.null(input$corr_sel_factor) || is.null(input$corr_sel_y)){return()}
        
        data_ <- reactive_data() %>% 
            filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
            select(input$corr_sel_y, input$corr_sel_factor)
        
        
        
        
        x <- input$corr_sel_factor
        y <- input$corr_sel_y
        
        p <- ggplot(data_, 
                    aes(get(x), 
                        get(y),
                        color = color[1],
                        fill = color[1])) +
            geom_boxplot(size = 1,
                       alpha = 0.7,
                       show_legend = F) +
            geom_jitter(alpha = 0.7) + 
            scale_fill_manual(values=c(color[1], color[2], color[3])) +
            scale_color_manual(values = c(color[1], color[2], color[3])) +
            xlab(input$corr_sel_factor) +
            {if(input$corr_sel_y_ff == "Yes") scale_y_log10()} +
            {if(input$corr_sel_y_ff == "Yes") ylab(paste0("Natural log ", input$corr_sel_y))} +            
            {if(input$corr_sel_y_ff == "No") ylab(paste0("", input$corr_sel_y))} +            
            theme_gg() +
            theme(aspect.ratio = 0.8) +
            theme(legend.position = "none") +
            theme(plot.background=element_rect(fill=background_hex))
        
        
        
        
        return(p)
    }, bg = background_hex)
    
    
    
    output$corr_corr <- renderUI({
        loading(input$corr_sel_city)
      dataframe_length_check()
        req(input$corr_sel_x)
        
        data_ <- reactive_data() %>% 
          filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
          select(input$corr_sel_x, input$corr_sel_y) %>% 
          drop_na()
      
          
        
        

        # library(mgcv)
        
        # summary(gam(get(y) ~ get(x), data = data_))
        x <- input$corr_sel_x
        y <- input$corr_sel_y
        
        if(input$corr_sel_x_ff == "Yes"){
          data_[[x]] <- log(data_[[x]])
        }
        if(input$corr_sel_y_ff == "Yes"){
          data_[[y]] <- log(data_[[y]])
        }
        # reg
        # loess(as.formula(get(y) ~ get(x)), data = data_)
        # 
        # reg <- lm(get(y) ~ get(x) + I(get(x)^2), data = data_)
        # 
        # reg <- loess(data_[[y]], data_[[x]])
        # reg <- loess(formula = data_[[y]] ~ data_[[x]])
        # a <- summary(reg)
        # 
        data_ <- data_ %>%      
          filter_all(all_vars(!is.infinite(.))) %>% 
          drop_na()
          
        if(input$corr_regressionline_type == 'linear')
        {reg <- lm(get(y) ~ get(x), data = data_)}
        if(input$corr_regressionline_type == 'lowess')
        {reg <- loess(data_[[y]] ~ data_[[x]])}
        if(input$corr_regressionline_type == 'quadratic')
        {reg <- lm(get(y) ~ get(x) + I(get(x)^2), data = data_)}
        if(input$corr_regressionline_type == 'cubic')
        {reg <- lm(get(y) ~ get(x) + I(get(x)^2) + I(get(x)^3), data = data_)}
    
        r2 <- cor(data_[[y]], predict(reg))^2
    
        
        HTML(paste0("<h3> R squared: <b> ", round(r2, digits = 2), "</b> </h3>"))
        
        })
 

    output$corr_corr_factor <- renderUI({
      loading(input$corr_sel_city)
      dataframe_length_check()
      req(input$corr_sel_x)
      
      data_ <- reactive_data() %>% 
        filter(city == input$corr_sel_city & date == input$corr_sel_date) %>% 
        select(input$corr_sel_y, input$corr_sel_factor) %>% 
        drop_na()
      
      
      
      # library(mgcv)
      
      # summary(gam(get(y) ~ get(x), data = data_))
      x <- input$corr_sel_factor
      y <- input$corr_sel_y
      # reg
      # loess(as.formula(get(y) ~ get(x)), data = data_)
      # 
      # reg <- lm(get(y) ~ get(x) + I(get(x)^2), data = data_)
      # 
      # reg <- loess(data_[[y]], data_[[x]])
      # reg <- loess(formula = data_[[y]] ~ data_[[x]])
      # a <- summary(reg)
      # 
      
      if(input$corr_sel_y_ff == "Yes"){
        data_[[y]] <- log(data_[[y]])
      }
      data_ <- data_ %>%      
        filter_all(all_vars(!is.infinite(.))) %>% 
        drop_na()
      
      reg <- lm(get(y) ~ get(x), data = data_)
  
      r2 <- cor(data_[[y]], predict(reg))^2
      
      
      HTML(paste0("<h3> R squared: <b> ", round(r2, digits = 2), "</b> </h3>"))
      
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
                selected = "Vienna")
})

### Pick a date
output$reg_sel_date <- renderUI({
    selectInput(inputId = "reg_sel_date",
                label = "Select a date:",
                choices = sort(unique(data_reg$date)),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-11-11_WEEKDAY"))
})


### Pick a date
output$reg_sel_dependent <- renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
    
    pickerInput(inputId = "reg_sel_dependent",
                label = "Select a dependent variable:",
                choices = selectable_names,
                multiple = FALSE,
                selected = c("price"))
})



### Pick three variables (default: price, distance, stars)
output$reg_sel_three_variables_A <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", "",  input$reg_sel_dependent))
    
    pickerInput(inputId = "reg_sel_three_variables_A",
                label = "Select quantitative variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "stars"))
})



### Pick factor variables (default: price, distance, stars)
output$reg_sel_three_variables_factor_A <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date", "city",  ""))
    
    pickerInput(inputId = "reg_sel_three_variables_factor_A",
                label = "Select categorical variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("stars"))
})





### Pick three variables (default: price, distance, stars)
output$reg_sel_three_variables_B <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", "",  input$reg_sel_dependent))
    
    pickerInput(inputId = "reg_sel_three_variables_B",
                label = "Select quantitative variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "stars"))
})



### Pick factor variables (default: price, distance, stars)
output$reg_sel_three_variables_factor_B <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date", "city",  ""))
    
    pickerInput(inputId = "reg_sel_three_variables_factor_B",
                label = "Select categorical variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("stars"))
})




### Pick three variables (default: price, distance, stars)
output$reg_sel_three_variables_C <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", "",  input$reg_sel_dependent))
    
    pickerInput(inputId = "reg_sel_three_variables_C",
                label = "Select quantitative variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "stars"))
})



### Pick factor variables (default: price, distance, stars)
output$reg_sel_three_variables_factor_C <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date", "city",  ""))
    
    pickerInput(inputId = "reg_sel_three_variables_factor_C",
                label = "Select categorical variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("stars"))
})



interaction_terms <- c("",
                      "distance*rating", 
                           "distance*stars",
                           "distance*Offer_Cat",
                           "distance*Type",
                           "ln_distance*rating",
                           "ln_distance*stars",
                           "ln_distance*Offer_Cat",
                           "ln_distance*Type",
                           "stars*rating",
                           "District*rating",
                           "Offer_Cat*stars",
                           "Type*stars",
                           "Type*rating",
                           "price*distance",
                           "price*rating",
                           "price*stars",
                           "price*Type",
                           "ln_price*distance",
                           "ln_price*rating",
                           "ln_price*stars",
                           "ln_price*Type")

output$reg_sel_interaction_terms_A <- renderUI({
    selectable_names <- interaction_terms
    
    pickerInput(inputId = "reg_sel_interaction_terms_A",
                label = "Select interaction terms:", selected = "",
                choices = selectable_names)
})

output$reg_sel_interaction_terms_B <- renderUI({
    selectable_names <- interaction_terms
    
    pickerInput(inputId = "reg_sel_interaction_terms_B",
                label = "Select interaction terms:", selected = "",
                choices = selectable_names)
})

output$reg_sel_interaction_terms_C <- renderUI({
    selectable_names <- interaction_terms
    
    pickerInput(inputId = "reg_sel_interaction_terms_C",
                label = "Select interaction terms:", selected = "",
                choices = selectable_names)
})


reg_reg_A <- reactive({
    
    interaction_terms <- split_interaction_terms(input$reg_sel_interaction_terms_A)
    
    
    data_ <- reactive_data_reg() %>%
        filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, input$reg_sel_dependent),
               interaction_terms) %>% 
        drop_na()
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    data_ <- District_basing(data_)
    
    variables <- c(input$reg_sel_three_variables_A, interaction_terms, input$reg_sel_three_variables_factor_A)
    
    f <- as.formula(paste(input$reg_sel_dependent, paste0(variables, collapse = " + "), sep = " ~ "))
    
    reg <- lm(f, data=data_)
    
    reg
    
})

reg_reg_B <- reactive({
    interaction_terms <- split_interaction_terms(input$reg_sel_interaction_terms_B)
    
    data_ <- reactive_data_reg() %>%
        filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_B, input$reg_sel_three_variables_factor_B, input$reg_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    data_ <- District_basing(data_)
    
    variables <- c(input$reg_sel_three_variables_B, interaction_terms, input$reg_sel_three_variables_factor_B)
    f <- as.formula(paste(input$reg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg <- lm(f, data=data_)
    
    reg
    
})



reg_reg_C <- reactive({
    interaction_terms <- split_interaction_terms(input$reg_sel_interaction_terms_C)
    
    data_ <- reactive_data_reg() %>%
        filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_C, input$reg_sel_three_variables_factor_C, input$reg_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    data_ <- District_basing(data_)
    
    variables <- c(input$reg_sel_three_variables_C, interaction_terms, input$reg_sel_three_variables_factor_C)
    f <- as.formula(paste(input$reg_sel_dependent, paste0(variables, collapse = " + "), sep = " ~ "))
    
    reg <- lm(f, data=data_)
    
    reg
    
})

### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$reg_reg_table_A <- renderTable({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, 
                         split_interaction_terms(input$reg_sel_interaction_terms_A)))
    
    reg_A <- reg_reg_A()

    coeffs_A <<- reg_A$coefficients
    confint_A <- confint(reg_A, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    
    df_A <- data.frame("Name" = c(names(coeffs_A)),
                "Value" = paste0(round(unname(coeffs_A), 2), ifelse(sign(confint_A[, 1]) == sign(confint_A[, 2]), "*", "")),
               "CI 95" = c(paste0(as.character(round(confint_A[, 1], digits = 1)), " | ", as.character(round(confint_A[, 2], digits = 1))))) %>% 
        mutate(across(where(is.factor), as.character))

    # df_A[1, 2] = round(unname(coeffs_A), 2)[1]
    
    df_A
})


output$reg_r_2_reg_A <- renderUI({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    
    req(input$reg_sel_dependent)
    
    selection_check_reg(list(input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, 
                         split_interaction_terms(input$reg_sel_interaction_terms_A)))
    
    reg_A <- reg_reg_A()
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_A)$r.squared, digits = 2), "</b> </h4>"))
 
})



output$reg_reg_table_B <- renderTable({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    
    req(input$reg_sel_dependent)
    
    selection_check_reg(list(input$reg_sel_three_variables_B, input$reg_sel_three_variables_factor_B, 
                         split_interaction_terms(input$reg_sel_interaction_terms_B)))
    
    reg_B <- reg_reg_B()

    coeffs_B <- reg_B$coefficients
    confint_B <- confint(reg_B, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    df_B <- data.frame("Name" = c(names(coeffs_B)), 
                       "Value" = paste0(round(unname(coeffs_B), 2), ifelse(sign(confint_B[, 1]) == sign(confint_B[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_B[, 1]), digits = 2), " | ", as.character(round(confint_B[, 2], digits = 1))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    # df_B[1, 2] = round(unname(coeffs_B), 2)[1]
    
    df_B
})



output$reg_r_2_reg_B <- renderUI({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    
    selection_check_reg(list(input$reg_sel_three_variables_B, input$reg_sel_three_variables_factor_B, 
                       split_interaction_terms(input$reg_sel_interaction_terms_B)))

    reg_B <- reg_reg_B()

    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_B)$r.squared, digits = 2), "</b> </h4>"))
    
})



output$reg_reg_table_C <- renderTable({
    loading(input$reg_sel_city)
    
  dataframe_length_check_reg()
  
  
    selection_check_reg(list(input$reg_sel_three_variables_C, input$reg_sel_three_variables_factor_C, 
                       split_interaction_terms(input$reg_sel_interaction_terms_C)))
  
    reg_C <- reg_reg_C()
    
    
    coeffs_C <<- reg_C$coefficients
    confint_C   <- confint(reg_C, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    df_C <- data.frame("Name" = c(names(coeffs_C)), 
                       "Value" = paste0(round(unname(coeffs_C), 2), ifelse(sign(confint_C[, 1]) == sign(confint_C[, 2]), "*", "")),
                       "CI 95" = c( paste0(as.character(round(confint_C[, 1], digits = 1)), " | ", as.character(round(confint_C[, 2], digits = 1))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_C
})



output$reg_r_2_reg_C <- renderUI({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    
    selection_check_reg(list(input$reg_sel_three_variables_C, input$reg_sel_three_variables_factor_C, 
                       split_interaction_terms(input$reg_sel_interaction_terms_C)))
  
    reg_C <- reg_reg_C()
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_C)$r.squared, digits = 2), "</b> </h4>"))
    
})

output$reg_nrows_reg_A <- renderUI({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    
    data_ <- reactive_data_reg() %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, input$reg_sel_dependent))
    
    HTML(paste0('Number of observations: ', nrow(data_)))
})


output$reg_nrows_reg_B <- renderUI({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    
    data_ <- reactive_data_reg() %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, input$reg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    HTML(paste0('Number of observations: ', nrow(data_)))
})


output$reg_nrows_reg_C <- renderUI({
    loading(input$reg_sel_city)
  dataframe_length_check_reg()
  
    
    data_ <- reactive_data_reg() %>% filter(city == input$reg_sel_city & date == input$reg_sel_date) %>% 
        select(c(c("city"), input$reg_sel_three_variables_A, input$reg_sel_three_variables_factor_A, input$reg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    HTML(paste0('Number of observations: ', nrow(data_)))
})




########################### COMPARING REGRESSIONS #########################################



### Pick three variables (default: price, distance, stars)
output$compreg_sel_three_variables <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id", "",  "", input$compreg_sel_dependent))
    
    pickerInput(inputId = "compreg_sel_three_variables",
                label = "Select quantitative variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "stars"))
    
    
})



### Pick factor variables (default: price, distance, stars)
output$compreg_sel_three_variables_factor <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date", "city",  ""))
    
    pickerInput(inputId = "compreg_sel_three_variables_factor",
                label = "Select categorical variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("stars"))
})


output$compreg_sel_interaction_terms <- renderUI({
    selectable_names <- interaction_terms
    
    pickerInput(inputId = "compreg_sel_interaction_terms",
                label = "Select interaction terms:", selected = "",
                choices = selectable_names)
})


### Pick a date
output$compreg_sel_dependent <- renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
    
    pickerInput(inputId = "compreg_sel_dependent",
                label = "Select a dependent variable:",
                choices = selectable_names,
                multiple = FALSE,
                selected = c("price"))
})





### Pick a city
output$compreg_sel_city_A <- renderUI({
    selectInput(inputId = "compreg_sel_city_A",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Vienna")
})

### Pick a date
output$compreg_sel_date_A <- renderUI({
    selectInput(inputId = "compreg_sel_date_A",
                label = "Select a date:",
                choices = sort(unique(data_reg$date)),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-11-11_WEEKDAY"))
})




compreg_reg_A <- reactive({
    interaction_terms <- split_interaction_terms(input$compreg_sel_interaction_terms)
    dataframe_length_check_reg()
    
    data_ <- reactive_data_reg() %>%
        filter(city == input$compreg_sel_city_A & date == input$compreg_sel_date_A) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    data_ <- District_basing(data_)
    
    variables <- c(input$compreg_sel_three_variables, interaction_terms, input$compreg_sel_three_variables_factor)
    f <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg <- lm(f, data=data_)
    
    reg
    
})

compreg_reg_B <- reactive({
    interaction_terms <- split_interaction_terms(input$compreg_sel_interaction_terms)
    dataframe_length_check_reg()
    
    data_ <- reactive_data_reg() %>%
        filter(city == input$compreg_sel_city_B & date == input$compreg_sel_date_B) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    data_ <- District_basing(data_)
    
    variables <- c(input$compreg_sel_three_variables, interaction_terms, input$compreg_sel_three_variables_factor)
    f <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg <- lm(f, data=data_)
    
    reg
    
})

compreg_reg_C <- reactive({
    interaction_terms <- split_interaction_terms(input$compreg_sel_interaction_terms)
    dataframe_length_check_reg()
    
    data_ <- reactive_data_reg() %>% filter(city == input$compreg_sel_city_C & date == input$compreg_sel_date_C) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    data_ <- District_basing(data_)
    
    variables <- c(input$compreg_sel_three_variables, interaction_terms, input$compreg_sel_three_variables_factor)
    f <- as.formula(paste(input$compreg_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg <- lm(f, data=data_)
    
    reg
    
})


### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$compreg_reg_table_A <- renderTable({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, 
                       split_interaction_terms(input$compreg_sel_interaction_terms)))

    reg_A <- compreg_reg_A()
    
    coeffs_A <<- reg_A$coefficients
    confint_A <- confint(reg_A, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    df_A <- data.frame("Name" = c(names(coeffs_A)),
                       "Value" = paste0(round(unname(coeffs_A), 2), ifelse(sign(confint_A[, 1]) == sign(confint_A[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_A[, 1], digits = 1)), " | ", as.character(round(confint_A[, 2], digits = 1))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_A
})


output$compreg_r_2_reg_A <- renderUI({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, 
                       split_interaction_terms(input$compreg_sel_interaction_terms)))

    
    reg_A <- compreg_reg_A()
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_A)$r.squared, digits = 2), "</b> </h4>"))
    
})



### Pick a city
output$compreg_sel_city_B <- renderUI({
    selectInput(inputId = "compreg_sel_city_B",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Vienna")
})

### Pick a date
output$compreg_sel_date_B <- renderUI({
    selectInput(inputId = "compreg_sel_date_B",
                label = "Select a date:",
                choices = sort(unique(data_reg$date)),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-11-11_WEEKDAY"))
})




### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$compreg_reg_table_B <- renderTable({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, 
                       split_interaction_terms(input$compreg_sel_interaction_terms)))
  
  
    reg_B <- compreg_reg_B()
    
    coeffs_B <<- reg_B$coefficients
    confint_B <- confint(reg_B, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    
    # unname(coeffs_B)
    
    df_B <- data.frame("Name" = c(names(coeffs_B)),
                       "Value" = paste0(round(unname(coeffs_B), 2), ifelse(sign(confint_B[, 1]) == sign(confint_B[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_B[, 1], digits = 1)), " | ", as.character(round(confint_B[, 2], digits = 1))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    # df_B[1, 2] = round(unname(coeffs_B), 1)[1]
    
    df_B
})


output$compreg_r_2_reg_B <- renderUI({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, 
                       split_interaction_terms(input$compreg_sel_interaction_terms)))

    
    reg_B <- compreg_reg_B()
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_B)$r.squared, digits = 2), "</b> </h4>"))
    
})



### Pick a city
output$compreg_sel_city_C <- renderUI({
    selectInput(inputId = "compreg_sel_city_C",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Vienna")
})

### Pick a date
output$compreg_sel_date_C <- renderUI({
    selectInput(inputId = "compreg_sel_date_C",
                label = "Select a date:",
                choices = sort(unique(data_reg$date)),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-11-11_WEEKDAY"))
})




### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$compreg_reg_table_C <- renderTable({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, 
                       split_interaction_terms(input$compreg_sel_interaction_terms)))
  
    reg_C <- compreg_reg_C()
    
    coeffs_C <<- reg_C$coefficients
    confint_C <- confint(reg_C, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    

    df_C <- data.frame("Name" = c(names(coeffs_C)),
                       "Value" = paste0(round(unname(coeffs_C), 2), ifelse(sign(confint_C[, 1]) == sign(confint_C[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint_C[, 1], digits = 1)), " | ", as.character(round(confint_C[, 2], digits = 1))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    df_C
})




output$compreg_r_2_reg_C <- renderUI({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, 
                       split_interaction_terms(input$compreg_sel_interaction_terms)))

    
    reg_C <- compreg_reg_C()

    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg_C)$r.squared, digits = 2), "</b> </h4>"))
    
})


output$compreg_nrows_A <- renderUI({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    data_ <- reactive_data_reg() %>% filter(city == input$compreg_sel_city_A & date == input$compreg_sel_date_A) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    HTML(paste0('Number of observations: ', nrow(data_)))
})


output$compreg_nrows_B <- renderUI({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    data_ <- reactive_data_reg() %>% filter(city == input$compreg_sel_city_B & date == input$compreg_sel_date_B) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    HTML(paste0('Number of observations: ', nrow(data_)))
})


output$compreg_nrows_C <- renderUI({
    loading(input$compreg_sel_dependent)
  dataframe_length_check_reg()
  
    data_ <- reactive_data_reg() %>% filter(city == input$compreg_sel_city_C & date == input$compreg_sel_date_C) %>% 
        select(c(c("city"), input$compreg_sel_three_variables, input$compreg_sel_three_variables_factor, input$compreg_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    HTML(paste0('Number of observations: ', nrow(data_)))
})



########################### PREDICTIONS #########################################


### Pick a city
output$pred_sel_city <- renderUI({
    selectInput(inputId = "pred_sel_city",
                label = "Select a city:",
                choices = unique(data_reg$city),
                multiple = FALSE,
                selectize = TRUE,
                selected = "Vienna")
})

### Pick a date
output$pred_sel_date <- renderUI({
    selectInput(inputId = "pred_sel_date",
                label = "Select a date:",
                choices = sort(unique(data_reg$date)),
                multiple = FALSE,
                selectize = TRUE,
                selected = c("2017-11-11_WEEKDAY"))
})

### Pick a date
output$pred_sel_dependent <- renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, T, F)], c("month", "year", "hotel_id"))
    
    pickerInput(inputId = "pred_sel_dependent",
                label = "Select a dependent variable:",
                choices = selectable_names,
                multiple = FALSE,
                selected = c("price"))
})


### Pick three variables (default: price, distance, stars)
output$pred_sel_three_variables <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(reactive_data_reg(), is.factor) == F, T, F)], c("month", "year", "hotel_id", "",  input$pred_sel_dependent))
    
    pickerInput(inputId = "pred_sel_three_variables",
                label = "Select quantitative variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("distance", "stars", "stars"))
})


### Pick factor variables (default: price, distance, stars)
output$pred_sel_three_variables_factor <-renderUI({
    selectable_names <- setdiff(names(data_reg)[ifelse(sapply(data_reg, is.factor) == F, F, T)], c("date", "city",  ""))
    
    pickerInput(inputId = "pred_sel_three_variables_factor",
                label = "Select categorical variables:",
                choices = selectable_names,
                multiple = TRUE,
                selected = c("stars"))
})


output$pred_sel_interaction_terms <- renderUI({
    selectable_names <- interaction_terms
    
    pickerInput(inputId = "pred_sel_interaction_terms",
                label = "Select interaction terms:", selected = "",
                choices = selectable_names)
})


### TABLE WITH 95% CI & ALL COEFFICIENTS & R
output$pred_table <- renderTable({
    loading(input$pred_sel_city)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$pred_sel_three_variables, input$pred_sel_three_variables_factor, 
                       split_interaction_terms(input$pred_sel_interaction_terms)))
  
    interaction_terms <- split_interaction_terms(input$pred_sel_interaction_terms)
    
    data_ <- reactive_data_reg() %>% filter(city == input$pred_sel_city & date == input$pred_sel_date) %>% 
        select(c(c("city"), input$pred_sel_three_variables, input$pred_sel_three_variables_factor, input$pred_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))

    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    data_ <- District_basing(data_)
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    variables <- c(input$pred_sel_three_variables, interaction_terms, input$pred_sel_three_variables_factor)
    f <- as.formula(paste(input$pred_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    
    
    reg <<- lm(f, data=data_)
    
    coeffs <<- reg$coefficients
    confint <- confint(reg, level = 0.9) # for some reason 0.9 will give 0.95 in reality
    

    df <- data.frame("Name" = c(names(coeffs)),
                       "Value" = paste0(round(unname(coeffs), 2), ifelse(sign(confint[, 1]) == sign(confint[, 2]), "*", "")),
                       "CI 95" = c(paste0(as.character(round(confint[, 1], digits = 1)), " | ", as.character(round(confint[, 2], digits = 1))))) %>% 
        mutate(across(where(is.factor), as.character))
    
    # df[1, 2] = round(unname(coeffs), 2)[1]
    
    df
})


output$pred_r_2_reg <- renderUI({
    loading(input$pred_sel_city)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$pred_sel_three_variables, input$pred_sel_three_variables_factor, 
                       split_interaction_terms(input$pred_sel_interaction_terms)))
    interaction_terms <- split_interaction_terms(input$pred_sel_interaction_terms)
    
    
    data_ <- reactive_data_reg() %>% filter(city == input$pred_sel_city & date == input$pred_sel_date) %>% 
        select(c(c("city"), input$pred_sel_three_variables, input$pred_sel_three_variables_factor, input$pred_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    data_ <- District_basing(data_)
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    
    variables <- c(input$pred_sel_three_variables, interaction_terms, input$pred_sel_three_variables_factor)
    f <- as.formula(paste(input$pred_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    
    reg <<- lm(f, data=data_)
    
    
    HTML(paste0("<h4> R squared: </h4> <b> <h4> ",round(summary(reg)$r.squared, digits = 2), "</b> </h4>"))
    
})


output$pred_best_deals <- renderTable({
    loading(input$pred_sel_city)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$pred_sel_three_variables, input$pred_sel_three_variables_factor, 
                       split_interaction_terms(input$pred_sel_interaction_terms)))
    
    interaction_terms <- split_interaction_terms(input$pred_sel_interaction_terms)
    
    data_ <- reactive_data_reg() %>% filter(city == input$pred_sel_city & date == input$pred_sel_date) %>% 
        select(c(c("city"), input$pred_sel_three_variables, input$pred_sel_three_variables_factor, input$pred_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    data_ <- District_basing(data_)
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    
    variables <- c(input$pred_sel_three_variables, interaction_terms, input$pred_sel_three_variables_factor)
    f <- as.formula(paste(input$pred_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    
    
    reg <<- lm(f, data=data_)
    
    data_ <- data_ %>% 
        mutate(prediction = predict(reg),
               resid = get(input$pred_sel_dependent) - prediction)
    
    data_$bestdeals <- ifelse(data_$resid %in% tail(sort(data_$resid, decreasing=TRUE),5),TRUE,FALSE)
    
    data_ %>% filter(bestdeals == T) %>% select(-bestdeals) %>% dplyr::arrange(resid)

})



output$pred_worst_deals <- renderTable({
    loading(input$pred_sel_city)    
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$pred_sel_three_variables, input$pred_sel_three_variables_factor, 
                       split_interaction_terms(input$pred_sel_interaction_terms)))  
  
    interaction_terms <- split_interaction_terms(input$pred_sel_interaction_terms)
    
    data_ <- reactive_data_reg() %>% filter(city == input$pred_sel_city & date == input$pred_sel_date) %>% 
        select(c(c("city"), input$pred_sel_three_variables, input$pred_sel_three_variables_factor, input$pred_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    
    
    data_ <- District_basing(data_)
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    
    variables <- c(input$pred_sel_three_variables, interaction_terms, input$pred_sel_three_variables_factor)
    f <- as.formula(paste(input$pred_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    reg <<- lm(f, data=data_)
    
    data_ <- data_ %>% 
        mutate(prediction = predict(reg),
               resid = get(input$pred_sel_dependent) - prediction)
    
    data_$worstdeals <- ifelse(data_$resid %in% tail(sort(data_$resid, decreasing=FALSE),5),TRUE,FALSE)
    
    data_ %>% filter(worstdeals == T) %>% select(-worstdeals)  %>% dplyr::arrange(desc(resid))
    
})


    
output$pred_plot <- renderPlot({
    loading(input$pred_sel_city)
  dataframe_length_check_reg()
  
    selection_check_reg(list(input$pred_sel_three_variables, input$pred_sel_three_variables_factor, 
                       split_interaction_terms(input$pred_sel_interaction_terms)))
  
    interaction_terms <- split_interaction_terms(input$pred_sel_interaction_terms)
    
    data_ <- reactive_data_reg() %>% filter(city == input$pred_sel_city & date == input$pred_sel_date) %>% 
        select(c(c("city"), input$pred_sel_three_variables, input$pred_sel_three_variables_factor, input$pred_sel_dependent),
               interaction_terms) %>% 
        drop_na() %>%         
        filter_all(all_vars(!is.infinite(.)))
    
    
    data_ <- Filter(function(x)(length(unique(x))>1), data_)
    
    data_ <- District_basing(data_)
    
    if(length(interaction_terms) == 0){
      interaction_terms <- c()
    }
    else{
      interaction_terms <- paste0(interaction_terms, collapse = " * ")
    }
    
    
    variables <- c(input$pred_sel_three_variables, interaction_terms, input$pred_sel_three_variables_factor)
    f <- as.formula(paste(input$pred_sel_dependent, paste(variables, collapse = " + "), sep = " ~ "))
    
    
    
    reg <<- lm(f, data=data_)
    
    data_ <- data_ %>% 
        mutate(prediction = predict(reg),
               resid = get(input$pred_sel_dependent) - prediction)
    
    
    data_$worstdeals <- ifelse(data_$resid %in% tail(sort(data_$resid, decreasing=FALSE),5),TRUE,FALSE)
    data_$bestdeals <- ifelse(data_$resid %in% tail(sort(data_$resid, decreasing=TRUE),5),TRUE,FALSE)
    
    data_ <- data_ %>% mutate(deal_type = ifelse(worstdeals == T, 'The 5 worst deals', ifelse(bestdeals == T, 'The 5 best deals',
                              'Not extreme')))
    
    
    p <- data_ %>% ggplot(aes(x = prediction,
                         y = get(input$pred_sel_dependent))) +
        geom_point(aes(color = deal_type,
                       fill = deal_type,
                       size = deal_type),
                   alpha = 0.3,
                   # size = 3,
                   show_legend = T) +
        geom_smooth(method = "lm", formula = y ~ x, color = color[4], se = F) +
      # scale_alpha_manual(values=c(0.3, 1, 1)) +
    
        # scale_color_manual(values = c(color[1], color[2], color[3])) +
        ggtitle(paste("Predictions vs actuals")) +
        ylab(input$pred_sel_dependent) +
        theme_gg() +
        theme(legend.position = "right",
              legend.text = element_text(size = 18),
              legend.title = element_blank()) +
      theme(aspect.ratio = 1) +
      
        theme(plot.background=element_rect(fill=background_hex)) +
      scale_fill_manual(values=c("Not extreme" = background_hex, "The 5 best deals" =  color[2], 
                                 "The 5 worst deals" = color[3])) +
      scale_color_manual(values=c("Not extreme" = color[1], "The 5 best deals" =  color[2], 
                                  "The 5 worst deals" = color[3])) +
      scale_size_manual(values = c("Not extreme" = 2, "The 5 best deals" =  4,
      "The 5 worst deals" = 4))
      
    
    return(p)
    
}, bg = background_hex)



output$pred_nrows <- renderUI({
    loading(input$pred_sel_city)
  dataframe_length_check_reg()
  
    data_ <- reactive_data_reg() %>% filter(city == input$pred_sel_city & date == input$pred_sel_date) %>% 
        select(c(c("city"), input$pred_sel_three_variables, input$pred_sel_three_variables_factor, input$pred_sel_dependent)) %>%
        drop_na() %>%
        filter_all(all_vars(!is.infinite(.)))
    HTML(paste0('Number of observations: ', nrow(data_)))
})













output$about <- renderUI({
    HTML('
    <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">About the project</span></p>
<p><br></p>
<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">This is an&nbsp;</span><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">interactive data visualization and analysis tool&nbsp;</span><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">based running on Shiny, an environment developed by RStudio. As you make click, there is an R script in the bac</span></p>
<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">kground running, calculating numbers, making plots. If interested, you may check out the code in the next panel.&nbsp;</span></p>
<p><br></p>
<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">Created by&nbsp;</span><a href="https://www.linkedin.com/in/benedek-pasztor/" style="text-decoration:none;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 122, 183); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">Benedek P&aacute;sztor</span></a><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">,&nbsp;</span><a href="https://www.linkedin.com/in/bekesgabor/" style="text-decoration:none;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 122, 183); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">G&aacute;bor B&eacute;k&eacute;s&nbsp;</span></a><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">and&nbsp;</span><a href="https://www.linkedin.com/in/gabor-kezdi-28951640/" style="text-decoration:none;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 122, 183); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">G&aacute;bor K&eacute;zdi.&nbsp;</span></a><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">P&aacute;sztor is a data scientist at Utopus Insights, a consultancy. B&eacute;k&eacute;s is Assistant Prof at CEU in Vienna, Austria, K&eacute;zdi is associate research professor at the University of Micigan in Ann Arbor, USA.&nbsp;</span><a href="https://gabors-data-analysis.com/authors/" style="text-decoration:none;"><span style="font-size: 11pt; font-family: Arial; color: rgb(17, 85, 204); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: underline; text-decoration-skip-ink: none; vertical-align: baseline; white-space: pre-wrap;">More about us.</span></a></p>
<p><br></p>
<p><br></p>
<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">This is&nbsp;</span><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">version 0.4.</span><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">&nbsp;(2020-03-09)</span></p>
<p><br></p>
<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">The application is based on the&nbsp;</span><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">textbook</span><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">&nbsp;B&eacute;k&eacute;s-K&eacute;zdi: Data Analysis for Business, Economics, and Policy, Cambridge University Press, 2021 April.&nbsp;</span><a href="http://gabors-data-analysis.com/" style="text-decoration:none;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 122, 183); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">http://gabors-data-analysis.com/</span></a><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">.&nbsp;</span></p>
<p><br></p>
<p><br></p>
<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;"><span style="border:none;display:inline-block;overflow:hidden;width:224px;height:224px;"><img src="data:image/png;base64,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" width="224" height="224"></span></span></p>
         <p><br></p>
             <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">If you have questions, you can contact us:&nbsp;</span><a href="https://gabors-data-analysis.com/contact-us/" style="text-decoration:none;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(17, 85, 204); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: underline; text-decoration-skip-ink: none; vertical-align: baseline; white-space: pre-wrap;">https://gabors-data-analysis.com/contact-us/</span></a><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">&nbsp;If you have any technical question, reach to Benedek.</span></p>
             <p><br></p>
             <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 51, 51); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">The project is created for an Executive MBA &nbsp;Data Analysis course running at the Central European University. &nbsp;</span><a href="https://emba.ceu.edu/" style="text-decoration:none;"><span style="font-size: 10.5pt; font-family: Arial; color: rgb(51, 122, 183); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">https://emba.ceu.edu</span></a><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">&nbsp;The development of this site is supported by the&nbsp;</span><a href="https://emba.ceu.edu" style="text-decoration:none;"><span style="font-size: 11pt; font-family: Arial; color: rgb(17, 85, 204); font-weight: 700; font-style: normal; font-variant: normal; text-decoration: underline; text-decoration-skip-ink: none; vertical-align: baseline; white-space: pre-wrap;">CEU EMBA&nbsp;</span></a><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">program. We also have a great&nbsp;</span><a href="https://economics.ceu.edu/program/master-science-business-analytics" style="text-decoration:none;"><span style="font-size: 11pt; font-family: Arial; color: rgb(17, 85, 204); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: underline; text-decoration-skip-ink: none; vertical-align: baseline; white-space: pre-wrap;">MS Business Analytics program</span></a><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">&nbsp;</span></p>
             <p><br></p>
             <p><br></p>
             <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;"><span style="border:none;display:inline-block;overflow:hidden;width:225px;height:225px;"><img src="data:image/png;base64,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" width="225" height="225"></span></span></p>
             <p><br></p>
             <p><br></p>
             <p><br></p>
             <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size: 11pt; font-family: Arial; color: rgb(0, 0, 0); font-weight: 700; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">About the data</span></p>
             <p><br></p>
             <p dir="ltr" style="line-height:1.38;background-color:#ffffff;margin-top:0pt;margin-bottom:0pt;padding:0pt 0pt 12pt 0pt;"><span style="font-size: 12pt; font-family: Arial; color: rgb(36, 41, 46); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">The&nbsp;</span><span style="font-size: 10pt; font-family: Consolas, sans-serif; color: rgb(36, 41, 46); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">hotels-europe</span><span style="font-size: 12pt; font-family: Arial; color: rgb(36, 41, 46); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">&nbsp;dataset includes information on price and features of hotels in 46 European cities and for 10 different dates. The dataset includes information on daily price, location, star rating and average customer review.&nbsp;</span></p>
             <p><span style="font-size: 12pt; font-family: Arial; color: rgb(36, 41, 46); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">See&nbsp;</span><a href="https://gabors-data-analysis.com/datasets/hotels-europe/" style="text-decoration:none;"><span style="font-size: 12pt; font-family: Arial; color: rgb(17, 85, 204); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: underline; text-decoration-skip-ink: none; vertical-align: baseline; white-space: pre-wrap;">more about the dataset&nbsp;</span></a><span style="font-size: 12pt; font-family: Arial; color: rgb(36, 41, 46); font-weight: 400; font-style: normal; font-variant: normal; text-decoration: none; vertical-align: baseline; white-space: pre-wrap;">from the textbook website. </span></p>
')})




output$footnote <- renderUI({
    HTML("Created by <a href = 'https://www.linkedin.com/in/benedek-pasztor/'> Benedek Pásztor</a>, 
    <a href = 'https://www.linkedin.com/in/bekesgabor/'> Gábor Békés </a> 
    and <a href = 'https://www.linkedin.com/in/gabor-kezdi-28951640/'> Gábor Kézdi</a>,
         based on the <a href = 'http://gabors-data-analysis.com/'> Data Analysis textbook  </a> by Békés and Kézdi (Cambridge UP 2021). Supported 
         by the  <a href = 'https://emba.ceu.edu/'> CEU Executive MBA </a>")
    
})


output$variable_description <- renderUI({
  if (input$filter_check == 0){
    
    HTML(paste0("<div style='background-color:", background_hex,"'>",
                '<div style="margin: 0px; padding: 0px; border: 0px; font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-variant-numeric: inherit; font-variant-east-asian: inherit; font-weight: 400; font-stretch: inherit; font-size: 12pt; line-height: inherit; font-family: Calibri, Arial, Helvetica, sans-serif; vertical-align: baseline; color: black; letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: transparent; text-decoration-thickness: initial; text-decoration-style: initial; text-decoration-color: initial;"><h3> Variable description: </h3> <u>Quantitative variables</u>: <strong>Price&nbsp;</strong>is USD price for a night. <strong>Distance&nbsp;</strong>is from city center in miles, <strong>Distance_alter&nbsp;</strong>is distance from alternative center in miles. <strong>Rating&nbsp;</strong>is average user rating. <strong>N_rating_review&nbsp;</strong>is the count of user ratings on the website.&nbsp;<div style="margin: 0px; padding: 0px; border: 0px; font: inherit; vertical-align: baseline; color: inherit;"><u><br>Categorical variables</u>: <strong>Stars&nbsp;</strong>is the number of stars for the hotel (3.5 means ***+). <strong>Offer_cat&nbsp;</strong>is various offer rates for the night, Type is accommodation type of room. <strong>City_actual</strong> is binary, 1 if hotel is in the city, 0 if satellite village. <strong>District&nbsp;</strong>is city part as defined by city.</div><u><br>Fine print</u>. Offer categories have been condensed. Apartments include vacation home and condos. Guest House/B&amp;B also includes pension, <span style="margin: 0px; padding: 0px; border: 0px; font: inherit; vertical-align: baseline; color: inherit;">inns and apartman-hotels</span>. The data exclude other types of accommodation like boats, or when star rating is below 2 or missing.</div>
<div style="margin: 0px; padding: 0px; border: 0px; font-style: normal; font-variant-ligatures: normal; font-variant-caps: normal; font-variant-numeric: inherit; font-variant-east-asian: inherit; font-weight: 400; font-stretch: inherit; font-size: 15px; line-height: inherit; font-family: "Segoe UI Web (East European)", "Segoe UI", -apple-system, BlinkMacSystemFont, Roboto, "Helvetica Neue", sans-serif; vertical-align: baseline; color: rgb(32, 31, 30); letter-spacing: normal; orphans: 2; text-align: start; text-indent: 0px; text-transform: none; white-space: normal; widows: 2; word-spacing: 0px; -webkit-text-stroke-width: 0px; background-color: transparent; text-decoration-thickness: initial; text-decoration-style: initial; text-decoration-color: initial;"><br class="Apple-interchange-newline"></div>'))
  }
  else{HTML("")}
    })

output$source <- renderUI({
    list(
        aceEditor("server",
                  value = paste(readLines("server.R"), collapse="\n"),
                  mode="r", theme="cobalt", height = '700px',
                  readOnly=TRUE),
        aceEditor("ui",
                  value = paste(readLines("ui.R"), collapse="\n"),
                  mode="r", theme="cobalt", height = '700px',
                  readOnly=TRUE)
    )
})


output$tab_description <- renderUI({
  loading(input$filter_check)
  
  if(input$tabs == 'desc'){
    text <- '<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><h3> Tab description: </h3><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:700;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;"> Describe a city</span></p>
         <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;">We show descriptive statistics for a single city and date. Sd is standard deviation, N_unique is the number of unique values for a categorical variable, top counts is a list of the most frequent values and their count.&nbsp;</span></p>'
  }
  else if(input$tabs == 'comp'){
    text <-    '<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><h3> Tab description: </h3><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:700;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;"> Compare two cities</span></p>
         <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;">We show descriptive statistics for for two cities thus allowing for a comparison. Sd is standard deviation, N_unique is the number of unique values for a categorical variable, top counts is a list of the most frequent values and their count.&nbsp;</span></p>'
      
  }
  else if(input$tabs == 'corr'){
    text <-          '<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><h3> Tab description: </h3><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:700;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;"> Association</span></p>
         <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;">We show patterns of association between two variables. In the first panel, we have two qualitative variables, a scatterplot and a regression line. In the second panel, we have a qualitative variable on the y axis, and a categorical variable on the x-axis, the boxplot shows the conditional mean, and the inter-quartile range.</span></p>'
    
  }
  else if(input$tabs == 'reg'){
    text <-          '<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><h3> Tab description: </h3><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:700;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;"Multivariate regression</span></p>
         <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;">Here we can estimate three different regressions for the same city and date. Estimated with OLS. Coefficients and the 95% confidence interval presented. Regarding categorical variables, the base for comparison are: Apartment type: hotel, Neighborhood: largest one, Offer_cat: no-offer, Stars: 3, city_actual =yes, stars=3stars. For quantitative variables, log values may also be selected.&nbsp;</span></p>'
  }
  else if(input$tabs == 'compreg'){
    text <-          '<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><h3> Tab description: </h3><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:700;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;"> Compare regs</span></p>
         <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;">Comparing regressions allows running the same regression on different samples, different cities or different dates. Estimated with OLS. Coefficients and the 95% confidence interval presented. Regarding categorical variables, the base for comparison are: Apartment type: hotel, Neighborhood: largest one, Offer_cat: no-offer, Stars: 3, city_actual =yes, stars=3stars. For quantitative variables, log values may also be selected.&nbsp;</span></p>'
    
  }
  else if(input$tabs == 'pred'){
    text <- '<p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><h3> Tab description: </h3><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:700;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;">Prediction</span></p>
      <p dir="ltr" style="line-height:1.38;margin-top:0pt;margin-bottom:0pt;"><span style="font-size:11pt;font-family:Arial;color:#000000;background-color:transparent;font-weight:400;font-style:normal;font-variant:normal;text-decoration:none;vertical-align:baseline;white-space:pre;white-space:pre-wrap;">We estimate a single regression for a city and date. Use the model to show predicted values. This allows comparing observed and predicted values, as well as residuals - difference between predicted an actual values. Best and worst deals are defined by largest residuals: Best deals are when actual values is way lower than predicted.&nbsp;</span></p>'
  }
  else{text <- ""}
  
  
  if (input$filter_check == 0){
  HTML(paste0("<div style='background-color:", background_hex,"'>",
              text))
  }
  else{HTML("")}
})

})
  