
########################################################################
#
# CEU "CEU DA Interactive Visualization
# UI functions DEV
# Benedek PASZTOR 2021-01-04 - 
#
########################################################################

library(shiny)
library(ggplot2)
library(pander)
library(plotly)
library(shinythemes)
library(shinyWidgets)
source("theme_bg.R")

line_color <- 'black'

background_hex <- "#f2e6d9"

# Define UI for application that draws a histogram
shinyUI(fluidPage( theme = shinytheme("lumen"),
                   shinyWidgets::setBackgroundColor(background_hex),
        
                  
    # Application title
    titlePanel("Gabors Interactive Data Analysis"),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
        # sidebarPanel(
        #     width = 1
        #     
        #     
        # )
        
        mainPanel(
            tags$style(HTML(" .tabbable > .nav > li > a[data-value='summary'] {background-color: orange;} .tabbable > .nav > li > a[data-value='plot'] {background-color: red;} .tabbable > .nav > li > a[data-value='table'] {background-color: green;} .tabbable > .nav > li[class=active] > a {background-color: #2432d5;} ")),
            
            tabsetPanel(type = "pills",
                        
                        
                        
                      
                                           tabPanel("Describe data for a city",
                                 fluidPage(
                                     fluidRow(
                                         column(3,
                                                h3("Data selection"),
                                                uiOutput('desc_sel_city'),
                                                uiOutput('desc_sel_date'),
                                                uiOutput('desc_sel_three_variables'),
                                                uiOutput('desc_sel_three_variables_factor')),
                                         column(3, 
                                                h3("Data filtering"),
                                                uiOutput('desc_filter_check'),
                                                uiOutput('desc_filters')),
                                         column(3,
                                                h3("Selected and filtered data summary"),
                                                fluidRow(
                                                    htmlOutput("desc_nrows"),
                                                    tableOutput("desc_summary"),
                                                tableOutput("desc_summary_factor"),
                                                style='margin-bottom:30px;border:1px solid; padding: 10px;'))
                                     ),
                                     fluidRow(
                                         tags$hr(style=paste0("border-color:", line_color)), 
                                     ),
                                    fluidRow(
                                        h3("Selected and filtered data histograms"),
                                         plotOutput("desc_histogram"),
                                        plotOutput("desc_histogram_factor"))
                                 )),
                            
                            tabPanel("Compare two cities",
                                     fluidPage(
                                         fluidRow(
                                             column(3,
                                                    h3("Data selection"),
                                                    uiOutput('comp_sel_cities'),
                                                    uiOutput('comp_sel_date'),
                                                    uiOutput('comp_sel_three_variables'),
                                                    uiOutput('comp_sel_three_variables_factor')),
                                             column(3, 
                                                    h3("Data filtering"),
                                                    uiOutput('comp_filter_check'),
                                                    uiOutput('comp_filters')),
                                             column(3,
                                                    h3("Selected and filtered data summary"),
                                                    h4(textOutput('comp_city_1')),
                                                    fluidRow(
                                                        htmlOutput("comp_nrows_1"),
                                                        tableOutput("comp_summary_1"),
                                                             tableOutput("comp_summary_factor_1"),
                                                             style='margin-bottom:30px;border:1px solid; padding: 10px;')),
                                             
                                             column(3,
                                                    h3("Selected and filtered data summary"), 
                                                    h4(textOutput('comp_city_2')),
                                                    fluidRow(
                                                        htmlOutput("comp_nrows_2"),
                                                        tableOutput("comp_summary_2"),
                                                     tableOutput("comp_summary_factor_2"),
                                                     style='margin-bottom:30px;border:1px solid; padding: 10px;'))
                                         ),
                                         fluidRow(
                                             tags$hr(style=paste0("border-color:", line_color)), 
                                         ),
                                         fluidRow(
                                             h3("Selected and filtered data histograms"),
                                             plotOutput("comp_histogram"))
                                     )),
                                 
                        tabPanel("Correlation", 

                                 fluidPage(
                                     fluidRow(
                                         h3("Data selection"),
                                         column(4, uiOutput('corr_sel_city'),
                                                uiOutput('corr_sel_date')))),
                                 
                                 
                                 fluidPage(
                                     fluidRow(
                                         column(4, ""),
                                         column(4, 
                                                htmlOutput("corr_nrows"),
                                                uiOutput('corr_filter_check')))),
                                                
                                 
                                 fluidPage(
                                     fluidRow(
                                         h3("Data selection, manipulation and filtering"),
                                         
                                         column(2,uiOutput('corr_sel_x')),
                                         column(2, uiOutput('corr_sel_x_ff')),
                                         column(2, uiOutput('corr_sel_x_filter'))),
                                 
                                     fluidRow(
                                         column(2, uiOutput('corr_sel_y')),
                                         column(2, uiOutput('corr_sel_y_ff')),
                                         column(2, uiOutput('corr_sel_y_filter')))),
                                 
                            
                                 tags$hr(style=paste0("border-color:", line_color)),
                                 
                                fluidPage(
                                    fluidRow(
                                        column(12,
                                    h3("Scatterplot and fitted regression line"),
                                         uiOutput('corr_regressionline_type'),
                                        uiOutput('corr_ci'),
                                    
                                         htmlOutput("corr_corr"),
                                         plotOutput("corr_scatterplot"))))),
                        
                        
                        tabPanel("Multivariate regression", 
                                 fluidPage(
                                     fluidRow(
                                         column(3,
                                                h3("Data selection"),
                                                uiOutput('reg_sel_city'),
                                                uiOutput('reg_sel_date'),
                                               uiOutput('reg_sel_dependent'),
                                               uiOutput('reg_filter_check'),
                                               uiOutput('reg_filters')),
                                         
                                         column(3,
                                                h3("Regression A"),
                                                uiOutput('reg_sel_three_variables_A'),
                                                uiOutput('reg_sel_three_variables_factor_A'),
                                                uiOutput('reg_sel_interaction_terms_A'),
                                                uiOutput('reg_r_2_reg_A'),
                                                htmlOutput("reg_nrows_reg_A"),
                                                tags$hr(style=paste0("border-color:", line_color)),
                                                tableOutput('reg_reg_table_A'),
                                                "Linear regression (OLS). Values that are statistically different from zero are denoted by a star."),
                                         column(3,
                                                h3("Regression B"),
                                                uiOutput('reg_sel_three_variables_B'),
                                                uiOutput('reg_sel_three_variables_factor_B'),
                                                uiOutput('reg_sel_interaction_terms_B'),
                                                uiOutput('reg_r_2_reg_B'),
                                                htmlOutput("reg_nrows_reg_B"),
                                                tags$hr(style=paste0("border-color:", line_color)),
                                                tableOutput('reg_reg_table_B')),
                                         column(3,
                                                h3("Regression C"),
                                                uiOutput('reg_sel_three_variables_C'),
                                                uiOutput('reg_sel_three_variables_factor_C'),
                                                uiOutput('reg_sel_interaction_terms_C'),
                                                uiOutput('reg_r_2_reg_C'),
                                                htmlOutput("reg_nrows_reg_C"),
                                                tags$hr(style=paste0("border-color:", line_color)),
                                                tableOutput('reg_reg_table_C')
                                     )
                        )),
                        tags$hr(style=paste0("border-color:", line_color))),
                        
                        tabPanel("Comparing regressions",
                                 fluidPage(
                                   fluidRow(
                                     column(3,
                                            h3("Data selection"),
                                            uiOutput('compreg_sel_three_variables'),
                                            uiOutput('compreg_sel_three_variables_factor'),
                                            uiOutput('compreg_sel_interaction_terms'),
                                            uiOutput('compreg_sel_dependent')),
                                     column(3,
                                            h3("Date and city A"),
                                            uiOutput('compreg_sel_city_A'),
                                            uiOutput('compreg_sel_date_A'),
                                            uiOutput('compreg_r_2_reg_A'),
                                            htmlOutput("compreg_nrows_A"),
                                            tags$hr(style=paste0("border-color:", line_color)),
                                            tableOutput('compreg_reg_table_A'),
                                            "Linear regression (OLS). Values that are statistically different from zero are denoted by a star."),
                                     column(3,
                                            h3("Date and city B"),
                                            uiOutput('compreg_sel_city_B'),
                                            uiOutput('compreg_sel_date_B'),
                                            uiOutput('compreg_r_2_reg_B'),
                                            htmlOutput("compreg_nrows_B"),
                                            tags$hr(style=paste0("border-color:", line_color)),
                                            tableOutput('compreg_reg_table_B')),
                                     column(3,
                                            h3("Date and city C"),
                                            uiOutput('compreg_sel_city_C'),
                                            uiOutput('compreg_sel_date_C'),
                                            uiOutput('compreg_r_2_reg_C'),
                                            htmlOutput("compreg_nrows_C"),
                                            tags$hr(style=paste0("border-color:", line_color)),
                                            tableOutput('compreg_reg_table_C')),
                                            ))),
                        tabPanel("Prediction",
                                 fluidPage(
                                     fluidRow(
                                         column(3,
                                                h3("Data selection"),
                                                uiOutput('pred_sel_three_variables'),
                                                uiOutput('pred_sel_three_variables_factor'),
                                                uiOutput('pred_sel_interaction_terms'),
                                                uiOutput('pred_sel_dependent')),
                                         column(3, 
                                                h3("Data filtering"),
                                                uiOutput('pred_filter_check'),
                                                uiOutput('pred_filters')),
                                         column(3,
                                                h3("Date and city"),
                                                uiOutput('pred_sel_city'),
                                                uiOutput('pred_sel_date'),
                                                uiOutput('pred_r_2_reg'),
                                                htmlOutput("pred_nrows"),
                                                tags$hr(style=paste0("border-color:", line_color)),
                                                tableOutput('pred_table'),
                                                "Linear regression (OLS). Values that are statistically different from zero are denoted by a star.")),
                                     tags$hr(style=paste0("border-color:", line_color)),
                                     
                                                                          fluidRow(column(3,
                                                     h3("Best deals"),
                                                     tableOutput("pred_best_deals"),
                                                     h3("Worst deals"),
                                                     tableOutput("pred_worst_deals")),
                                              column(8,
                                                     plotOutput('pred_plot'))))),
                        tabPanel("About the project",
                                 uiOutput('about')),
                                           
                       tabPanel("Source code of the app",
                                uiOutput('source'))
                                           
                                 
                        ),
        
            tags$hr(style=paste0("border-color:", line_color)),
                       # tabPanel("About")
            # 
            htmlOutput("variable_description"),
            tags$hr(style=paste0("border-color:", line_color)),
            
                       fluidRow(column(12, align = "center", 
                                       htmlOutput("footnote")))
                      
            , width = '100%'
            
        
    )
))

