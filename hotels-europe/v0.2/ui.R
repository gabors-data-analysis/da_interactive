# Based on code for the textbook "Data Analysis for Business, Economics, and Policy"
# Cambridge University Press, 2021
# by Gábor Békés and Gábor Kézdi
# https://gabors-data-analysis.com/
# https://github.com/gabors-data-analysis/da_case_studies

########################################################################
#
# CEU DA Interactive Visualization
# UI functions
# Benedek PASZTOR
#
########################################################################

library(shiny)
# library(ggplot2)
# library(pander)
# library(plotly)
library(shinythemes)
library(shinyWidgets)
source("theme_bg.R")
library("shinyBS")


line_color <- '#F1F2F2'

background_hex <- "#ffffff"



# Define UI for application that draws a histogram
shinyUI(fluidPage( theme = shinytheme("lumen"),
                   shinyWidgets::setBackgroundColor(background_hex),
        
                  
    # Application title
    titlePanel("Gabors Interactive Data Analysis"),
    
    fluidRow(
        column(2,
             h3(""),
             # bsModal("modalExample", "Data download", "Download", size = "large",
             #         tableOutput('table'),
             #         radioButtons("filetype", "File type:", choices = c("csv", "tsv")),
             #         downloadButton('downloadData', 'Download'))),
            h3("Data filtering:"),
            uiOutput("filter_check"),
            uiOutput("filters"),
            tags$hr(style=paste0("border-color:", line_color)), 
            uiOutput("tab_description"),
            tags$hr(style=paste0("border-color:", line_color)), 
            uiOutput("variable_description"),
            style='border-right: 1px solid #F1F2F2'            
        ),
        column(10,
            tags$style(HTML(" .tabbable > .nav > li > a[data-value='summary'] {background-color: orange;} .tabbable > .nav > li > a[data-value='plot'] {background-color: red;} .tabbable > .nav > li > a[data-value='table'] {background-color: green;} .tabbable > .nav > li[class=active] > a {background-color: #2432d5;} ")),
            
            tabsetPanel(type = "pills", id = 'tabs',
                        hr(),
                        tabPanel("Download Data", value = 'down',
                                 sidebarLayout(
                                     sidebarPanel(
                                     width = 2,
                                     theme = shinytheme("lumen"),
                                     fluidRow(
                                         column(width = 12,
                                                    h3('File download'),
                                                    uiOutput('down_sel_city'),
                                                    uiOutput('down_sel_date'),
                                                    actionButton("mybutton","Submit", class = "btn-warning"),
                                                    br(),
                                                    helpText("Click the button to update the value displayed in the main panel."),
                                                    hr(),
                                                    radioButtons("filetype", "File type:",
                                                             choices = c("csv", "tsv")),
                                                    downloadButton('downloadData', 'Download', class="btn btn-success")))),
                                     mainPanel(
                                         theme = shinytheme("lumen"),
                                         fluidRow(
                                         column(width = 11, 
                                                fluidRow(
                                                    h3("Selected and filtered data"),
                                                    htmlOutput("down_nrows"),
                                                    br(),
                                                    DT::dataTableOutput("mytable"))),
                                             )
                                         ),
                                     )
                                 ),
                                 # fluidPage(
                                 #     fluidRow(
                                 #         h3('File download'),
                                 #         sidebarLayout(
                                 #             fluid = TRUE,
                                 #             sidebarPanel(
                                 #                 uiOutput('down_sel_city'),
                                 #                 uiOutput('down_sel_date'),
                                 #                 actionButton("mybutton","Submit"),
                                 #                 br(),
                                 #                 p("Click the button to update the value displayed in the main panel."),
                                 #                 hr(),
                                 #                 radioButtons("filetype", "File type:",
                                 #                              choices = c("csv", "tsv")),
                                 #                 downloadButton('downloadData', 'Download')
                                 #                 ),
                                 #             mainPanel(
                                 #                 width = 5,
                                 #                 h3("Selected and filtered data"),
                                 #                 fluidRow(
                                 #                     htmlOutput("down_nrows"),
                                 #                     br(),
                                 #                     DT::dataTableOutput("mytable")),
                                 #                )
                                 #            ),
                                 #         )
                                 #     )
                                 # ),
        
                        tabPanel("Describe data for a city", value = 'desc',
                                 fluidPage(
                                     fluidRow(
                                         column(3,
                                                h3("Data selection"),
                                                uiOutput('desc_sel_city'),
                                                uiOutput('desc_sel_date'),
                                                uiOutput('desc_sel_three_variables'),
                                                uiOutput('desc_sel_three_variables_factor')),
                                         # column(3,
                                         #        h3("Data filtering"),
                                         #        uiOutput('desc_filter_check'),
                                         #        uiOutput('desc_filters')),
                                         column(3,
                                                h3("Selected and filtered data summary"),
                                                fluidRow(
                                                    htmlOutput("desc_nrows"),
                                                    tableOutput("desc_summary"),
                                                tableOutput("desc_summary_factor")))
                                                # style='margin-bottom:30px;border:1px solid  #3a5e8cFF; padding: 10px;'))
                                     ),
                                     fluidRow(
                                         tags$hr(style=paste0("border-color:", line_color)),
                                     ),
                                    fluidRow(
                                        h3("Selected and filtered data histograms"),
                                         plotOutput("desc_histogram"),
                                        plotOutput("desc_histogram_factor"),
                                        plotOutput("desc_histogram_factor_district", width = "100%", height = "700px")
                                    )
                                 )
                                 ),
                            
                            tabPanel("Compare two cities",  value = 'comp',
                                     fluidPage(
                                         fluidRow(
                                             column(3,
                                                    h3("Data selection"),
                                                    uiOutput('comp_sel_cities'),
                                                    uiOutput('comp_sel_date'),
                                                    uiOutput('comp_sel_three_variables'),
                                                    uiOutput('comp_sel_three_variables_factor')),
                                             # column(3, 
                                             #        h3("Data filtering"),
                                             #        uiOutput('comp_filter_check'),
                                             #        uiOutput('comp_filters')),
                                             column(3,
                                                    h3("Selected and filtered data summary"),
                                                    h4(textOutput('comp_city_1')),
                                                    fluidRow(
                                                        htmlOutput("comp_nrows_1"),
                                                        tableOutput("comp_summary_1"),
                                                             tableOutput("comp_summary_factor_1"))),
                                                             # style='margin-bottom:30px;border:1px solid  #3a5e8cFF; padding: 10px;')),
                                             
                                             column(3,
                                                    h3("Selected and filtered data summary"), 
                                                    h4(textOutput('comp_city_2')),
                                                    fluidRow(
                                                        htmlOutput("comp_nrows_2"),
                                                        tableOutput("comp_summary_2"),
                                                     tableOutput("comp_summary_factor_2")))
                                                     # style='margin-bottom:30px;border:1px solid  #3a5e8cFF; padding: 10px;'))
                                         ),
                                         fluidRow(
                                             tags$hr(style=paste0("border-color:", line_color)), 
                                         ),
                                         fluidRow(
                                             h3("Selected and filtered data histograms"),
                                             plotOutput("comp_histogram"),
                                             plotOutput("comp_histogram_factor"))
                                     )),
                                 
                        tabPanel("Associations",  value = 'corr',

                                 fluidPage(
                                     fluidRow(
                                         h3("Data selection"),
                                         column(4, uiOutput('corr_sel_city'),
                                                uiOutput('corr_sel_date')))),
                                 
                                 
                                 fluidPage(
                                     fluidRow(),
                                     fluidRow(
                                         column(4, ""),
                                         column(4, 
                                                ""))),
                                                # uiOutput('corr_filter_check')))),
                                                
                                 
                                 fluidPage(
                                     fluidRow(
                                         h3("Data selection and manipulation"),
                                         
                                         column(2,uiOutput('corr_sel_x')),
                                         column(2, uiOutput('corr_sel_x_ff')),
                                         column(2, uiOutput('corr_sel_factor'))),
                                         # column(2, uiOutput('corr_sel_x_filter'))),
                                 
                                     fluidRow(
                                         column(2, uiOutput('corr_sel_y'),
                                                htmlOutput("corr_nrows")),
                                         column(2, uiOutput('corr_sel_y_ff')))),
                                
                                 tags$hr(style=paste0("border-color:", line_color)),
                                 
                                fluidPage(
                                    fluidRow(
                                        h3("Scatterplot and fitted regression line"),
                                        uiOutput('corr_regressionline_type'),
                                        uiOutput('corr_ci'),        
                                        plotOutput("corr_scatterplot"),
                                        htmlOutput("corr_corr"),
                                        plotOutput("corr_boxplot"),
                                        htmlOutput("corr_corr_factor")
                                    ))),
                        
                        
                        tabPanel("Multivariate regression",  value = 'reg',
                                 fluidPage(
                                     fluidRow(
                                         column(3,
                                                h3("Data selection"),
                                                uiOutput('reg_sel_city'),
                                                uiOutput('reg_sel_date'),
                                               uiOutput('reg_sel_dependent')),
                                         
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
                        
                        tabPanel("Comparing regressions", value = 'compreg',
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
                        tabPanel("Prediction", value = 'pred',
                                 fluidPage(
                                     fluidRow(
                                         column(3,
                                                h3("Data selection"),
                                                uiOutput('pred_sel_three_variables'),
                                                uiOutput('pred_sel_three_variables_factor'),
                                                uiOutput('pred_sel_interaction_terms'),
                                                uiOutput('pred_sel_dependent')),
                                         # column(3, 
                                         #        h3("Data filtering"),
                                         #        uiOutput('pred_filter_check'),
                                         #        uiOutput('pred_filters')),
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
                                     
                                                                          fluidRow(column(12,
                                                     h3("Best deals"),
                                                     tableOutput("pred_best_deals"),
                                                     h3("Worst deals"),
                                                     tableOutput("pred_worst_deals"),
                                                     plotOutput('pred_plot'))))),
                        tabPanel("About the project", value = 'about',
                                 uiOutput('about')),
                                           
                       tabPanel("Source code of the app", value = 'source',
                                uiOutput('source'))
                                           
                                 
                        ),
        
            tags$hr(style=paste0("border-color:", line_color)),

                       fluidRow(column(12, align = "center", 
                                       htmlOutput("footnote"))))
                      
            # , width = '80%'
            
        
 )
))



