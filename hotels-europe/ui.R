
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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("CEU DA Interactive Visualization"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            width = 0
            
            
        ),
        
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Describe data for a city",
                                 "Under development - Describe data for a city",
                                 
                                 uiOutput('desc_sel_city'),
                                 uiOutput('desc_sel_date'),
                                 
                                 uiOutput('desc_sel_three_variables'),
                                 
                                 
                                 plotlyOutput("desc_histogram"),
                                 verbatimTextOutput("desc_summary")
                                 ),
                            
                            tabPanel("Compare two cities", 
                                 "Under development - Compare two cities",
                                 
                                 uiOutput('comp_sel_cities'),
                                 uiOutput('comp_sel_date'),
                                 
                                 uiOutput('comp_sel_three_variables'),
                                 
                                 "TODO: filtering extreme values tickmark & extreme value definition input for each var.",
                                 
                                plotlyOutput("comp_density"),
                                 verbatimTextOutput("comp_summary")


                                 ),
                  
                                 
                        tabPanel("Correlation and scatterplotting", 
                                 "Under development - Correlation",
                                 uiOutput('corr_sel_city'),
                                 uiOutput('corr_sel_date'),
                                 
                                 uiOutput('corr_sel_x'),
                                 uiOutput('corr_sel_x_ff'),
                                 
                                 uiOutput('corr_sel_y'),
                                 uiOutput('corr_sel_y_ff'),
                                 
                                 uiOutput('corr_trendline_type'),
                                 
                                 
                                 
                                 "TODO: filtering extreme values tickmark & extreme value definition input for each var.",
                                 
                                 h3("Correlation:"),
                                 verbatimTextOutput("corr_corr"),
                                 

                                 plotlyOutput("corr_scatterplot")),
                        
                        
                        tabPanel("Multivariate regression", 
                                 "Under development - Multivariate regression"),
                        
                        
                        tabPanel("Prediction", 
                                 "Under development - Correlation & histograms"))
                                 
                      
            , width = '100%'
            
        )
    )
))

