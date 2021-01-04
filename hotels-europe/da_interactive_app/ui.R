
########################################################################
#
# CEU "CEU DA Interactive Visualization
# UI functions DEV
# Benedek PASZTOR 2021-01-04
#
########################################################################

library(shiny)

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
                        
                        tabPanel("Describe a variable",
                                 "Under development - Describe a variable"),
                      
                        tabPanel("Histograms", 
                                 "Under development - Histograms"),
                                 
                        tabPanel("Correlation", 
                                 "Under development - Correlation & histograms"),
                        tabPanel("Multivariate regression", 
                                 "Under development - Multivariate regression"),
                        tabPanel("Prediction", 
                                 "Under development - Correlation & histograms"))
                                 
                      
            , width = '100%'
            
        )
    )
))

