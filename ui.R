#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

rm(list = ls())

library(shiny)
library(shinycssloaders)
library(DT)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel(img(src='https://www.unmc.edu/publichealth/_images/research/multidisciplinary/kidsights/ksd-logo-whiterec.png', align = "left", height = .25*600, width = .25*900)),
    
    mainPanel(
      tabsetPanel(
        tabPanel("API", fileInput("auth", label = "REDCap API:", accept = ".csv"),  withSpinner(tableOutput("retention"))), 
        tabPanel("Vetting", shinycssloaders::withSpinner(DT::dataTableOutput("vetting_summary"))), 
        tabPanel("Sampling", shinycssloaders::withSpinner(plotOutput("sample_sizes_barchart")))
      )
    )

   
)
