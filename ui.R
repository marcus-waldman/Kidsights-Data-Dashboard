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

  fluidRow(width = 10, 
           column(width = 3, 
                  img(src="https://www.unmc.edu/publichealth/_images/research/multidisciplinary/kidsights/ksd-logo-whiterec.png", height="100%", width="100%", align = "center")
           ),
            column(width = 7, 
                   titlePanel("Study Dashboard")
            )
    ),
   fluidRow(width = 10, 
            tabsetPanel(
              tabPanel("API", fileInput("auth", label = "REDCap API:", accept = ".csv"),  withSpinner(tableOutput("retention"))), 
              tabPanel("Vetting", shinycssloaders::withSpinner(DT::dataTableOutput("vetting_summary"))), 
              tabPanel("Sampling", shinycssloaders::withSpinner(plotOutput("sample_sizes_barchart")))
            )
   )
   
)
