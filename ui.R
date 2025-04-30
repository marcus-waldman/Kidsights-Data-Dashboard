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
library(markdown)
library(ggiraph)
library(tigris)
require(sf)




# Define UI for application that draws a histogram
fluidPage(
  
  column(width = 1), 
  
  column(
    width = 10, 
    fluidRow(
      width = 10, 
      imageOutput("logo", inline = T, fill = T)
    ),
    fluidRow(
      width = 10, 
      navbarPage(
        "", 
        inverse = F,
        
        #### API ####
        tabPanel(
          "API", 
          fileInput("auth", label = "REDCap API:", accept = ".csv"),  
          shinycssloaders::withSpinner(tableOutput("retention"))
        ), 


        #### Vetting ####
        tabPanel(
          "Vetting", 
          shinycssloaders::withSpinner(DT::dataTableOutput("vetting_summary"))
        ), 


        #### Sampling ####
        navbarMenu(
          "Sampling Strata", 
          tabPanel(
            "Education", 
            shinycssloaders::withSpinner(
              #girafeOutput("sample_sizes_barchart", height = "1200px", width = "800px")
              plotOutput("plot_education", height = "1100px", width = "700px")
            )
          ), 
          tabPanel(
            "Race & Ethnicity",
            shinycssloaders::withSpinner(
              #girafeOutput("sample_sizes_barchart", height = "1200px", width = "800px")
              plotOutput("plot_race", height = "1100px", width = "700px")
            )
          ), 
          tabPanel(
            "Geography",
            checkboxGroupInput("geo_ages",
              "Ages:", 
              choices = c("0-11 mo.", "12-23 mo.", "24-35 mo.", "36-47 mo.", "48-59 mo.", "60-71 mo."), 
              selected = c("0-11 mo.", "12-23 mo.", "24-35 mo.", "36-47 mo.", "48-59 mo.", "60-71 mo."), 
              inline = T
            ), 
            shinycssloaders::withSpinner(
              #girafeOutput("sample_sizes_barchart", height = "1200px", width = "800px")
              plotOutput("plot_geo")
            )
          )
        ), 
        
        
        header = tags$head(
          tags$style(
            HTML(
              ".navbar {
                background-color: #D2D2D2 !important; /* Change to desired color */
              }
              .navbar-default .navbar-nav > li > a {
                color: black !important; /* Change text color */
              }
              "
           )
          )
        )
        
      )
    )
  ), 
  
  column(width = 1), 
  
  
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Century Gothic', sans-serif !important;
      }
    "))
  )
  
  
)
