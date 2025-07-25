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
library(sf)
library(bslib)

my_theme <- bslib::bs_theme(
  version     = 5,
  #base_font   = font_google("Century Gothic"),
  bg          = "#FFFFFF",    # page background
  fg          = "#0b3474",    # default text color
  primary     = "#0b3474",    # buttons, links
  secondary   = "#6baedb" #,    # accents, hover states
#  "gray-600"  = "#D2D2D2"     # custom override for grays
)

# Define UI for application that draws a histogram
fluidPage(
  #theme = my_theme,
  
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
            "Federal Poverty level",
            shinycssloaders::withSpinner(
              #girafeOutput("sample_sizes_barchart", height = "1200px", width = "800px")
              plotOutput("plot_fpl", height = "1100px", width = "700px")
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
        
        #### AI ####
        tabPanel("AI Playground",
                 uiOutput("ai_inputs"),
                 uiOutput("ai_prompt")
                 
        ),

         header = tags$head(
           tags$style(
             HTML(
               ".navbar {
                 background-color: #FFFFFF !important; /* Change to desired color */
               }
               .navbar-default .navbar-nav > li > a {
                 color: #0b3474 !important; /* Change text color */
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
     font-color:  #D2D2D2;
    }
    .ai-prompt-label {
      font-size: 1.00em;       /* 100% of normal text */
      font-weight: 600;        /* semi-bold */
      line-height: 1.4;        /* comfortable spacing for multiple lines */
      color: #333333;          /* dark grey for better readability */
    }
    .form-group .shiny-input-container textarea {
      font-size: 1em;          /* match textarea text to your theme */
      padding: 0.5em;          /* give it some breathing room */
    }
    "))
  ), 
  
  
  
)


