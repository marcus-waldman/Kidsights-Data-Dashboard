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
library(shinychat)
library(plotly)
library(ellmer)

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
          ),
          tabPanel(
            "Crosstab",
            fluidRow(
              column(4,
                selectInput("crosstab_var1", 
                  "Select First Variable:",
                  choices = c("Race/Ethnicity" = "raceG", 
                            "Federal Poverty Level" = "fplcat", 
                            "Education" = "educ4_max"),
                  selected = "raceG")
              ),
              column(4,
                selectInput("crosstab_var2", 
                  "Select Second Variable:",
                  choices = c("Race/Ethnicity" = "raceG", 
                            "Federal Poverty Level" = "fplcat", 
                            "Education" = "educ4_max"),
                  selected = "fplcat")
              ),
              column(4,
                checkboxGroupInput("crosstab_years", 
                  "Filter by Age (years):",
                  choices = c("0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                  selected = c(0, 1, 2, 3, 4, 5),
                  inline = TRUE)
              )
            ),
            shinycssloaders::withSpinner(
              DT::dataTableOutput("crosstab_table")
            ),
            hr(),
            h4("Age Distribution of Eligible Respondents"),
            fluidRow(
              column(4,
                checkboxGroupInput("age_plot_fplcat",
                  "Filter by Federal Poverty Level:",
                  choices = c("<100% FPL" = "<100% FPL",
                            "100-199% FPL" = "100-199% FPL",
                            "200-299% FPL" = "200-299% FPL",
                            "300-399% FPL" = "300-399% FPL",
                            "400+% FPL" = "400+% FPL"),
                  selected = c("<100% FPL", "100-199% FPL", "200-299% FPL", "300-399% FPL", "400+% FPL"))
              ),
              column(4,
                checkboxGroupInput("age_plot_raceG",
                  "Filter by Race/Ethnicity:",
                  choices = c("White, non-Hisp." = "White, non-Hisp.",
                            "American Indian or Alaska Native, non-Hisp." = "American Indian or Alaska Native, non-Hisp.",
                            "Asian or Pacific Islander, non-Hisp." = "Asian or Pacific Islander, non-Hisp.",
                            "Black or African American, non-Hisp." = "Black or African American, non-Hisp.",
                            "Hispanic" = "Hispanic",
                            "NA, non-Hisp." = "NA, non-Hisp.",
                            "Some Other Race, non-Hisp." = "Some Other Race, non-Hisp.",
                            "Two or More, non-Hisp." = "Two or More, non-Hisp."),
                  selected = c("White, non-Hisp.", "American Indian or Alaska Native, non-Hisp.", "Asian or Pacific Islander, non-Hisp.", "Black or African American, non-Hisp.", "Hispanic", "NA, non-Hisp.", "Some Other Race, non-Hisp.", "Two or More, non-Hisp."))
              ),
              column(4,
                checkboxGroupInput("age_plot_educ4_max",
                  "Filter by Education:",
                  choices = c("Less than High School Graduate" = "Less than High School Graduate",
                            "High School Graduate (including Equivalency)" = "High School Graduate (including Equivalency)",
                            "Some College or Associate's Degree" = "Some College or Associate's Degree",
                            "College Degree" = "College Degree"),
                  selected = c("Less than High School Graduate", "High School Graduate (including Equivalency)", "Some College or Associate's Degree", "College Degree"))
              )
            ),
            shinycssloaders::withSpinner(
              plotlyOutput("age_distribution_plot", height = "400px")
            )
          )
        ), 
        
        #### AI ####
        tabPanel("AI Plotter (Experimental)",
                 uiOutput("ai_inputs"),
                 uiOutput("ai_prompt"), 
                 # Option 2: With error handling display
                 conditionalPanel(
                   condition = "output.ai_plot_error == ''",
                   plotOutput("ai_plot", height = "500px")
                 ),
                 conditionalPanel(
                   condition = "output.ai_plot_error != ''",
                   div(
                     class = "alert alert-danger",
                     textOutput("ai_plot_error")
                   )
                 )
                 
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


