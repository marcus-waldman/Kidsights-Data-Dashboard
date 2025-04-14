#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Kidsights Data Dashboard"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fileInput("auth", label = "REDCap API:", accept = ".csv")
        ),

        # 
        mainPanel(
          # Retention analysis
            tableOutput("retention"), 
          # Sample sizes by selected demographic
            #selectInput("stratum", label = "Select a demographic", choices = c("education", "race/ethnicity")), 
            plotOutput("sample_sizes_barchart")
        )
        
        
    )
)
