#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

rm(list = ls())



library(tidyverse)
library(shiny)
library(REDCapR)
library(ggthemes)
library(writexl)
library(shinycssloaders)
library(DT)
library(jtools)
library(extrafont) 
library(ggiraph)
library(tigris)
library(sf)
library(readxl) 
library(haven)
library(mirt)
library(shinyWidgets)
library(shinychat)
library(ellmer)

#my_API = if(file.exists("C:/my-APIs/kidsights_redcap_api.csv")) readr::read_csv("C:/my-APIs/kidsights_redcap_api.csv")
#my_API = if(file.exists("C:/Users/waldmanm/my-APIs/kidsights_redcap_api.csv")) readr::read_csv("C:/Users/waldmanm/my-APIs/kidsights_redcap_api.csv")

sourced = purrr::map(.x=list.files("utils/", full.names = T), .f = function(ufile){source(ufile)})
options(keyring_backend=keyring::backend_file)

ne_counties <<- readr::read_rds("data/ne_counties.rds")
zcta <<- readr::read_rds("data/zcta.rds")
codebook <<- readxl::read_excel(path = file.path("data", "codebook.xlsx"), sheet = "codebook")
calibdat <<- readr::read_rds("data/calibdat.rds") %>% dplyr::mutate(across(everything(), haven::zap_formats))
form <<-readr::read_rds("data/form.rds")
  
# Define server logic required to draw a histogram
function(input, output, session) {
    output$logo<-renderImage({
      image_path <- file.path("branding","dashboard-logo.png")
      
      list(
        src = image_path, 
        contentType = "image/png", width = .25*2332, height = .25*590
      )
    }, deleteFile = F)
  
    plist<-
      reactive({
          req(input$auth)
          ext<-tools::file_ext(input$auth$name)
          my_API = switch(
            ext,
            csv = readr::read_csv(input$auth$datapath), 
            validate("Invalid file type. File must be a .csv file.")
          )
          proj_list = download_vet_responses(my_API=my_API, codebook=codebook)
          dat = proj_list$data %>%   
            include_exclude(dict=proj_list$dictionary, elig_list=proj_list$vetting) %>% 
            recode_it(dict = proj_list$dictionary) 
          metadata <- create_variable_metadata(dat = dat, dict = proj_list$dict, what = "all")
          summary_table <- create_variable_summary_table(metadata)
          return(list(proj_list = proj_list, dat = dat, metadata = metadata, summary_table = summary_table))
    })
    
    
    output$retention<-renderTable({
      make_retention_table(elig_list = plist()$proj_list$vetting)
    })
   
     output$plot_education<- renderPlot({#renderGirafe({
       make_sample_sizes_barcharts(df = plist()$dat %>% filter_include_exclude(), var = "education")
     })

     output$plot_race<- renderPlot({#renderGirafe({
       make_sample_sizes_barcharts(df = plist()$dat %>% filter_include_exclude(), var = "race")
     })

    
     output$plot_fpl<- renderPlot({#renderGirafe({
       make_sample_sizes_barcharts(df = plist()$dat %>% filter_include_exclude(), var = "fpl")
     })
     
    # for(str in c("education", "race", "fpl")) {
    #   output[[paste0("plot_", str)]] <-  renderPlot({#renderGirafe({
    #     make_sample_sizes_barcharts(df = plist()$dat, var = str)
    #   })
    # }
    # 
    output$plot_geo <- renderPlot({#renderGirafe({
      make_geography_plot(df = plist()$dat %>% filter_include_exclude(), years_keep = input$geo_ages %>% mobins2yrs())
    })
    

    output$vetting_summary<- renderDataTable({
      DT::datatable(plist()$proj_list$vetting$summary %>%
                      dplyr::left_join(
                        plist()$proj_list$vetting$mailing, by = c("pid","record_id")
                      ), 
                    extensions = "Buttons",
                    options = list(
                      paging = TRUE,
                      scrollX=TRUE,
                      searching = TRUE,
                      ordering = TRUE,
                      dom = 'l<"sep">Bfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf'),
                      pageLength=nrow(plist()$proj_list$vetting$summary),
                      lengthMenu=c(10,20,nrow(plist()$proj_list$vetting$summary)) 
                    )
      )
    })
    
    
    output$ai_inputs <- renderUI({
      # Won’t run until recap_data() is available
      #req(plist())
      tagList(
        fileInput("ai_auth", label = "Anthropic API:", accept = ".csv") 
      )
    })

    ai_plist<-
      reactive({
        req(input$ai_auth)
        ext<-tools::file_ext(input$ai_auth$name)
        AI_API = switch(
          ext,
          csv = readr::read_csv(input$ai_auth$datapath), 
          validate("Invalid file type. File must be a .csv file.")
        )
        Sys.setenv(ANTHROPIC_API_KEY = AI_API$api)
      })
    

    
    output$ai_prompt<-renderUI({
      req(ai_plist())
      tagList(
        selectInput(
          inputId  = "ai_model",
          label    = "Choose ChatGPT model:",
          choices  = ellmer::models_anthropic()$id,
          selected = "claude-sonnet-4-20250514"
        ),
        pickerInput(
          inputId    = "ai_vars",
          label      = "My plot will only require data on the following topics:",
          choices    = c("Inclusion criteria","Caregiver race/ethnicity", "Caregiver education", "Household income", "Geography","Child's race/ethnicity", "Child's age", "Child's sex","Participation date", "Redcap Project ID", "Survey attrition"),
          multiple   = TRUE,
          options    = pickerOptions(
            liveSearch  = TRUE,
            actionsBox  = TRUE
          )
        ),
        textAreaInput(
          inputId   = "ai_prompt",
          label     = tags$span(
            class = "ai-prompt-label",
            "What would you like to plot?"
          ),
          value     = "",
          placeholder = "e.g. “Make me a plot that allows me to see if there are differences in survey attrition by race…”",
          rows      = 6,
          width     = "100%"
        ),
        actionButton("run_ai", "Run AI")
      )
    })
    
}
