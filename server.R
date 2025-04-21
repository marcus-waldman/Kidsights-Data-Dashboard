#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(shiny)
library(REDCapR)
library(ggthemes)
library(writexl)
library(shinycssloaders)

sourced = purrr::map(.x=list.files("utils/", full.names = T), .f = function(ufile){source(ufile)})
options(keyring_backend=keyring::backend_file)

# Define server logic required to draw a histogram
function(input, output, session) {
    plist<-
      reactive({
          req(input$auth)
          ext<-tools::file_ext(input$auth$name)
          my_API = switch(
            ext,
            csv = readr::read_csv(input$auth$datapath), 
            validate("Invalid file type. File must be a .csv file.")
          )
          proj_list = download_vet_responses(my_API=my_API)
          dat = proj_list$data %>%   
            filter_include_exclude(dict=proj_list$dictionary, elig_list=proj_list$vetting) %>% 
            recode_it(dict = proj_list$dictionary) 
          
          return(list(proj_list = proj_list, dat = dat))
    })
    
    
    output$download<-downloadHandler(
      filename = function(){
        paste0("Kidsights_NE25_",
               Sys.time() %>% 
               stringr::str_replace_all(":","_") %>% 
               stringr::str_replace_all("\\.", "_"), 
               ".xlsx"
        )
      }, 
      content = function(file){
        writexl::write_xlsx(proj_list$vetting, path = file)
      }
    )
    
    output$retention<-renderTable({
      make_retention_table(elig_list = plist()$proj_list$vetting)
    })
   
    output$sample_sizes_barchart<-renderPlot({
      make_sample_sizes_barcharts(df = plist()$dat, var = "education")
    })
}
