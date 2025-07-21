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
library(DT)
library(jtools)
library(extrafont) 
library(ggiraph)
library(tigris)
require(sf)
require(readxl) 
library(haven)


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
            filter_include_exclude(dict=proj_list$dictionary, elig_list=proj_list$vetting) %>% 
            recode_it(dict = proj_list$dictionary) 
          
          return(list(proj_list = proj_list, dat = dat))
    })
    
    
    output$retention<-renderTable({
      make_retention_table(elig_list = plist()$proj_list$vetting)
    })
   
    output$plot_education<- renderPlot({#renderGirafe({
      make_sample_sizes_barcharts(df = plist()$dat, var = "education")
    })
    
    output$plot_race<- renderPlot({#renderGirafe({
      make_sample_sizes_barcharts(df = plist()$dat, var = "race")
    })
    
    
    output$plot_geo <- renderPlot({#renderGirafe({
      make_geography_plot(df = plist()$dat, years_keep = input$geo_ages %>% mobins2yrs())
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
}
