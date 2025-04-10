rm(list = ls())

setwd("C:/Users/waldmanm/git-repositories/Kidsights-Data-Dashboard/")

library(tidyverse)
#library(redcapAPI)

sourced = map(.x=list.files("utils/", full.names = T), .f = function(ufile){source(ufile)})

options(keyring_backend=keyring::backend_file)


my_API = readr::read_csv(file = "C:/Users/waldmanm/my-APIs/kidsights_redcap_api.csv")

#library(tidyREDCap)
#tidyREDCap::import_instruments(url = "https://unmcredcap.unmc.edu/redcap/api/", token = my_API$api_code[1])
#m3 = module_3_child_information





library(REDCapR)
result = REDCapR::redcap_read(redcap_uri = "https://unmcredcap.unmc.edu/redcap/api/", token =  my_API$api_code[1])
dat = result$data %>%  dplyr::mutate(pid = my_API$pid[1]) %>% dplyr::relocate(pid)
# Above results in a dataframe from values (not labels)



#!/usr/bin/env Rscript
url <- "https://unmcredcap.unmc.edu/redcap/api/"
formData <- list("token"=my_API$api_code[1],
                 content='metadata',
                 format='json',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
dict <- httr::content(response)
print(dict)
# results in a dictionary in list format

for(i in 1:length(dict)){
  names(dict)[i] = dict[[i]]$field_name
}

dict[["sq002"]]


dat = dat %>% recode_it(dict = dict)

