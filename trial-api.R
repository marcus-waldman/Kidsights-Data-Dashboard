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


elig_list = check_eligibility_authenticity(dat=dat,dict=dict)


dat1 = dat %>% 
  filter_include_exclude(dict=dict,elig_list=elig_list) %>% 
  recode_it(dict = dict)


# #utils-plots.R
# 
library(ggthemes)



elig_list$summary %>%
  dplyr::mutate(`1. Total Records` = T, `2. After Eligibility Screening` = (eligibility == "Pass"), `3. After Authenticity Screening` = (eligibility == "Pass" & authenticity == "Pass")) %>% 
  dplyr::select(-(eligibility:compensation)) %>% 
  tidyr::pivot_longer(`1. Total Records`:`3. After Authenticity Screening`) %>% 
  dplyr::filter(value) %>% 
  dplyr::group_by(name) %>% 
  dplyr::reframe(n = sum(value)) %>% 
  dplyr::mutate(`Retention %` = paste0(round(100*n/max(n))) ) %>% 
  dplyr::ungroup() 


elig_list$details %>%
  dplyr::filter(pass == F, category != "Compensation") %>% 
  dplyr::group_by(category,description) %>% 
  dplyr::summarise(n = n())


df = dat1 %>% dplyr::mutate(years_old = paste0(floor(age_in_days/365.25), " years old"))


plot1 = ggplot(df, aes(x = educ4_max, col = sex, fill = sex)) +
  geom_histogram(stat = "count") +
  facet_wrap(years_old~.) +
  coord_flip() +
  ggthemes::theme_few() +
  ggthemes::scale_colour_tableau() +
  ggthemes::scale_fill_tableau() +
  geom_hline(yintercept=100, linetype = 2, col = "purple")

ggsave(plot1, file = "plot1.png")

ggplot(df, aes(x = raceG, col = sex, fill = sex)) +
  geom_histogram(stat = "count") +
  facet_wrap(years_old~.) +
  coord_flip() +
  ggthemes::theme_few() +
  ggthemes::scale_colour_tableau() +
  ggthemes::scale_fill_tableau() +
  geom_hline(yintercept=100, linetype = 2, col = "purple")



