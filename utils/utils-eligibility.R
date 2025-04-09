#utils.R

init_elig_df<-function(){
  df = tibble(
    category = c(rep("Compensation",2), rep("Eligibility",4), rep("Authenticity",3)),
    action = c(rep("Exclusion",2),rep("Inclusion",3), rep("Exclusion",4)) 
  ) %>% 
    dplyr::mutate(cid = 1:n(), pid = NA, record_id = NA) %>% dplyr::relocate(pid,record_id,cid) %>% 
    dplyr::mutate(
      description = c(
        "Failed to acknowledge compensation terms and conditions",
        "Failed to provide informed consent",
        "Respondent is 19 years of older and a primary caregiver",
        "Child is 2191 days or younger",
        "Currently lives in the state of Nebraska",
        "Zipcode not match for county or surrounding county",
        "Child's birthday failed to be confirmed",
        "Kidsight z-score within 5SD",
        "Did not complete all modules of survey"
      )
    ) %>% 
    dplyr::mutate(pass = NA, notes = "")
  return(df)
}


passes_cid1<-function(dat){
  #"Failed to acknowledge compensation terms and conditions"
  cid1 = dat %>% 
    dplyr::select(
      pid,record_id,
     state_law_requires_that_kidsights_data_collect_my_name___1,
     financial_compensation_be_sent_to_a_nebraska_residential_address___1,
     state_law_prohibits_sending_compensation_electronically___1, 
     kidsights_data_reviews_all_responses_for_quality___1
     ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pass_cid1 = sum(c_across(state_law_requires_that_kidsights_data_collect_my_name___1:kidsights_data_reviews_all_responses_for_quality___1))==4) %>% 
    dplyr::relocate(pid,record_id,pass_cid1)
  
  return(cid1)
}


passes_cid2<-function(dat){
  #"Failed to provide informed consent"
  cid2 = dat %>% 
    dplyr::select(pid,record_id, eq001) %>% 
    dplyr::mutate(pass_cid2 = eq001==1) %>% 
    dplyr::relocate(pid,record_id, pass_cid2)
  
  return(cid2)
}

passes_cid3 = function(dat){
  #"Respondent is 19 years of older and a primary caregiver"
  cid3 = dat %>% 
    dplyr::select(pid,record_id,eq003,eq002) %>% 
    dplyr::mutate(pass_cid3 = (eq002 == 1 & eq003==1)) %>% 
    dplyr::relocate(pid,record_id,pass_cid3)
  
  return(cid3)
}

passes_cid4 = function(dat){
  #"Child is 2191 days or younger"
  cid4 = dat %>% 
    dplyr::select(pid,record_id,age_in_days) %>% 
    dplyr::mutate(pass_cid4 = (age_in_days<=2191)) %>% 
    dplyr::relocate(pid,record_id,pass_cid4)
  
  return(cid4)
}

passes_cid5 = function(dat){
  #"Currently lives in the state of Nebraska"
  cid5 = dat %>% 
    dplyr::select(pid,record_id,eqstate) %>% 
    dplyr::mutate(pass_cid5 = (eqstate==1)) %>% 
    dplyr::relocate(pid,record_id,pass_cid5)
  
  return(cid5)
  
}

passes_cid6 = function(dat, dict){
  #"Zipcode not match for county or surrounding county"
  
  cid6 = dat %>% 
    dplyr::select(pid,record_id, sq001, fq001)
  
  mapdat = value_labels(lex = "fq001", dict=dict)
  
  cid6 = cid6 %>% 
    dplyr::mutate(county = plyr::mapvalues(x = as.character(fq001), from = mapdat$value, to = mapdat$label, warn_missing = F)) 
  
  acceptable_zipcodes = get_KH_acceptable_zipcodes() %>% 
    dplyr::mutate(sq001 = ZipCode) %>%
    tibble::as_tibble() %>% 
    dplyr::group_by(sq001) %>% 
    dplyr::reframe(acceptable_counties = paste0(County, collapse = "; "))
  
  cid6 = cid6 %>% 
    dplyr::left_join(acceptable_zipcodes, by = "sq001")
  
  cid6 = cid6 %>% 
    dplyr::mutate(pass_cid6 = stringr::str_detect(acceptable_counties, county))

  cid6 = cid6 %>% dplyr::relocate(pid,record_id,pass_cid6)
  
  return(cid6)
}

passes_cid7 = function(dat){
  #"Child's birthday failed to be confirmed"
}

passes_cid8 = function(dat){
  #"Kidsight z-score within 5SD"
}

passes_cid9 = function(dat){
  #"Did not complete all modules of survey"
}


get_KH_acceptable_zipcodes<-function(dir = getwd(), verbose = F){
  
  require(readxl)
  require(zipcodeR)
  
  # Load in the zip code data 
  zipcodes_df = readxl::read_excel(path = file.path(dir,"data/Zip_County_Code_UPDATED10.18.xlsx"), sheet = "Master") %>% 
    dplyr::mutate(County = stringr::str_remove_all(County," County"))
  # Check to see that all Nebraska zipcodes are in KH and/or zipcodeR data set
  zipcodeR::search_state("NE") %>% 
    dplyr::filter(zipcode %in%  setdiff(zipcodeR::search_state("NE") %>% purrr::pluck("zipcode"), unique(zipcodes_df$ZipCode))) %>% 
    dplyr::select(zipcode:major_city, county, radius_in_miles, population) %>% 
    arrange(county) %>% 
    knitr::kable(caption = "Zipcodes in zipcodeR package but not in KH Database") #Some missing zip codes from KH list!
  zipcodes_df %>% 
    dplyr::filter(ZipCode %in% setdiff(unique(zipcodes_df$ZipCode), zipcodeR::search_state("NE") %>% purrr::pluck("zipcode"))) %>% 
    dplyr::arrange(ZipCode) %>% 
    dplyr::group_by(ZipCode) %>% 
    dplyr::summarize(Counties = paste(County, collapse = " + ")) %>% 
    dplyr::arrange(ZipCode) %>% 
    knitr::kable(caption = "Zipcodes in KH database but not in zipcodeR database") #Are there missing zipcodes in zipcodeR database?
  # Check to see if the counties listed by a KH zipcode contains the county from the zipcodeR database
  foo = zipcodes_df %>% 
    dplyr::mutate(ZipCode = as.character(ZipCode)) %>% 
    dplyr::filter(ZipCode %in% (zipcodeR::search_state("NE") %>% purrr::pluck("zipcode"))) %>% 
    dplyr::left_join(zipcodeR::search_state("NE") %>% 
                       dplyr::mutate(zipcodeR_cnty = str_remove_all(county," County"), 
                                     ZipCode = zipcode) %>% 
                       dplyr::select(ZipCode, zipcodeR_cnty), 
                     by = "ZipCode") %>% 
    dplyr::group_by(ZipCode) %>% 
    dplyr::summarize(contains_zipcodeR_cnty = (zipcodeR_cnty[1] %in% County), 
                     zipcodeR_cnty = zipcodeR_cnty[1], 
                     KH_counties = paste(County, collapse = " + ")) %>% 
    dplyr::filter(!contains_zipcodeR_cnty)
  if(verbose){message(paste("There are",nrow(foo),"zipcodes from KH database with a non matching zipcodeR county"))} 
  
  
  return(zipcodes_df)
}

