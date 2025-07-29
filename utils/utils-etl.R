

download_vet_responses<-function(my_API, codebook){
  
  library(REDCapR)
  library(httr)
  library(tidyverse)
  library(purrr)
  library(haven)
  
  dat<-lapply(my_API$api_code, function(the_code){
    ret <- 
      REDCapR::redcap_read(
          redcap_uri = "https://unmcredcap.unmc.edu/redcap/api/", 
          token =  the_code) %>% 
      purrr::pluck("data") %>% 
      dplyr::mutate(retrieved_date = Sys.time(),
                    pid = with(my_API, pid[api_code == the_code])) %>% 
      dplyr::relocate(retrieved_date, pid) %>% 
      dplyr::mutate(sq001 = as.character(sq001))
  }) %>% flexible_bind_rows()
  

  
  
  #!/usr/bin/env Rscript
  url <- "https://unmcredcap.unmc.edu/redcap/api/"
  formData <- list("token"=my_API$api_code[1],
                   content='metadata',
                   format='json',
                   returnFormat='json'
  )
  response <- httr::POST(url, body = formData, encode = "form")
  dict <- httr::content(response)
  # results in a dictionary in list format
  
  
  for(i in 1:length(dict)){
    names(dict)[i] = dict[[i]]$field_name
  }
  
  items_ne25 = codebook$lex_ne25 %>% tolower() %>% na.omit()
  
  # Make any don't know responses missing
  dat = dat %>% 
    dplyr::mutate(
      across(
        dplyr::any_of(items_ne25), 
        function(y){ynew = y; ynew[abs(y)==9]=NA; return(ynew)}
      )
    )
  
  # Reverse the reverse coded items
  dat %>% dplyr::mutate(
    nom054x = abs(nom054x-4), 
    nom052y = abs(nom052y-4), 
    nom056x = abs(nom056x-4) 
  )
  
  
  elig_list = check_eligibility_authenticity(dat=dat,dict=dict, codebook = codebook)
  
  
  return(list(data = dat, dictionary = dict, vetting = elig_list))
  
}


value_labels<-function(lex, dict,varname = "lex_ne25"){
  
  # Note issue issue in education labels due to commas in description
  tmp = dict[[lex]]$select_choices_or_calculations %>% stringr::str_split_1(" \\| ")
  outdf = data.frame(value = rep(NA,length(tmp)), label = NA)
  for(i in 1:length(tmp)){
    tmp_i = tmp[i] %>% stringr::str_split_1(", ")
    outdf$value[i] = tmp_i[1]
    outdf$label[i] = paste0(tmp_i[-1], collapse = ", ")
  }
  outdf = outdf %>% 
    dplyr::mutate(var = lex) %>% 
    dplyr::relocate(var)
  
  names(outdf)[1] = varname
  
  return(outdf)
}

recode__<-function(dat, dict, my_API, what = NULL, relevel_it = T, add_labels = TRUE){
  
  recodes_df = NULL
  
  if(what %in% c("childcare")){
    recodes_df = clean_childcare_variables(dat)
  }
  
  if(what %in% c("mental health")){
    recodes_df = clean_mental_health_ace_data(dat)
  }
  
  if(what %in% c("include")){
    recodes_df = dat %>% 
      dplyr::select(pid,record_id,eligibility,authenticity) %>% 
      dplyr::mutate(
        eligible = (eligibility=="Pass"),
        authentic = (authenticity=="Pass"),
        include =  (eligible & authentic)
      ) %>% 
      dplyr::select(-eligibility,-authenticity)
    
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(recodes_df$eligible) <- "Meets study inclusion criteria"
      labelled::var_label(recodes_df$authentic) <- "Passes authenticity screening"
      labelled::var_label(recodes_df$include) <- "Meets inclusion criteria (inclusion + authenticity)"
    }
  }
  
  if(what %in% c("race", "ethnicity")){
    
    #---------------------------------------------------------------------------
    # Child Race
    # --------------------------------------------------------------------------
    raceth_df = dat %>% 
      dplyr::select(pid, record_id, dplyr::starts_with("cqr011"), dplyr::starts_with("cqr010_")) %>% 
      tidyr::pivot_longer(dplyr::starts_with("cqr010"), names_to = "var", values_to = "response") %>% 
      dplyr::left_join(
        value_labels("cqr010", dict = dict) %>% 
          dplyr::mutate(var = paste(lex_ne25,value,sep = "___")) %>% 
          dplyr::select(var,label), 
        by = "var"
      ) %>% 
      dplyr::mutate(
        label = ifelse(label %in% c("Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese","Native Hawaiian", "Guamanian or Chamorro", "Samoan", "Other Pacific Islander"), "Asian or Pacific Islander", label), 
        label = ifelse(label %in% c("Middle Eastern", "Some other race"), "Some Other Race", label), 
      ) %>% 
      dplyr::filter(response==1) %>% 
      dplyr::group_by(pid,record_id, label) %>% 
      dplyr::reframe(hisp = ifelse(cqr011[1]==1, "Hispanic", "non-Hisp.")) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(pid, record_id) %>% 
      dplyr::reframe(hisp = hisp[1], race = ifelse(n()>1, "Two or More", label[1])) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(raceG = ifelse(hisp == "Hispanic", "Hispanic", paste0(race,", non-Hisp."))) %>% 
      dplyr::mutate(raceG = ifelse(raceG=="Other Asian, non-Hisp.", "Asian or Pacific Islander, non-Hisp.", raceG)) %>% 
      dplyr::mutate(across(where(is.character), as.factor)) %>% 
      dplyr::select(pid:record_id, hisp, race, raceG)
    
    if(relevel_it){
      #Set baseline categories
      raceth_df$hisp = relevel(raceth_df$hisp, ref = "non-Hisp.")
      raceth_df$race = relevel(raceth_df$race, ref = "White")
      raceth_df$raceG = relevel(raceth_df$raceG, ref = "White, non-Hisp.")
    }
    
    
    #---------------------------------------------------------------------------
    # Caregiver's Race
    #---------------------------------------------------------------------------
    a1_raceth_df = dat %>% 
      dplyr::select(pid, record_id, dplyr::starts_with("sq003"), dplyr::starts_with("sq002_")) %>% 
      tidyr::pivot_longer(dplyr::starts_with("sq002_"), names_to = "var", values_to = "response") %>% 
      dplyr::left_join(
        value_labels("sq002", dict = dict) %>% 
          dplyr::mutate(var = paste(lex_ne25,value,sep = "___")) %>% 
          dplyr::select(var,label), 
        by = "var"
      ) %>% 
      dplyr::mutate(
        label = ifelse(label %in% c("Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese","Native Hawaiian", "Guamanian or Chamorro", "Samoan", "Other Pacific Islander"), "Asian or Pacific Islander", label), 
        label = ifelse(label %in% c("Middle Eastern", "Some other race"), "Some Other Race", label), 
      ) %>% 
      dplyr::filter(response==1) %>% 
      dplyr::group_by(pid,record_id, label) %>% 
      dplyr::reframe(a1_hisp = ifelse(sq003[1]==1, "Hispanic", "non-Hisp.")) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(pid, record_id) %>% 
      dplyr::reframe(a1_hisp = a1_hisp[1], a1_race = ifelse(n()>1, "Two or More", label[1])) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(a1_raceG = ifelse(a1_hisp == "Hispanic", "Hispanic", paste0(a1_race,", non-Hisp."))) %>% 
      dplyr::mutate(a1_raceG = ifelse(a1_raceG=="Other Asian, non-Hisp.", "Asian or Pacific Islander, non-Hisp.", a1_raceG)) %>% 
      dplyr::mutate(across(where(is.character), as.factor)) %>% 
      dplyr::select(pid:record_id, a1_hisp, a1_race, a1_raceG)
    
    if(relevel_it){
      #Set baseline categories
      a1_raceth_df$a1_hisp = relevel(a1_raceth_df$a1_hisp, ref = "non-Hisp.")
      a1_raceth_df$a1_race = relevel(a1_raceth_df$a1_race, ref = "White")
      a1_raceth_df$a1_raceG = relevel(a1_raceth_df$a1_raceG, ref = "White, non-Hisp.")
    }
    
    
    recodes_df = raceth_df %>% dplyr::left_join(a1_raceth_df, by = c("pid","record_id"))
    
    # Add labels after creating variables
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(recodes_df$hisp) <- "Child Hispanic/Latino ethnicity"
      labelled::var_label(recodes_df$race) <- "Child race (collapsed categories)"
      labelled::var_label(recodes_df$raceG) <- "Child race/ethnicity combined"
      labelled::var_label(recodes_df$a1_hisp) <- "Primary caregiver Hispanic/Latino ethnicity"
      labelled::var_label(recodes_df$a1_race) <- "Primary caregiver race (collapsed categories)"
      labelled::var_label(recodes_df$a1_raceG) <- "Primary caregiver race/ethnicity combined"
    }
  }
  
  if(what %in% c("caregiver relationship")){
    # responding caregiver
    relate_df = dat %>% 
      dplyr::mutate(
        relation1 = plyr::mapvalues(cqr008, from =  value_labels(lex = "cqr008",dict = dict)$value, to=  value_labels(lex = "cqr008",dict = dict)$label, warn_missing = F), 
        relation2 = plyr::mapvalues(nschj013, from = value_labels(lex = "nschj013",dict = dict)$value, to=  value_labels(lex = "nschj013",dict = dict)$label, warn_missing = F), 
        female_a1 = as.logical(cqr002==0), 
        mom_a1 = as.logical(relation1==value_labels(lex = "cqr008",dict = dict)$label[1] & female_a1)
      ) %>% 
      dplyr::select(pid,record_id,relation1:mom_a1) %>% 
      dplyr::mutate(across(where(is.character), as.factor)) 
    
    
    if(relevel_it){relate_df$relation2 = relevel(relate_df$relation2, value_labels(lex = "nschj013",dict = dict)$label[1])}
    
    # Add labels after creating variables
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(relate_df$relation1) <- "Primary caregiver relationship to child"
      labelled::var_label(relate_df$relation2) <- "Secondary caregiver relationship to child"
      labelled::var_label(relate_df$female_a1) <- "Primary caregiver is female"
      labelled::var_label(relate_df$mom_a1) <- "Primary caregiver is mother"
    }
    
    recodes_df = relate_df
    
  }
  
  if(what %in% c("education")){
    
    simple_educ_label = data.frame(
      educ=  value_labels(lex = "cqr004",dict = dict)$label) %>% 
      dplyr::mutate(
        educ4 = c(rep("Less than High School Graduate", 2), 
                  rep("High School Graduate (including Equivalency)",1), 
                  rep("Some College or Associate's Degree", 3), 
                  rep("College Degree",3)
        ), 
        educ6 = c(rep("Less than High School Graduate", 2), 
                  rep("High School Graduate (including Equivalency)",1), 
                  rep("Some College or Associate's Degree", 3), 
                  rep("Bachelor's Degree",1), 
                  rep("Master's Degree",1), 
                  rep("Doctorate or Professional Degree",1)
        )
      )
    
    simple_educ_value = data.frame(
      label = value_labels(lex = "cqr004",dict = dict)$label,
      educ=  value_labels(lex = "cqr004",dict = dict)$value) %>% 
      dplyr::mutate(educ4 = c(rep(0, 2), 
                              rep(1,1), 
                              rep(2, 3), 
                              rep(3,3)
      ), 
      educ6 = c(rep(0, 2), 
                rep(1,1), 
                rep(2, 3), 
                rep(3,1), 
                rep(4,1), 
                rep(5,1)
      )
      )
    
    # FIXED: Call recode__ directly instead of recode_it to avoid recursion
    relate_vars <- recode__(dat = dat, dict = dict, what = "caregiver relationship", 
                            relevel_it = relevel_it, add_labels = FALSE)
    
    educ_df = dat %>% 
      dplyr::select(-dplyr::any_of(c("relation1","relation2","mom_a1"))) %>% 
      dplyr::left_join(relate_vars, by = c("pid", "record_id")) %>%
      dplyr::mutate(
        ## Maximum education of caregivers (8 categories)
        educ_max = 
          dplyr::case_when(
            nschj017 > cqr004 ~ nschj017,
            is.na(cqr004) & !is.na(nschj017) ~ nschj017,
            .default = cqr004
          ) %>%  
          factor(
            levels = value_labels(lex = "cqr004",dict = dict)$value, 
            labels = value_labels(lex = "cqr004",dict = dict)$label
          ),
        
        # Caregiver 1 and 2 education (8 categories)
        educ_a1 =  factor(cqr004, levels = value_labels(lex = "cqr004",dict = dict)$value, labels = value_labels(lex = "cqr004",dict = dict)$label), 
        educ_a2 =  factor(nschj017, levels = value_labels(lex = "nschj017",dict = dict)$value, labels = value_labels(lex = "nschj017",dict = dict)$label), 
        
        # Maternal education (8 categories)
        educ_mom = ifelse(mom_a1, educ_a1, NA) %>% factor(levels = value_labels(lex = "cqr004",dict = dict)$value, labels = value_labels(lex = "cqr004",dict = dict)$label), 
        
        # Convert to four categories
        educ4_max = plyr::mapvalues(as.character(educ_max), from = simple_educ_label$educ, to = simple_educ_label$educ4) %>% 
          plyr::mapvalues(from = simple_educ_label$educ4, to = simple_educ_value$educ4) %>% 
          factor(levels = simple_educ_value$educ4, labels = simple_educ_label$educ4), 
        educ4_a1 = plyr::mapvalues(as.character(educ_a1), from = simple_educ_label$educ, to = simple_educ_label$educ4) %>% 
          plyr::mapvalues(from = simple_educ_label$educ4, to = simple_educ_value$educ4) %>% 
          factor(levels = simple_educ_value$educ4, labels = simple_educ_label$educ4), 
        educ4_a2 = plyr::mapvalues(as.character(educ_a2), from = simple_educ_label$educ, to = simple_educ_label$educ4) %>% 
          plyr::mapvalues(from = simple_educ_label$educ4, to = simple_educ_value$educ4) %>% 
          factor(levels = simple_educ_value$educ4, labels = simple_educ_label$educ4), 
        educ4_mom = plyr::mapvalues(as.character(educ_mom), from = simple_educ_label$educ, to = simple_educ_label$educ4) %>% 
          plyr::mapvalues(from = simple_educ_label$educ4, to = simple_educ_value$educ4) %>% 
          factor(levels = simple_educ_value$educ4, labels = simple_educ_label$educ4), 
        
        # Convert to 6 categories
        educ6_max = plyr::mapvalues(as.character(educ_max), from = simple_educ_label$educ, to = simple_educ_label$educ6) %>% 
          plyr::mapvalues(from = simple_educ_label$educ6, to = simple_educ_value$educ6) %>% 
          factor(levels = simple_educ_value$educ6, labels = simple_educ_label$educ6), 
        educ6_a1 = plyr::mapvalues(as.character(educ_a1), from = simple_educ_label$educ, to = simple_educ_label$educ6) %>%
          plyr::mapvalues(from = simple_educ_label$educ6, to = simple_educ_value$educ6) %>% 
          factor(levels = simple_educ_value$educ6, labels = simple_educ_label$educ6), 
        educ6_a2 = plyr::mapvalues(as.character(educ_a2), from = simple_educ_label$educ, to = simple_educ_label$educ6) %>%
          plyr::mapvalues(from = simple_educ_label$educ6, to = simple_educ_value$educ6) %>% 
          factor(levels = simple_educ_value$educ6, labels = simple_educ_label$educ6), 
        educ6_mom = plyr::mapvalues(as.character(educ_mom), from = simple_educ_label$educ, to = simple_educ_label$educ6) %>%
          plyr::mapvalues(from = simple_educ_label$educ6, to = simple_educ_value$educ6) %>% 
          factor(levels = simple_educ_value$educ6, labels = simple_educ_label$educ6) 
        
      ) %>% 
      dplyr::select(pid, record_id, educ_max:educ6_mom) %>% 
      dplyr::mutate(across(where(is.character), as.factor))
    
    
    if(relevel_it){
      # relevel
      educ_df$educ_max = relevel( as.factor(educ_df$educ_max), ref = simple_educ_label$educ[7]) #BA/BS as reference
      educ_df$educ_a1 = relevel( as.factor(educ_df$educ_a1), ref = simple_educ_label$educ[7]) #BA/BS as reference
      educ_df$educ_a2 = relevel( as.factor(educ_df$educ_a2), ref = simple_educ_label$educ[7]) #BA/BS as reference
      
      educ_df$educ4_max = relevel( as.factor(educ_df$educ4_max), ref = simple_educ_label$educ4[7]) #College degree reference
      educ_df$educ4_a1 = relevel( as.factor(educ_df$educ4_a1), ref = simple_educ_label$educ4[7]) #College degree reference
      educ_df$educ4_a2 = relevel( as.factor(educ_df$educ4_a2), ref = simple_educ_label$educ4[7]) #College degree reference
      
      educ_df$educ6_max = relevel( as.factor(educ_df$educ6_max), ref = simple_educ_label$educ6[7]) #College degree reference
      educ_df$educ6_a1 = relevel( as.factor(educ_df$educ6_a1), ref = simple_educ_label$educ6[7]) #College degree reference
      educ_df$educ6_a2 = relevel( as.factor(educ_df$educ6_a2), ref = simple_educ_label$educ6[7]) #College degree reference
    }
    
    # Add labels after creating variables
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(educ_df$educ_max) <- "Maximum education level among caregivers (8 categories)"
      labelled::var_label(educ_df$educ_a1) <- "Primary caregiver education level (8 categories)"
      labelled::var_label(educ_df$educ_a2) <- "Secondary caregiver education level (8 categories)"
      labelled::var_label(educ_df$educ_mom) <- "Maternal education level (8 categories)"
      labelled::var_label(educ_df$educ4_max) <- "Maximum education level among caregivers (4 categories)"
      labelled::var_label(educ_df$educ4_a1) <- "Primary caregiver education level (4 categories)"
      labelled::var_label(educ_df$educ4_a2) <- "Secondary caregiver education level (4 categories)"
      labelled::var_label(educ_df$educ4_mom) <- "Maternal education level (4 categories)"
      labelled::var_label(educ_df$educ6_max) <- "Maximum education level among caregivers (6 categories)"
      labelled::var_label(educ_df$educ6_a1) <- "Primary caregiver education level (6 categories)"
      labelled::var_label(educ_df$educ6_a2) <- "Secondary caregiver education level (6 categories)"
      labelled::var_label(educ_df$educ6_mom) <- "Maternal education level (6 categories)"
    }
    
    recodes_df = educ_df
    
  }
  
  if(what == "sex"){
    
    sex_df = dat %>% dplyr::select(pid, record_id, cqr009) %>% 
      dplyr::mutate(sex = plyr::mapvalues(cqr009, from =  value_labels(lex = "cqr009",dict = dict)$value, to=  value_labels(lex = "cqr009",dict = dict)$label, warn_missing = F), 
                    female = (sex == "Female")) %>% 
      dplyr::mutate(across(where(is.character), as.factor))
    
    if(relevel_it){sex_df$sex = relevel(sex_df$sex, ref = "Female")}  
    
    # Add labels after creating variables
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(sex_df$sex) <- "Child's sex"
      labelled::var_label(sex_df$female) <- "Child is female"
    }
    
    recodes_df = sex_df %>% dplyr::select(-cqr009)
  }
  
  if(what == "age"){
    
    age_df = dat %>% dplyr::select(pid, record_id, age_in_days, cqr003) %>% 
      dplyr::mutate(
        days_old = age_in_days,
        years_old = age_in_days/365.25, 
        months_old = years_old*12, 
        a1_years_old = cqr003
        ) 
    
    # Add labels after creating variables
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(age_df$days_old) <- "Child's age (days)"
      labelled::var_label(age_df$years_old) <- "Child's age (years)"
      labelled::var_label(age_df$months_old) <- "Child's age (months)"
      labelled::var_label(age_df$a1_years_old) <- "Primary caregiver age (years)"
    }
    
    recodes_df = age_df %>% dplyr::select(-cqr003,-age_in_days)
  }
  
  if(what == "income"){
    income_df = dat %>% 
      dplyr::select(consent_date, pid, record_id, cqr006,fqlive1_1, fqlive1_2) %>% 
      dplyr::rename(
        income = cqr006
      ) %>% 
      dplyr::mutate(
        cpi99 = cpi_ratio_1999(consent_date), 
        inc99 = income*cpi99, 
        family_size = dplyr::case_when(
          fqlive1_1<999 & fqlive1_2<999 ~ fqlive1_1 + fqlive1_2, 
          fqlive1_1<999 & fqlive1_2==999 ~ fqlive1_2 + 1, 
          .default = NA
        ),
        federal_poverty_threshold = get_poverty_threshold(dates = consent_date, family_size = family_size), 
        fpl = round(100*income/federal_poverty_threshold,0), 
        fplcat = cut(fpl, c(-Inf,100,200,300,400,Inf), labels = c("<100% FPL", "100-199% FPL", "200-299% FPL", "300-399% FPL", "400+% FPL"))
      ) 
    
    if(relevel_it){income_df$fplcat = relevel(income_df$fplcat, ref = "400+% FPL")}
    
    # Add labels after creating variables
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(income_df$income) <- "Household annual income (nominal dollars)"
      labelled::var_label(income_df$cpi99) <- "CPI adjustment ratio to 1999 dollars"
      labelled::var_label(income_df$inc99) <- "Household annual income (1999 dollars)"
      labelled::var_label(income_df$family_size) <- "Family size (number of people in household)"
      labelled::var_label(income_df$federal_poverty_threshold) <- "Federal poverty threshold for family size"
      labelled::var_label(income_df$fpl) <- "Household income as percentage of federal poverty level"
      labelled::var_label(income_df$fplcat) <- "Household income as percentage of federal poverty level (categories)"
    }
    
    recodes_df = income_df %>% dplyr::select(pid, record_id, income,cpi99:fplcat)
    
    
  }
  
  if(what == "survey completion"){
    recodes_df= dat %>% 
      dplyr::select(pid,record_id, dplyr::ends_with("_complete")) %>%
      tidyr::pivot_longer(dplyr::ends_with("_complete")) %>% 
      dplyr::filter(value==2) %>% 
      na.omit() %>% 
      dplyr::mutate(name = dplyr::case_when(
        name == "consent_doc_complete" ~  "module_0_consent_form_complete", 
        name == "eligibility_form_complete" ~   "module_1_consent_form_complete", 
        startsWith(name, "module_6") ~ "module_6_KMT_complete", 
        .default = name
      )) %>% 
      dplyr::filter(startsWith(name,"module")) %>% 
      dplyr::arrange(pid,record_id,name) %>% 
      dplyr::group_by(pid,record_id,name) %>% 
      dplyr::summarise(value = max(value, na.rm = T)) %>% 
      dplyr::ungroup(name) %>% 
      dplyr::summarise(last_module = n()-1) %>% 
      dplyr::ungroup()
    
    recodes_df = recodes_df %>% 
      dplyr::left_join(
        dat %>% dplyr::select(pid,record_id, date = consent_date), by = c("pid","record_id"), 
      ) %>% 
      dplyr::left_join(my_API %>% dplyr::select(pid,project) %>% dplyr::mutate(project = factor(project)), by = "pid")
    
    # Add labels after creating variables
    if(add_labels && requireNamespace("labelled", quietly = TRUE)) {
      labelled::var_label(recodes_df$last_module) <- "Last completed survey module (indicator of attrition)"
      labelled::var_label(recodes_df$date) <- "Date survey started"
      labelled::var_label(recodes_df$project) <- "Redcap project identifier description"
      
    }
  }
  
  
  return(recodes_df)
  
}

create_variable_metadata <- function(dat, dict, my_API, what = "all") {
  
  library(labelled)
  library(jsonlite)
  library(dplyr)
  
  # Helper function to get detailed variable info
  get_variable_info <- function(var, var_name) {
    info <- list(
      variable_name = var_name,
      label = attr(var, "label") %||% "",
      data_type = class(var)[1],
      storage_mode = mode(var),
      length = length(var),
      n_missing = sum(is.na(var)),
      missing_percentage = round(sum(is.na(var))/length(var) * 100, 2)
    )
    
    # Add type-specific information
    if (is.factor(var)) {
      info$factor_info <- list(
        levels = levels(var),
        n_levels = nlevels(var),
        level_counts = as.list(table(var, useNA = "ifany")),
        is_ordered = is.ordered(var)
      )
    } else if (is.logical(var)) {
      info$logical_info <- list(
        n_true = sum(var, na.rm = TRUE),
        n_false = sum(!var, na.rm = TRUE),
        proportion_true = round(mean(var, na.rm = TRUE), 3)
      )
    } else if (is.numeric(var)) {
      info$numeric_info <- list(
        min = min(var, na.rm = TRUE),
        max = max(var, na.rm = TRUE),
        mean = round(mean(var, na.rm = TRUE), 3),
        median = median(var, na.rm = TRUE),
        sd = round(sd(var, na.rm = TRUE), 3),
        q25 = quantile(var, 0.25, na.rm = TRUE),
        q75 = quantile(var, 0.75, na.rm = TRUE)
      )
    } else if (is.character(var)) {
      info$character_info <- list(
        n_unique = length(unique(var)),
        unique_values = if(length(unique(var)) <= 20) unique(var) else paste("Too many unique values:", length(unique(var))),
        most_common = names(sort(table(var), decreasing = TRUE))[1]
      )
    }
    
    return(info)
  }
  
  # Initialize metadata list
  metadata <- list(
    creation_date = Sys.time(),
    data_dimensions = dim(dat),
    variable_categories = list()
  )
  
  # Define variable categories based on your recode__ function
  if(what == "all") {
    categories <- init__("all")
  } else {
    categories <- what
  }
  
  for(category in categories) {
    
    # Get the recoded data for this category
    category_data <- recode__(dat = dat, dict = dict, my_API = my_API, what = category, relevel_it = TRUE)
    
    category_vars <- names(category_data)[!names(category_data) %in% c("pid", "record_id")]
    
    category_metadata <- list(
      category_name = category,
      description = get_category_description(category),
      n_variables = length(category_vars),
      variables = list()
    )
    
    # Get metadata for each variable in this category
    for(var_name in category_vars) {
      var_data <- category_data[[var_name]]
      category_metadata$variables[[var_name]] <- get_variable_info(var_data, var_name)
    }
    
    metadata$variable_categories[[category]] <- category_metadata
  }
  
  return(metadata)
}

# Helper function to provide category descriptions
get_category_description <- function(category) {
  
  descriptions<-init__("category descriptions")
  
  return(descriptions[[category]] %||% "No description available")
}

# Enhanced function to also add variable labels to the original recode__ function
recode_with_metadata <- function(dat, dict, my_API = my_API, what = NULL, relevel_it = TRUE, add_labels = TRUE) {
  
  library(labelled)
  
  # Get the recoded data
  recoded_df <- recode__(dat = dat, dict = dict, my_API = my_API, what = what, relevel_it = relevel_it)
  # 
  # if(add_labels && !is.null(recoded_df)) {
  #   
  #   # Add variable labels based on category
  #   if(what %in% c("include")){
  #     if("eligible" %in% names(recoded_df)) var_label(recoded_df$include) <- "Meets study eligiblity criteria"
  #     if("authentic" %in% names(recoded_df)) var_label(recoded_df$include) <- "Passes autenticity screening protocol"
  #     if("include" %in% names(recoded_df)) var_label(recoded_df$include) <- "Meets inclusion criteria (eligible + authentic)"
  #   }
  #   
  #   if(what %in% c("race", "ethnicity")) {
  #     if("hisp" %in% names(recoded_df)) var_label(recoded_df$hisp) <- "Child Hispanic/Latino ethnicity"
  #     if("race" %in% names(recoded_df)) var_label(recoded_df$race) <- "Child race (collapsed categories)"
  #     if("raceG" %in% names(recoded_df)) var_label(recoded_df$raceG) <- "Child race/ethnicity combined"
  #     if("a1_hisp" %in% names(recoded_df)) var_label(recoded_df$a1_hisp) <- "Primary caregiver Hispanic/Latino ethnicity"
  #     if("a1_race" %in% names(recoded_df)) var_label(recoded_df$a1_race) <- "Primary caregiver race (collapsed categories)"
  #     if("a1_raceG" %in% names(recoded_df)) var_label(recoded_df$a1_raceG) <- "Primary caregiver race/ethnicity combined"
  #   }
  #   
  #   if(what %in% c("caregiver relationship")) {
  #     if("relation1" %in% names(recoded_df)) var_label(recoded_df$relation1) <- "Primary caregiver relationship to child"
  #     if("relation2" %in% names(recoded_df)) var_label(recoded_df$relation2) <- "Secondary caregiver relationship to child"
  #     if("female_a1" %in% names(recoded_df)) var_label(recoded_df$female_a1) <- "Primary caregiver is female"
  #     if("mom_a1" %in% names(recoded_df)) var_label(recoded_df$mom_a1) <- "Primary caregiver is mother"
  #   }
  #   
  #   if(what %in% c("education")) {
  #     education_labels <- list(
  #       "educ_max" = "Maximum education level among caregivers (8 categories)",
  #       "educ_a1" = "Primary caregiver education level (8 categories)",
  #       "educ_a2" = "Secondary caregiver education level (8 categories)",
  #       "educ_mom" = "Maternal education level (8 categories)",
  #       "educ4_max" = "Maximum education level among caregivers (4 categories)",
  #       "educ4_a1" = "Primary caregiver education level (4 categories)",
  #       "educ4_a2" = "Secondary caregiver education level (4 categories)",
  #       "educ4_mom" = "Maternal education level (4 categories)",
  #       "educ6_max" = "Maximum education level among caregivers (6 categories)",
  #       "educ6_a1" = "Primary caregiver education level (6 categories)",
  #       "educ6_a2" = "Secondary caregiver education level (6 categories)",
  #       "educ6_mom" = "Maternal education level (6 categories)"
  #     )
  #     
  #     for(var_name in names(education_labels)) {
  #       if(var_name %in% names(recoded_df)) {
  #         var_label(recoded_df[[var_name]]) <- education_labels[[var_name]]
  #       }
  #     }
  #   }
  #   
  #   if(what == "sex") {
  #     if("sex" %in% names(recoded_df)) var_label(recoded_df$sex) <- "Child's sex"
  #     if("female" %in% names(recoded_df)) var_label(recoded_df$female) <- "Child is female"
  #   }
  #   
  #   if(what == "age") {
  #     if("days_old" %in% names(recoded_df)) labelled::var_label(recoded_df$days_old) <- "Child's age (days)"
  #     if("years_old" %in% names(recoded_df)) labelled::var_label(recoded_df$years_old) <- "Child's age (years)"
  #     if("months_old" %in% names(recoded_df)) labelled::var_label(recoded_df$months_old) <- "Child's age (months)"
  #     if("a1_years_old" %in% names(recoded_df)) labelled::var_label(recoded_df$a1_years_old) <- "Primary caregiver age (years)"
  #   }
  #   
  #   if(what == "income") {
  #     income_labels <- list(
  #       "income" = "Household annual income (nominal dollars)",
  #       "cpi99" = "CPI adjustment ratio to 1999 dollars",
  #       "inc99" = "Household annual income (1999 dollars)",
  #       "family_size" = "Family size (number of people in household)",
  #       "federal_poverty_threshold" = "Federal poverty threshold for family size",
  #       "fpl" = "Household income as percentage of federal poverty level",
  #       "fplcat" = "Household income as percentage of federal poverty level (categories)"
  #     )
  #     
  #     for(var_name in names(income_labels)) {
  #       if(var_name %in% names(recoded_df)) {
  #         var_label(recoded_df[[var_name]]) <- income_labels[[var_name]]
  #       }
  #     }
  #   }
  #   
  #   if(what == "survey completion") {
  #     if("last_module" %in% names(recoded_df)) var_label(recoded_df$last_module) <- "Last completed survey module (indicator of attrition)"
  #     if("date" %in% names(recoded_df)) var_label(recoded_df$last_module) <- "Date survey started"
  #     if("project" %in% names(recoded_df)) var_label(recoded_df$last_module) <- "Redcap project identifier description"
  #   }
  #   
  # }
  
  return(recoded_df)
}

recode_it<-function(dat, dict, my_API, what = "all"){
  if(what=="all"){
    vars = init__("all")
  } else {
    vars = what
  }
  
  recoded_dat = dat
  for(v in vars){
    print(v)
    recoded_dat = recoded_dat %>% 
      dplyr::left_join(
        recode_with_metadata(dat = dat, dict = dict, my_API = my_API, what = v), 
        by = c("pid", "record_id")
      )
  }
  return(recoded_dat)
  
}

# Function to export metadata as JSON
export_metadata_json <- function(metadata, filename = "variable_metadata.json") {
  jsonlite::write_json(
    metadata, 
    filename, 
    pretty = TRUE, 
    auto_unbox = TRUE,
    na = "null"
  )
  message(paste("Metadata exported to:", filename))
}

# Function to create a summary table of all variables
create_variable_summary_table <- function(metadata) {
  
  summary_rows <- list()
  
  for(category_name in names(metadata$variable_categories)) {
    category <- metadata$variable_categories[[category_name]]
    
    for(var_name in names(category$variables)) {
      var_info <- category$variables[[var_name]]
      
      row <- data.frame(
        category = category_name,
        variable_name = var_name,
        label = var_info$label,
        data_type = var_info$data_type,
        n_missing = var_info$n_missing,
        missing_pct = var_info$missing_percentage,
        stringsAsFactors = FALSE
      )
      
      # Add type-specific summary info
      if(!is.null(var_info$factor_info)) {
        row$n_levels <- var_info$factor_info$n_levels
        row$summary_info <- paste("Factor with", var_info$factor_info$n_levels, "levels")
      } else if(!is.null(var_info$numeric_info)) {
        row$n_levels <- NA
        row$summary_info <- paste("Mean:", var_info$numeric_info$mean, 
                                  "| Range:", var_info$numeric_info$min, "-", var_info$numeric_info$max)
      } else if(!is.null(var_info$logical_info)) {
        row$n_levels <- 2
        row$summary_info <- paste("Prop. TRUE:", var_info$logical_info$proportion_true)
      } else {
        row$n_levels <- NA
        row$summary_info <- ""
      }
      
      summary_rows[[length(summary_rows) + 1]] <- row
    }
  }
  
  return(do.call(rbind, summary_rows))
}



cpi_ratio_1999 <- function(date_vector) {
  # Ensure required packages are installed
  if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
  
  library(httr)
  library(tidyverse)
  
  # Step 1: Download CPI data from FRED
  url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=CPIAUCSL"
  temp_file <- tempfile(fileext = ".csv")
  GET(url, write_disk(temp_file, overwrite = TRUE))
  
  # Step 2: Read and preprocess CPI data
  cpi_data <- read.csv(temp_file)
  cpi_data$DATE <- as.Date(cpi_data$observation_date)
  cpi_data <- cpi_data %>%
    mutate(
      year = lubridate::year(DATE),
      month = lubridate::month(DATE),
      cpi = CPIAUCSL
    ) %>%
    select(month, year, cpi)
  
  # Step 3: Create lookup for 1999 CPI values by month
  cpi_1999 <- cpi_data %>%
    filter(year == 1999) %>%
    select(month, cpi_1999 = cpi)
  
  # Step 4: Prepare input dates for matching
  input_df <- tibble(
    original_date = as.Date(date_vector),
    year = lubridate::year(original_date),
    month = lubridate::month(original_date)
  )
  
  # Step 5: Join CPI data
  final_df <- input_df %>%
    left_join(cpi_data, by = c("month", "year")) %>%
    left_join(cpi_1999, by = "month") %>%
    mutate(ratio = cpi_1999/cpi)
  
  # Step 6: For dates where the fed has not released a CPI number, simply take the latest value
  final_df = final_df %>% 
    dplyr::mutate(rid = 1:n()) %>% 
    dplyr::arrange(original_date) %>% 
    tidyr::fill(everything(), .direction = "down") %>% 
    dplyr::arrange(rid)
    
  
  # Step 6: Return ratio vector
  return(final_df$ratio)
}


get_poverty_threshold <- function(dates, family_size) {
  
  # Install and load required packages
  required <- c("readxl", "dplyr")
  invisible(lapply(required, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }))
  
  # Ensure required packages are installed and loaded
  required_packages <- c("rvest", "stringr", "lubridate")
  invisible(lapply(required_packages, function(pkg) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
      library(pkg, character.only = TRUE)
    }
  }))
  
  

  # Convert to proper format
  year_vec <- lubridate::year(dates)
  if (any(is.na(year_vec))){message("Invalid dates supplied. Assuming median of observed dates."); year_vec[is.na(year_vec)] = median(year_vec,na.rm=T)}
  

  
  # Download the Excel file to a temp location
  url      <- "https://aspe.hhs.gov/sites/default/files/documents/3edbd42a9b8de4f2a87211283e541ca4/historical-poverty-guidelines-through-2024.xlsx"
  tmp_file <- tempfile(fileext = ".xlsx")
  download.file(url, tmp_file, mode = "wb")
  
  # Identify the sheet containing the 48‐state nonfarm table
  all_sheets   <- readxl::excel_sheets(tmp_file)
  target_sheet <- all_sheets[grepl("48 Contiguous States--Nonfarm", all_sheets, ignore.case = TRUE)]
  
  # Read that sheet into a data frame
  raw_df <- readxl::read_excel(tmp_file, sheet = target_sheet, skip = 3) 
  
  # Locate column positions for "Year" and "$ For Each Additional Person (9+)"
  col_start <- which(names(raw_df) == "Year")
  col_end   <- which(names(raw_df) == "$ For Each Additional Person (9+)")
  
  # Subset to only those columns
  trimmed_df <- raw_df %>% 
    dplyr::select(col_start:col_end) %>% 
    dplyr::rename_all(tolower)
  
  names(trimmed_df) = stringr::str_remove_all(names(trimmed_df), "person") %>% stringr::str_remove_all("s") %>% stringr::str_remove_all("\\$ for") %>% stringr::str_trim("both")
  # Clean up temp file
  unlink(tmp_file)
  

  # Grab latest guidelines from HHS
  guidelines_url <- "https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines"
  page <- read_html(guidelines_url)
  
  # Extract table for 48 contiguous states (latest available)
  tables <- page %>% html_table(header = TRUE)
  poverty_table <- tables[[1]]  # Assumes first table is relevant
  names(poverty_table) = tolower(as.character(poverty_table[1,c(1:2)]))
  poverty_table = poverty_table[-1,] %>% tidyr::pivot_wider(names_from = `persons in family/household`, values_from = `poverty guideline`) %>% 
    dplyr::mutate(year = year(today())) %>% dplyr::relocate(year)
  names(poverty_table) = names(trimmed_df)

  poverty_table = poverty_table %>% tidyr::pivot_longer(`1`:`each additional  (9+)`) %>%
    dplyr::mutate(value = stringr::str_remove_all(value, "\\$") %>% 
                    stringr::str_remove_all(",") %>% 
                    stringr::str_remove_all("For families/households with more than 8 persons add") %>% 
                    stringr::str_remove_all("for each additional person.") %>% 
                    stringr::str_trim("both") %>% as.numeric()
                  ) %>% 
    tidyr::pivot_wider(names_from = name, values_from = value) 
  
  poverty_table = poverty_table %>% 
    dplyr::bind_rows(trimmed_df) %>% 
    dplyr::rename(additional = `each additional  (9+)`)  %>% 
    tidyr::pivot_longer(`1`:`8`, names_to = 'family_size', values_to = "threshold") %>% 
    dplyr::mutate(additional = ifelse(family_size<8, 0, additional)) %>% 
    dplyr::mutate(family_size = as.numeric(family_size))
  
  final_df = data.frame(date = dates, year = year_vec, family_size = family_size) %>% 
    dplyr::mutate(above9 = ifelse(family_size>8, family_size-8, 0), 
                  family_size = ifelse(family_size>8, 8, family_size)
                  ) %>% 
    dplyr::left_join(poverty_table, by = c("year", "family_size")) %>% 
    dplyr::mutate(threshold = threshold + additional*above9 )

  return(final_df$threshold)
}

# Function that binds rows like dplyr::bind_rows but converts 
# conflicting variable types using a data type hierarchy
flexible_bind_rows <- function(..., .id = NULL) {
  # Load required library
  library(dplyr)
  
  # Get all data frames - handle both multiple arguments and list input
  args <- list(...)
  
  # If first argument is a list and it's the only argument, use it as the list of data frames
  if (length(args) == 1 && is.list(args[[1]]) && !is.data.frame(args[[1]])) {
    dfs <- args[[1]]
  } else {
    # Otherwise, treat all arguments as individual data frames
    dfs <- args
  }
  
  # Handle empty input
  if (length(dfs) == 0) {
    return(data.frame())
  }
  
  # Define data type hierarchy (1 = lowest, 4 = highest)
  type_hierarchy <- function(type) {
    switch(type,
           "logical" = 1,
           "integer" = 2,
           "numeric" = 3,
           "double" = 3,   # treat double same as numeric
           "character" = 4,
           4)  # default to character level for unknown types
  }
  
  # Function to convert to target type
  convert_to_type <- function(x, target_type) {
    switch(target_type,
           "logical" = as.logical(x),
           "integer" = as.integer(x),
           "numeric" = as.numeric(x),
           "double" = as.numeric(x),
           "character" = as.character(x),
           as.character(x))  # default to character
  }
  
  # If .id is specified, add it to each data frame
  if (!is.null(.id)) {
    for (i in seq_along(dfs)) {
      dfs[[i]][[.id]] <- i
    }
  }
  
  # Get all unique column names across all data frames
  all_cols <- unique(unlist(lapply(dfs, names)))
  
  # For each column, determine the highest type in hierarchy and convert all instances
  for (col in all_cols) {
    # Get the types of this column across all data frames that have it
    col_types <- character()
    df_indices_with_col <- integer()
    
    for (i in seq_along(dfs)) {
      if (col %in% names(dfs[[i]])) {
        col_types <- c(col_types, class(dfs[[i]][[col]])[1])
        df_indices_with_col <- c(df_indices_with_col, i)
      }
    }
    
    # Find the highest type in the hierarchy
    hierarchy_levels <- sapply(col_types, type_hierarchy)
    max_hierarchy <- max(hierarchy_levels)
    
    # Determine the target type
    target_type <- names(which(sapply(c("logical", "integer", "numeric", "character"), 
                                      function(t) type_hierarchy(t) == max_hierarchy))[1])
    
    # If there are conflicts (more than one unique type), convert all instances
    if (length(unique(col_types)) > 1) {
      cat("Converting column '", col, "' from types [", 
          paste(unique(col_types), collapse = ", "), "] to '", target_type, 
          "' following hierarchy\n", sep = "")
      
      # Convert to target type in all data frames that have this column
      for (i in df_indices_with_col) {
        dfs[[i]][[col]] <- convert_to_type(dfs[[i]][[col]], target_type)
      }
    }
  }
  
  # Now use dplyr::bind_rows since all conflicts are resolved
  return(bind_rows(dfs))
}


clean_mental_health_ace_data <- function(df) {
  
  # Function to clean and process mental health and ACE variables
  # Author: Claude
  # Date: 2025
  # Purpose: Clean PHQ-2, GAD-2, and ACE variables with NSCH-compatible naming
  # Example usage:
  # cleaned_data <- clean_mental_health_ace_data(your_dataframe)
  # 
  # # View variable labels
  # library(labelled)
  # look_for(cleaned_data)
  # 
  # # Basic descriptive statistics
  # summary(cleaned_data[c("phq2_total", "gad2_total", "ace_total")])
  # 
  # # Cross-tabulation example
  # table(cleaned_data$phq2_positive, cleaned_data$ace_risk_cat, useNA = "ifany")
  
  # Load required packages
  if (!require(labelled)) {
    stop("The 'labelled' package is required. Please install it with: install.packages('labelled')")
  }
  
  # =============================================================================
  # CONVERT ALL VARIABLE NAMES TO LOWERCASE
  # =============================================================================
  
  # Convert all column names to lowercase
  names(df) <- tolower(names(df))
  cat("Converted all variable names to lowercase\n")
  
  # =============================================================================
  # IDENTIFY AND SELECT RELEVANT VARIABLES
  # =============================================================================
  
  # Identify unique identifier variables (lowercase)
  id_vars <- c()
  if ("pid" %in% names(df)) {
    id_vars <- c(id_vars, "pid")
  }
  if ("record_id" %in% names(df)) {
    id_vars <- c(id_vars, "record_id")
  }
  
  # Identify PHQ-2, GAD-2, and ACE variables (lowercase)
  phq_gad_vars <- c("cqfb013", "cqfb014", "cqfb015", "cqfb016")
  ace_vars <- paste0("cace", 1:10)  # Caregiver ACE variables
  child_ace_vars <- paste0("cqr0", 17:24)  # Child ACE variables
  
  # Select only relevant variables that exist in the dataframe
  relevant_vars <- c(id_vars, 
                     phq_gad_vars[phq_gad_vars %in% names(df)],
                     ace_vars[ace_vars %in% names(df)],
                     child_ace_vars[child_ace_vars %in% names(df)])
  
  # Create cleaned dataframe with only relevant variables
  cleaned_df <- df[, relevant_vars, drop = FALSE]
  
  cat("Selected", length(relevant_vars), "relevant variables:\n")
  cat("- Identifiers:", length(id_vars), "\n")
  cat("- PHQ-2/GAD-2:", sum(phq_gad_vars %in% names(df)), "\n")
  cat("- Caregiver ACE variables:", sum(ace_vars %in% names(df)), "\n")
  cat("- Child ACE variables:", sum(child_ace_vars %in% names(df)), "\n")
  
  # =============================================================================
  # VARIABLE RENAMING (Following NSCH naming conventions where possible)
  # =============================================================================
  
  # PHQ-2 Variables (Depression Screening)
  if ("cqfb013" %in% names(cleaned_df)) {
    names(cleaned_df)[names(cleaned_df) == "cqfb013"] <- "phq2_interest"
    cat("Renamed cqfb013 -> phq2_interest\n")
  }
  
  if ("cqfb014" %in% names(cleaned_df)) {
    names(cleaned_df)[names(cleaned_df) == "cqfb014"] <- "phq2_depressed"
    cat("Renamed cqfb014 -> phq2_depressed\n")
  }
  
  # GAD-2 Variables (Anxiety Screening)
  if ("cqfb015" %in% names(cleaned_df)) {
    names(cleaned_df)[names(cleaned_df) == "cqfb015"] <- "gad2_nervous"
    cat("Renamed cqfb015 -> gad2_nervous\n")
  }
  
  if ("cqfb016" %in% names(cleaned_df)) {
    names(cleaned_df)[names(cleaned_df) == "cqfb016"] <- "gad2_worry"
    cat("Renamed cqfb016 -> gad2_worry\n")
  }
  
  # ACE Variables (Following NSCH ACE naming conventions)
  ace_mapping <- list(
    "cace1" = "ace_neglect",           # Physical/emotional neglect
    "cace2" = "ace_parent_loss",       # Parental loss/separation
    "cace3" = "ace_mental_illness",    # Household mental illness/suicide
    "cace4" = "ace_substance_use",     # Household substance abuse
    "cace5" = "ace_domestic_violence", # Domestic violence
    "cace6" = "ace_incarceration",     # Household member incarceration
    "cace7" = "ace_verbal_abuse",      # Verbal/emotional abuse
    "cace8" = "ace_physical_abuse",    # Physical abuse
    "cace9" = "ace_emotional_neglect", # Emotional neglect (feeling unloved)
    "cace10" = "ace_sexual_abuse"      # Sexual abuse
  )
  
  for (old_name in names(ace_mapping)) {
    if (old_name %in% names(cleaned_df)) {
      new_name <- ace_mapping[[old_name]]
      names(cleaned_df)[names(cleaned_df) == old_name] <- new_name
      cat("Renamed", old_name, "->", new_name, "\n")
    }
  }
  
  # Child ACE Variables (Following consistent naming with child prefix)
  child_ace_mapping <- list(
    "cqr017" = "child_ace_parent_divorce",     # Parent or guardian divorced or separated
    "cqr018" = "child_ace_parent_death",       # Parent or guardian died
    "cqr019" = "child_ace_parent_jail",        # Parent or guardian served time in jail
    "cqr020" = "child_ace_domestic_violence",  # Saw/heard parents/adults hit each other
    "cqr021" = "child_ace_neighborhood_violence", # Victim/witnessed neighborhood violence
    "cqr022" = "child_ace_mental_illness",     # Lived with mentally ill/suicidal person
    "cqr023" = "child_ace_substance_use",      # Lived with person with alcohol/drug problems
    "cqr024" = "child_ace_discrimination"      # Treated unfairly due to race/ethnicity
  )
  
  for (old_name in names(child_ace_mapping)) {
    if (old_name %in% names(cleaned_df)) {
      new_name <- child_ace_mapping[[old_name]]
      names(cleaned_df)[names(cleaned_df) == old_name] <- new_name
      cat("Renamed", old_name, "->", new_name, "\n")
    }
  }
  
  # =============================================================================
  # DATA CLEANING
  # =============================================================================
  
  # Function to clean individual variables
  clean_variable <- function(x) {
    # Convert to character first to handle different data types
    x_char <- as.character(x)
    
    # Patterns that indicate missing/refused responses
    missing_patterns <- c(
      "Don't know", "don't know", "Dont know", "dont know",
      "Prefer not to answer", "prefer not to answer",
      "Refused", "refused", "REFUSED",
      "Missing", "missing", "MISSING",
      "NA", "N/A", "n/a",
      "-99", "-98", "-97", "-96", "-95",  # Common missing value codes
      "99", "98", "97", "96", "95"       # Alternative missing codes
    )
    
    # Set matching values to NA
    x_char[x_char %in% missing_patterns] <- NA
    
    # Remove leading/trailing whitespace
    x_char <- trimws(x_char)
    
    # Convert empty strings to NA
    x_char[x_char == ""] <- NA
    
    return(x_char)
  }
  
  # Clean all variables
  cat("\n=== CLEANING DATA ===\n")
  for (col_name in names(cleaned_df)) {
    original_na_count <- sum(is.na(cleaned_df[[col_name]]))
    cleaned_df[[col_name]] <- clean_variable(cleaned_df[[col_name]])
    new_na_count <- sum(is.na(cleaned_df[[col_name]]))
    
    if (new_na_count > original_na_count) {
      cat("Cleaned", col_name, ": converted", new_na_count - original_na_count, 
          "additional values to missing\n")
    }
  }
  
  # =============================================================================
  # VARIABLE LABELING
  # =============================================================================
  
  cat("\n=== ADDING VARIABLE LABELS ===\n")
  
  # PHQ-2 Labels and Values (convert to numeric if needed, then add labels)
  if ("phq2_interest" %in% names(cleaned_df)) {
    # Convert to numeric if it's character, keeping existing numeric values
    cleaned_df$phq2_interest <- as.numeric(as.character(cleaned_df$phq2_interest))
    
    # Add value labels to numeric data
    val_labels(cleaned_df$phq2_interest) <- c(
      "Not at all" = 0,
      "Several days" = 1,
      "More than half the days" = 2,
      "Nearly every day" = 3
    )
    
    # Add variable label
    var_label(cleaned_df$phq2_interest) <- "PHQ-2 Item 1: Responding caregiver - Little interest or pleasure in doing things (past 2 weeks)"
    cat("Labeled phq2_interest\n")
  }
  
  if ("phq2_depressed" %in% names(cleaned_df)) {
    # Convert to numeric if it's character, keeping existing numeric values
    cleaned_df$phq2_depressed <- as.numeric(as.character(cleaned_df$phq2_depressed))
    
    # Add value labels to numeric data
    val_labels(cleaned_df$phq2_depressed) <- c(
      "Not at all" = 0,
      "Several days" = 1,
      "More than half the days" = 2,
      "Nearly every day" = 3
    )
    
    var_label(cleaned_df$phq2_depressed) <- "PHQ-2 Item 2: Responding caregiver - Feeling down, depressed, or hopeless (past 2 weeks)"
    cat("Labeled phq2_depressed\n")
  }
  
  # GAD-2 Labels and Values (convert to numeric if needed, then add labels)
  if ("gad2_nervous" %in% names(cleaned_df)) {
    # Convert to numeric if it's character, keeping existing numeric values
    cleaned_df$gad2_nervous <- as.numeric(as.character(cleaned_df$gad2_nervous))
    
    # Add value labels to numeric data
    val_labels(cleaned_df$gad2_nervous) <- c(
      "Not at all" = 0,
      "Several days" = 1,
      "More than half the days" = 2,
      "Nearly every day" = 3
    )
    
    var_label(cleaned_df$gad2_nervous) <- "GAD-2 Item 1: Responding caregiver - Feeling nervous, anxious, or on edge (past 2 weeks)"
    cat("Labeled gad2_nervous\n")
  }
  
  if ("gad2_worry" %in% names(cleaned_df)) {
    # Convert to numeric if it's character, keeping existing numeric values
    cleaned_df$gad2_worry <- as.numeric(as.character(cleaned_df$gad2_worry))
    
    # Add value labels to numeric data
    val_labels(cleaned_df$gad2_worry) <- c(
      "Not at all" = 0,
      "Several days" = 1,
      "More than half the days" = 2,
      "Nearly every day" = 3
    )
    
    var_label(cleaned_df$gad2_worry) <- "GAD-2 Item 2: Responding caregiver - Not being able to stop or control worrying (past 2 weeks)"
    cat("Labeled gad2_worry\n")
  }
  
  # ACE Variables (Binary coding: 0 = No, 1 = Yes)
  ace_labels <- list(
    "ace_neglect" = "ACE: Responding caregiver - Physical/emotional neglect during childhood (first 18 years)",
    "ace_parent_loss" = "ACE: Responding caregiver - Lost parent through divorce, abandonment, death, etc. (first 18 years)",
    "ace_mental_illness" = "ACE: Responding caregiver - Lived with someone with mental illness/depression/suicide (first 18 years)",
    "ace_substance_use" = "ACE: Responding caregiver - Lived with someone with alcohol/drug problems (first 18 years)",
    "ace_domestic_violence" = "ACE: Responding caregiver - Witnessed domestic violence between parents/adults (first 18 years)",
    "ace_incarceration" = "ACE: Responding caregiver - Lived with someone who went to jail/prison (first 18 years)",
    "ace_verbal_abuse" = "ACE: Responding caregiver - Experienced verbal/emotional abuse from parent/adult (first 18 years)",
    "ace_physical_abuse" = "ACE: Responding caregiver - Experienced physical abuse from parent/adult (first 18 years)",
    "ace_emotional_neglect" = "ACE: Responding caregiver - Felt unloved or not special in family (first 18 years)",
    "ace_sexual_abuse" = "ACE: Responding caregiver - Experienced unwanted sexual contact (first 18 years)"
  )
  
  for (var_name in names(ace_labels)) {
    if (var_name %in% names(cleaned_df)) {
      # Convert to numeric if it's character, keeping existing numeric values
      cleaned_df[[var_name]] <- as.numeric(as.character(cleaned_df[[var_name]]))
      
      # Add value labels to numeric data (coded as 0=No, 1=Yes)
      val_labels(cleaned_df[[var_name]]) <- c("No" = 0, "Yes" = 1)
      
      # Add variable label
      var_label(cleaned_df[[var_name]]) <- ace_labels[[var_name]]
      cat("Labeled", var_name, "\n")
    }
  }
  
  # Child ACE Variables (Binary coding: 0 = No, 1 = Yes)
  child_ace_labels <- list(
    "child_ace_parent_divorce" = "Child ACE: Reported by caregiver - Child experienced parent/guardian divorce or separation",
    "child_ace_parent_death" = "Child ACE: Reported by caregiver - Child experienced parent/guardian death",
    "child_ace_parent_jail" = "Child ACE: Reported by caregiver - Child's parent/guardian served time in jail",
    "child_ace_domestic_violence" = "Child ACE: Reported by caregiver - Child saw/heard parents or adults hit each other in home",
    "child_ace_neighborhood_violence" = "Child ACE: Reported by caregiver - Child was victim/witnessed violence in neighborhood",
    "child_ace_mental_illness" = "Child ACE: Reported by caregiver - Child lived with someone mentally ill, suicidal, or severely depressed",
    "child_ace_substance_use" = "Child ACE: Reported by caregiver - Child lived with someone with alcohol/drug problems",
    "child_ace_discrimination" = "Child ACE: Reported by caregiver - Child treated unfairly due to race/ethnicity"
  )
  
  for (var_name in names(child_ace_labels)) {
    if (var_name %in% names(cleaned_df)) {
      # Convert to numeric if it's character, keeping existing numeric values
      cleaned_df[[var_name]] <- as.numeric(as.character(cleaned_df[[var_name]]))
      
      # Add value labels to numeric data (coded as 0=No, 1=Yes)
      val_labels(cleaned_df[[var_name]]) <- c("No" = 0, "Yes" = 1)
      
      # Add variable label
      var_label(cleaned_df[[var_name]]) <- child_ace_labels[[var_name]]
      cat("Labeled", var_name, "\n")
    }
  }
  
  # =============================================================================
  # CREATE COMPOSITE SCORES
  # =============================================================================
  
  cat("\n=== CREATING COMPOSITE SCORES ===\n")
  
  # PHQ-2 Total Score (0-6)
  if (all(c("phq2_interest", "phq2_depressed") %in% names(cleaned_df))) {
    cleaned_df$phq2_total <- rowSums(
      cleaned_df[c("phq2_interest", "phq2_depressed")], 
      na.rm = FALSE
    )
    var_label(cleaned_df$phq2_total) <- "PHQ-2 Total Score (0-6): Responding caregiver - Depression screening score"
    cat("Created phq2_total score\n")
  }
  
  # GAD-2 Total Score (0-6)
  if (all(c("gad2_nervous", "gad2_worry") %in% names(cleaned_df))) {
    cleaned_df$gad2_total <- rowSums(
      cleaned_df[c("gad2_nervous", "gad2_worry")], 
      na.rm = FALSE
    )
    var_label(cleaned_df$gad2_total) <- "GAD-2 Total Score (0-6): Responding caregiver - Anxiety screening score"
    cat("Created gad2_total score\n")
  }
  
  # ACE Total Score (0-10) for caregiver
  ace_vars <- names(cleaned_df)[grepl("^ace_", names(cleaned_df))]
  if (length(ace_vars) > 0) {
    cleaned_df$ace_total <- rowSums(
      cleaned_df[ace_vars], 
      na.rm = FALSE
    )
    var_label(cleaned_df$ace_total) <- "ACE Total Score (0-10): Responding caregiver - Total count of adverse childhood experiences"
    cat("Created ace_total score from", length(ace_vars), "caregiver ACE variables\n")
  }
  
  # Child ACE Total Score (0-8) 
  child_ace_vars <- names(cleaned_df)[grepl("^child_ace_", names(cleaned_df))]
  if (length(child_ace_vars) > 0) {
    cleaned_df$child_ace_total <- rowSums(
      cleaned_df[child_ace_vars], 
      na.rm = FALSE
    )
    var_label(cleaned_df$child_ace_total) <- "Child ACE Total Score (0-8): Reported by caregiver - Total count of child's adverse childhood experiences"
    cat("Created child_ace_total score from", length(child_ace_vars), "child ACE variables\n")
  }
  
  # Clinical cutoffs and risk categories (binary and multi-level indicators)
  if ("phq2_total" %in% names(cleaned_df)) {
    # Binary positive screen (≥3)
    cleaned_df$phq2_positive <- ifelse(cleaned_df$phq2_total >= 3, 1, 0)
    var_label(cleaned_df$phq2_positive) <- "PHQ-2 Positive Screen (≥3): Responding caregiver - Indicates likely depression, further evaluation needed"
    cat("Created phq2_positive cutoff (>=3)\n")
    
    # PHQ-2 Risk Categories (based on clinical literature)
    cleaned_df$phq2_risk_cat <- case_when(
      cleaned_df$phq2_total %in% 0:1 ~ 0,
      cleaned_df$phq2_total == 2 ~ 1,
      cleaned_df$phq2_total %in% 3:6 ~ 2,
      TRUE ~ NA_real_
    )
    
    val_labels(cleaned_df$phq2_risk_cat) <- c(
      "Minimal/None" = 0,
      "Mild" = 1,
      "Moderate/Severe" = 2
    )
    
    var_label(cleaned_df$phq2_risk_cat) <- "PHQ-2 Risk Category: Responding caregiver - 0=Minimal/None(0-1), 1=Mild(2), 2=Moderate/Severe(3-6)"
    cat("Created phq2_risk_cat\n")
  }
  
  if ("gad2_total" %in% names(cleaned_df)) {
    # Binary positive screen (≥3)
    cleaned_df$gad2_positive <- ifelse(cleaned_df$gad2_total >= 3, 1, 0)
    var_label(cleaned_df$gad2_positive) <- "GAD-2 Positive Screen (≥3): Responding caregiver - Indicates likely anxiety, further evaluation needed"
    cat("Created gad2_positive cutoff (>=3)\n")
    
    # GAD-2 Risk Categories (based on GAD-7 severity levels, scaled to GAD-2 range)
    cleaned_df$gad2_risk_cat <- case_when(
      cleaned_df$gad2_total %in% 0:1 ~ 0,
      cleaned_df$gad2_total == 2 ~ 1,
      cleaned_df$gad2_total %in% 3:4 ~ 2,
      cleaned_df$gad2_total %in% 5:6 ~ 3,
      TRUE ~ NA_real_
    )
    
    val_labels(cleaned_df$gad2_risk_cat) <- c(
      "Minimal/None" = 0,
      "Mild" = 1,
      "Moderate" = 2,
      "Severe" = 3
    )
    
    var_label(cleaned_df$gad2_risk_cat) <- "GAD-2 Risk Category: Responding caregiver - 0=Minimal/None(0-1), 1=Mild(2), 2=Moderate(3-4), 3=Severe(5-6)"
    cat("Created gad2_risk_cat\n")
  }
  
  if ("ace_total" %in% names(cleaned_df)) {
    # ACE risk categories (common in literature)
    cleaned_df$ace_risk_cat <- case_when(
      cleaned_df$ace_total == 0 ~ 0,
      cleaned_df$ace_total == 1 ~ 1,
      cleaned_df$ace_total %in% 2:3 ~ 2,
      cleaned_df$ace_total >= 4 ~ 3,
      TRUE ~ NA_real_
    )
    
    val_labels(cleaned_df$ace_risk_cat) <- c(
      "No ACEs" = 0,
      "1 ACE" = 1,
      "2-3 ACEs" = 2,
      "4+ ACEs" = 3
    )
    
    var_label(cleaned_df$ace_risk_cat) <- "ACE Risk Category: Responding caregiver - 0=None, 1=Low(1), 2=Moderate(2-3), 3=High(4+)"
    cat("Created ace_risk_cat\n")
  }
  
  if ("child_ace_total" %in% names(cleaned_df)) {
    # Child ACE risk categories (adapted for 0-8 scale)
    cleaned_df$child_ace_risk_cat <- case_when(
      cleaned_df$child_ace_total == 0 ~ 0,
      cleaned_df$child_ace_total == 1 ~ 1,
      cleaned_df$child_ace_total %in% 2:3 ~ 2,
      cleaned_df$child_ace_total >= 4 ~ 3,
      TRUE ~ NA_real_
    )
    
    val_labels(cleaned_df$child_ace_risk_cat) <- c(
      "No ACEs" = 0,
      "1 ACE" = 1,
      "2-3 ACEs" = 2,
      "4+ ACEs" = 3
    )
    
    var_label(cleaned_df$child_ace_risk_cat) <- "Child ACE Risk Category: Reported by caregiver - 0=None, 1=Low(1), 2=Moderate(2-3), 3=High(4+)"
    cat("Created child_ace_risk_cat\n")
  }
  
  # =============================================================================
  # SUMMARY REPORT
  # =============================================================================
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("DATA CLEANING SUMMARY\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  cat("Variables processed:\n")
  cat("- PHQ-2 (Depression):", sum(c("phq2_interest", "phq2_depressed") %in% names(cleaned_df)), "of 2 items\n")
  cat("- GAD-2 (Anxiety):", sum(c("gad2_nervous", "gad2_worry") %in% names(cleaned_df)), "of 2 items\n")
  cat("- Caregiver ACE Variables:", length(ace_vars), "adverse childhood experiences\n")
  cat("- Child ACE Variables:", length(child_ace_vars), "child adverse experiences\n")
  cat("- Total variables in dataset:", ncol(cleaned_df), "\n")
  
  cat("\nComposite scores created:\n")
  composite_vars <- c("phq2_total", "gad2_total", "ace_total", "child_ace_total", 
                      "phq2_positive", "gad2_positive", 
                      "phq2_risk_cat", "gad2_risk_cat", "ace_risk_cat", "child_ace_risk_cat")
  for (var in composite_vars) {
    if (var %in% names(cleaned_df)) {
      cat("-", var, ": Available\n")
    }
  }
  
  cat("\nRecommendations:\n")
  cat("- PHQ-2 ≥3: Further evaluation with PHQ-9 recommended\n")
  cat("- GAD-2 ≥3: Further evaluation with GAD-7 recommended\n")
  cat("- PHQ-2 Risk: 0=Minimal/None(0-1), 1=Mild(2), 2=Moderate/Severe(3-6)\n")
  cat("- GAD-2 Risk: 0=Minimal/None(0-1), 1=Mild(2), 2=Moderate(3-4), 3=Severe(5-6)\n")
  cat("- Caregiver ACE scores: Higher scores associated with increased health risks\n")
  cat("- Child ACE scores: Higher scores may impact child development and wellbeing\n")
  cat("- Missing data: Review patterns and consider multiple imputation if needed\n")
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  
  return(cleaned_df %>% dplyr::mutate(pid = as.integer(pid), record_id = as.integer(record_id)))
}

clean_childcare_variables <- function(df) {
  
  #' Clean and Process Child Care Variables
  #'
  #' This function takes a data frame with lowercase variable names, identifies child care
  #' variables based on the codebook, renames them descriptively, cleans missing values,
  #' and applies appropriate labels and factor conversions.
  #'
  #' @param df A data frame containing child care variables with lowercase names
  #' @return A cleaned data frame with renamed variables, proper labels, and factor conversions
  #' @import dplyr
  #' @import labelled
  #' @export
  
  # Example usage:
  # cleaned_data <- clean_childcare_variables(raw_data)
  # 
  # # View variable labels
  # labelled::look_for(cleaned_data)
  # 
  # # View structure of cleaned data
  # str(cleaned_data)
  
  # Load required packages
  if (!require(dplyr, quietly = TRUE)) {
    stop("Package 'dplyr' is required but not installed.")
  }
  if (!require(labelled, quietly = TRUE)) {
    stop("Package 'labelled' is required but not installed.")
  }
  
  # Create a copy of the input data frame
  cleaned_df <- df
  
  # Define variable mappings based on codebook analysis
  # Format: original_name = list(new_name, type, labels, var_label)
  variable_mappings <- list(
    
    # Access and Difficulty Variables
    "mmi013" = list(
      new_name = "cc_access_difficulty",
      type = "categorical",
      levels = c(0, 1, 2, 3, 99),
      labels = c("Did not need childcare", "Not difficult", "Somewhat difficult", 
                 "Very difficult", "Missing/Don't know"),
      var_label = "Difficulty finding child care (past 12 months)"
    ),
    
    "mmi014" = list(
      new_name = "cc_difficulty_reason",
      type = "categorical", 
      levels = c(1, 2, 3, 4, 5, 6, 7, 99),
      labels = c("Cost too high", "No openings", "Location not convenient", 
                 "Hours not suitable", "Quality not satisfactory", 
                 "Transportation difficulties", "Other", "Missing/Not applicable"),
      var_label = "Main reason child care was difficult to find"
    ),
    
    # Child Care Receipt and Type Variables
    "cqfb007x" = list(
      new_name = "cc_receives_care",
      type = "categorical",
      levels = c(0, 1, 99),
      labels = c("No", "Yes", "Missing"),
      var_label = "Child receives non-parental care (10+ hours/week)"
    ),
    
    "mmi000" = list(
      new_name = "cc_primary_type",
      type = "categorical",
      levels = c(1, 2, 3, 4, 5, 6, 99),
      labels = c("Relative care", "Non-relative care", "Childcare center", 
                 "Preschool program", "Head Start/Early Head Start", 
                 "Other", "Missing/Not applicable"),
      var_label = "Primary child care arrangement type"
    ),
    
    # Cost Variables (Numeric)
    "mrw002" = list(
      new_name = "cc_weekly_cost_all",
      type = "numeric",
      var_label = "Weekly household child care costs - all children ($)"
    ),
    
    "mmi003" = list(
      new_name = "cc_weekly_cost_primary",
      type = "numeric", 
      var_label = "Weekly cost - primary child care arrangement ($)"
    ),
    
    "mmi003b" = list(
      new_name = "cc_weekly_cost_total",
      type = "numeric",
      var_label = "Weekly cost - all arrangements this child ($)"
    ),
    
    # Financial Support Variables
    "mrw003_1" = list(
      new_name = "cc_family_support_all",
      type = "numeric",
      var_label = "Weekly family financial support - all children ($)"
    ),
    
    "mrw003_2" = list(
      new_name = "cc_family_support_child",
      type = "numeric", 
      var_label = "Weekly family financial support - this child ($)"
    ),
    
    "mmi018" = list(
      new_name = "cc_receives_subsidy",
      type = "categorical",
      levels = c(0, 1, 99),
      labels = c("No", "Yes", "Missing"),
      var_label = "Receives child care subsidy assistance"
    ),
    
    # Impact and Quality Variables
    "mmi009" = list(
      new_name = "cc_financial_hardship",
      type = "categorical",
      levels = c(0, 1, 99),
      labels = c("No", "Yes", "Missing"),
      var_label = "Child care costs create financial hardship"
    ),
    
    "q941" = list(
      new_name = "cc_quality_satisfaction",
      type = "categorical",
      levels = c(1, 2, 3, 4, 5, 99),
      labels = c("Very dissatisfied", "Dissatisfied", "Neither", 
                 "Satisfied", "Very satisfied", "Missing"),
      var_label = "Satisfaction with primary child care quality"
    ),
    
    # Hours and Schedule Variables
    "q958" = list(
      new_name = "cc_hours_per_week",
      type = "numeric",
      var_label = "Total hours in child care per week"
    ),
    
    "mmi100" = list(
      new_name = "cc_nonstandard_hours",
      type = "categorical",
      levels = c(0, 1, 99),
      labels = c("No", "Yes", "Missing"),
      var_label = "Requires evening/weekend/overnight care"
    ),
    
    # Subsidy Satisfaction Variables
    "mmi019_1" = list(
      new_name = "cc_subsidy_sat_process",
      type = "categorical",
      levels = c(1, 2, 3, 4, 5, 99),
      labels = c("Very dissatisfied", "Dissatisfied", "Neither", 
                 "Satisfied", "Very satisfied", "Missing"),
      var_label = "Satisfaction with subsidy application process"
    ),
    
    "mmi019_2" = list(
      new_name = "cc_subsidy_sat_amount",
      type = "categorical", 
      levels = c(1, 2, 3, 4, 5, 99),
      labels = c("Very dissatisfied", "Dissatisfied", "Neither",
                 "Satisfied", "Very satisfied", "Missing"),
      var_label = "Satisfaction with subsidy amount"
    ),
    
    "mmi019_3" = list(
      new_name = "cc_subsidy_sat_options",
      type = "categorical",
      levels = c(1, 2, 3, 4, 5, 99), 
      labels = c("Very dissatisfied", "Dissatisfied", "Neither",
                 "Satisfied", "Very satisfied", "Missing"),
      var_label = "Satisfaction with subsidy care options"
    ),
    
    # Multiple Child Payment Variable
    "mrw001" = list(
      new_name = "cc_pays_multiple_children",
      type = "categorical",
      levels = c(0, 1, 99),
      labels = c("No", "Yes", "Missing"),
      var_label = "Pays for childcare for multiple children (10+ hrs/week)"
    )
  )
  
  # Start with ID variables
  result_df <- cleaned_df %>%
    select(all_of(c("pid", "record_id")))
  
  # Process each child care variable
  for (orig_var in names(variable_mappings)) {
    
    if (orig_var %in% names(cleaned_df)) {
      
      var_info <- variable_mappings[[orig_var]]
      new_var_name <- var_info$new_name
      
      # Get the original variable
      var_data <- cleaned_df[[orig_var]]
      
      # Convert common missing value codes to NA
      # Adjust these based on your specific missing value codes
      missing_codes <- c(-99, -98, -97, -9, -8, -7, 99, 98, 97, 9999, 9998, 9997)
      var_data[var_data %in% missing_codes] <- NA
      
      if (var_info$type == "categorical") {
        
        # Convert to factor with proper levels and labels
        var_data <- factor(var_data, 
                           levels = var_info$levels,
                           labels = var_info$labels)
        
        # Add variable label
        var_data <- labelled::set_variable_labels(var_data, var_info$var_label)
        
      } else if (var_info$type == "numeric") {
        
        # Convert to numeric and handle outliers for cost variables
        var_data <- as.numeric(var_data)
        
        # Flag extreme outliers for cost variables (optional data quality check)
        if (grepl("cost|support", new_var_name, ignore.case = TRUE)) {
          # Flag weekly costs > $2000 as potential data quality issues
          if (any(var_data > 2000, na.rm = TRUE)) {
            warning(paste("Variable", new_var_name, "contains values > $2000/week. Consider reviewing."))
          }
        }
        
        # Flag hours > 168 for hours variables
        if (grepl("hours", new_var_name, ignore.case = TRUE)) {
          if (any(var_data > 168, na.rm = TRUE)) {
            warning(paste("Variable", new_var_name, "contains values > 168 hours/week. Consider reviewing."))
          }
        }
        
        # Add variable label using labelled package
        var_data <- labelled::set_variable_labels(var_data, var_info$var_label)
      }
      
      # Add processed variable to result dataframe
      result_df[[new_var_name]] <- var_data
      
      cat("Processed:", orig_var, "->", new_var_name, "\n")
    }
  }
  
  # Add some derived variables
  result_df <- result_df %>%
    mutate(
      # Binary indicator for any formal care (center or preschool)
      cc_formal_care = case_when(
        cc_primary_type %in% c("Childcare center", "Preschool program", "Head Start/Early Head Start") ~ 
          factor(1, levels = c(0, 1), labels = c("No", "Yes")),
        !is.na(cc_primary_type) ~ factor(0, levels = c(0, 1), labels = c("No", "Yes")),
        TRUE ~ NA
      ),
      
      # Care intensity categories
      cc_intensity = case_when(
        cc_hours_per_week < 30 ~ factor(1, levels = c(1, 2, 3), labels = c("Part-time (<30 hrs)", "Full-time (30-50 hrs)", "Extended (>50 hrs)")),
        cc_hours_per_week >= 30 & cc_hours_per_week <= 50 ~ factor(2, levels = c(1, 2, 3), labels = c("Part-time (<30 hrs)", "Full-time (30-50 hrs)", "Extended (>50 hrs)")),
        cc_hours_per_week > 50 ~ factor(3, levels = c(1, 2, 3), labels = c("Part-time (<30 hrs)", "Full-time (30-50 hrs)", "Extended (>50 hrs)")),
        TRUE ~ NA
      ),
      
      # Binary indicator for receiving any financial support
      cc_any_support = case_when(
        (!is.na(cc_family_support_all) & cc_family_support_all > 0) |
          (!is.na(cc_family_support_child) & cc_family_support_child > 0) |
          cc_receives_subsidy == "Yes" ~ factor(1, levels = c(0, 1), labels = c("No", "Yes")),
        TRUE ~ factor(0, levels = c(0, 1), labels = c("No", "Yes"))
      )
    )
  
  # Add variable labels for derived variables
  result_df$cc_formal_care <- labelled::set_variable_labels(result_df$cc_formal_care, 
                                                            "Uses formal child care (center/preschool)")
  result_df$cc_intensity <- labelled::set_variable_labels(result_df$cc_intensity, 
                                                          "Child care intensity level")
  result_df$cc_any_support <- labelled::set_variable_labels(result_df$cc_any_support, 
                                                            "Receives any child care financial support")
  
  # Print summary of processed variables
  cat("\n=== PROCESSING SUMMARY ===\n")
  cat("Variables processed:", sum(names(variable_mappings) %in% names(cleaned_df)), 
      "out of", length(variable_mappings), "possible\n")
  cat("Final dataset contains:", ncol(result_df), "variables\n")
  cat("Derived variables added: cc_formal_care, cc_intensity, cc_any_support\n")
  
  return(result_df)
}


