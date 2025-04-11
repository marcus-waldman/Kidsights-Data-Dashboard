value_labels<-function(lex, dict,varname = "lex_ne25"){
  
  # Note issue issue in education labels due to commas in description
  tmp = dict[[lex]]$select_choices_or_calculations %>% stringr::str_split_1(" \\| ")
  outdf = data.frame(value = rep(NA,length(tmp)), label = NA)
  for(i in 1:length(tmp)){
    tmp_i = tmp[i] %>% stringr::str_split_1(", ")
    outdf$value[i] = tmp_i[1]
    outdf$label[i] = tmp_i[2]
  }
  outdf = outdf %>% 
    dplyr::mutate(var = lex) %>% 
    dplyr::relocate(var)
  
  names(outdf)[1] = varname
  
  return(outdf)
}

recode__<-function(dat, dict, what = NULL){
  
  recodes_df = NULL
  
  if(what %in% c("race", "ethnicity")){
    raceth_df = dat %>% 
      dplyr::select(pid, record_id, dplyr::starts_with("sq003"), dplyr::starts_with("sq002_")) %>% 
      tidyr::pivot_longer(sq002___1:sq002___16, names_to = "var", values_to = "response") %>% 
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
      dplyr::reframe(hisp = ifelse(sq003[1]==1, "Hispanic", "non-Hisp.")) %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(pid, record_id) %>% 
      dplyr::reframe(hisp = hisp[1], race = ifelse(n()>1, "Two or More", label[1])) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(raceG = ifelse(hisp == "Hispanic", "Hispanic", paste0(race,", non-Hisp."))) %>% 
      dplyr::mutate(across(where(is.character), as.factor)) %>% 
      dplyr::select(pid:record_id, hisp, race, raceG)
    
    #Set baseline categories
    raceth_df$hisp = relevel(raceth_df$hisp, ref = "non-Hisp.")
    raceth_df$race = relevel(raceth_df$race, ref = "White")
    raceth_df$raceG = relevel(raceth_df$raceG, ref = "White, non-Hisp.")
    
    recodes_df = raceth_df 
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
      
    
    relate_df$relation1 = relevel(relate_df$relation1, value_labels(lex = "cqr008",dict = dict)$label[1])
    relate_df$relation2 = relevel(relate_df$relation2, value_labels(lex = "nschj013",dict = dict)$label[1])


    recodes_df = relate_df
    
  }
  
  if(what %in% c("education")){
    #educ_df = dat %>% 
    
    simple_educ = data.frame(educ=  value_labels(lex = "cqr004",dict = dict)$label) %>% 
      dplyr::mutate(educ4 = c(rep("Less than High School Graduate", 2), 
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
    
    educ_df = dat %>% dplyr::select(-dplyr::any_of(c("relation1","relation2","mom_a1"))) %>% 
      recode_it(dict=dict, what = "caregiver relationship") %>% 
      dplyr::mutate(educ_max = ifelse( is.na(nschj017), cqr004, NA),
                    educ_max = ifelse( nschj017 > cqr004, nschj017, educ_max),
                    educ_max = ifelse( nschj017 <= cqr004, cqr004, educ_max),
                    educ_max = ifelse(is.na(educ_max),cqr004,educ_max),
                    educ_max = factor(educ_max, levels = value_labels(lex = "cqr004",dict = dict)$value, labels = value_labels(lex = "cqr004",dict = dict)$label),
                    educ_a1 =  factor(cqr004, levels = value_labels(lex = "cqr004",dict = dict)$value, labels = value_labels(lex = "cqr004",dict = dict)$label), 
                    educ_a2 =  factor(nschj017, levels = value_labels(lex = "nschj017",dict = dict)$value, labels = value_labels(lex = "nschj017",dict = dict)$label), 
                    educ_mom = ifelse(mom_a1, educ_a1, NA) %>% factor(levels = value_labels(lex = "cqr004",dict = dict)$value, labels = value_labels(lex = "cqr004",dict = dict)$label), 
                    
                    educ4_max = plyr::mapvalues(as.character(educ_max), from = simple_educ$educ, to = simple_educ$educ4, warn_missing = F),
                    educ4_a1 = plyr::mapvalues(as.character(educ_a1), from = simple_educ$educ, to = simple_educ$educ4, warn_missing = F), 
                    educ4_a2 = plyr::mapvalues(as.character(educ_a2), from = simple_educ$educ, to = simple_educ$educ4, warn_missing = F), 
                    educ4_mom =  plyr::mapvalues(as.character(educ_mom), from = simple_educ$educ, to = simple_educ$educ4, warn_missing = F), 
                    
                    educ6_max = plyr::mapvalues(as.character(educ_max), from = simple_educ$educ, to = simple_educ$educ6, warn_missing = F),
                    educ6_a1 = plyr::mapvalues(as.character(educ_a1), from = simple_educ$educ, to = simple_educ$educ6, warn_missing = F), 
                    educ6_a2 = plyr::mapvalues(as.character(educ_a2), from = simple_educ$educ, to = simple_educ$educ6, warn_missing = F), 
                    educ6_mom =  plyr::mapvalues(as.character(educ_mom), from = simple_educ$educ, to = simple_educ$educ6, warn_missing = F) 
      ) %>% 
      dplyr::select(pid, record_id, educ_max:educ6_mom) %>% 
      dplyr::mutate(across(where(is.character), as.factor))
    
    
    # relevel
    educ_df$educ_max = relevel( educ_df$educ_max, ref = simple_educ$educ[7]) #BA/BS as reference
    educ_df$educ_a1 = relevel( educ_df$educ_a1, ref = simple_educ$educ[7]) #BA/BS as reference
    educ_df$educ_a2 = relevel( educ_df$educ_a2, ref = simple_educ$educ[7]) #BA/BS as reference
    
    educ_df$educ4_max = relevel( educ_df$educ4_max, ref = simple_educ$educ4[7]) #College degree reference
    educ_df$educ4_a1 = relevel( educ_df$educ4_a1, ref = simple_educ$educ4[7]) #College degree reference
    educ_df$educ4_a2 = relevel( educ_df$educ4_a2, ref = simple_educ$educ4[7]) #College degree reference
    
    educ_df$educ6_max = relevel( educ_df$educ6_max, ref = simple_educ$educ6[7]) #College degree reference
    educ_df$educ6_a1 = relevel( educ_df$educ6_a1, ref = simple_educ$educ6[7]) #College degree reference
    educ_df$educ6_a2 = relevel( educ_df$educ6_a2, ref = simple_educ$educ6[7]) #College degree reference
    
    
    recodes_df = educ_df
      
  }
  
  if(what == "sex"){
    
    sex_df = dat %>% dplyr::select(pid, record_id, cqr009) %>% 
      dplyr::mutate(sex = plyr::mapvalues(cqr009, from =  value_labels(lex = "cqr009",dict = dict)$value, to=  value_labels(lex = "cqr009",dict = dict)$label, warn_missing = F), 
                    female = (sex == "Female")) %>% 
      dplyr::mutate(across(where(is.character), as.factor))
    
    sex_df$sex = relevel(sex_df$sex, ref = "Female")
    
    recodes_df = sex_df
  }
  
  return(recodes_df)
  
}


recode_it<-function(dat, dict, what = "all"){
  if(what=="all"){
    vars = c(init__("demographic recodes"))
  } else {
    vars = what
  }
  
  recoded_dat = dat
  for(v in vars){
    recoded_dat = recoded_dat %>% 
      dplyr::left_join(
        recode__(dat = dat, dict = dict, what = v), 
        by = c("pid", "record_id")
      )
  }
  
  return(recoded_dat)
  
}

