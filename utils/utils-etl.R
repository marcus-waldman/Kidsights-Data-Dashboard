value_labels<-function(lex, dict,varname = "lex_ne25"){
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
      dplyr::mutate(across(where(is.character), as.factor))
    
    #Set baseline categories
    raceth_df$hisp = relevel(raceth_df$hisp, ref = "non-Hisp.")
    raceth_df$race = relevel(raceth_df$race, ref = "White")
    raceth_df$raceG = relevel(raceth_df$raceG, ref = "White, non-Hisp.")
    
    recodes_df = raceth_df
  }
  
  if(what %in% c("education")){
    stop("function not yet operational.")
  }
  
  if(is.null(recodes_df)){
    return(dat)
  } else {
    return(dat %>% dplyr::left_join(recodes_df, by = c("pid", "record_id")))
  }
  
}


recode_it<-function(dat, dict, vars = "all"){
  if(vars=="all"){
    vars = c(init__("demographic recodes"))
  } else if(vars == "demographics"){
    vars = init__("demographic recodes")
  } else {
    stop(paste0("Specified vars argument (", vars, ") not recognized."))
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

