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
