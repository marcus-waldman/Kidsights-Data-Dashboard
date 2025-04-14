#utils-server.R

# Retention after eligbility and vetting
make_retention_table<-function(elig_list){
  outtab = elig_list$summary %>%
    dplyr::mutate(`1. Total Records` = T, `2. After Eligibility Screening` = (eligibility == "Pass"), `3. After Authenticity Screening` = (eligibility == "Pass" & authenticity == "Pass")) %>% 
    dplyr::select(-(eligibility:compensation)) %>% 
    tidyr::pivot_longer(`1. Total Records`:`3. After Authenticity Screening`) %>% 
    dplyr::filter(value) %>% 
    dplyr::group_by(name) %>% 
    dplyr::reframe(n = sum(value)) %>% 
    dplyr::mutate(`Retention %` = paste0(round(100*n/max(n))) ) %>% 
    dplyr::ungroup() 
  return(outtab)
}

# Plots by education

make_sample_sizes_barcharts<-function(df, var = "education"){
  
  

  df = df %>% dplyr::mutate(years_old = paste0(floor(age_in_days/365.25), " years old"))
  
  if(var == "education"){
    outplot = ggplot(df, aes(x = educ4_max, col = sex, fill = sex))   
  }

  outplot = outplot +
    geom_histogram(stat = "count") +
    facet_wrap(years_old~.) +
    coord_flip() +
    ggthemes::theme_few() +
    ggthemes::scale_colour_tableau() +
    ggthemes::scale_fill_tableau() +
    geom_hline(yintercept=100, linetype = 2, col = "purple") +
    labs(x = var)

  return(outplot)
}

var2lex<-function(var){
  map = data.frame(var = c("education", "race/ethnicity"), 
                   lex_ne25 = c("educ4_max", "raceG")
  )
  
  return(map$lex_ne25[map$var == var])
}
