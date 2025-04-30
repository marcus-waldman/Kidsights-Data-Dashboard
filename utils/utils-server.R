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

make_sample_sizes_barcharts<-function(df, var){
  
  df = df %>% dplyr::mutate(years_old = paste0(floor(age_in_days/365.25), " years old"))
  
 if(var == "education"){df$v = df$educ4_max}
 if(var == "race"){df$v = df$raceG}
  
  gg = ggplot() + 
    theme_Kidsights(base_font_size = 16, legend.pos = "top") + 
    scale_fill_Kidsights_qualitative() + 
    scale_color_Kidsights_qualitative() +
    labs(x = NULL, y = NULL, title = NULL) +
    geom_histogram(data = df, aes(x=v, col = sex), fill = "white", linewidth = 1.5, stat = "count") +
    scale_x_discrete(drop = FALSE) +
    facet_grid(years_old~.) +
    coord_flip() +
    geom_hline(yintercept=100, linetype = 2)

  
  #return(girafe(ggobj = gg))
  return(gg)
}

var2lex<-function(var){
  map = data.frame(var = c("education", "race/ethnicity"), 
                   lex_ne25 = c("educ4_max", "raceG")
  )
  
  return(map$lex_ne25[map$var == var])
}

make_geography_plot<-function(df, years_keep=c(0:5)){
  
  library(tigris)
  library(sf)
  library(ggplot2)#  Nebraska county boundaries
  #ne_counties <- counties("NE", cb = TRUE, class = "sf")  # Simplified county boundaries
  

  
  points_df = df %>%
    dplyr::mutate(years = floor(age_in_days/365.25)) %>% 
    dplyr::filter(years %in% years_keep) %>% 
    dplyr::mutate(zip = as.character(sq001)) %>% 
    dplyr::select(zip) %>% dplyr::left_join(zcta %>% dplyr::select(zip, INTPTLAT10:INTPTLON10), by = "zip") %>% 
    dplyr::rename(latitude = INTPTLAT10, longitude = INTPTLON10) %>% 
    dplyr::select(zip,latitude,longitude) %>% 
    dplyr::mutate(across(latitude:longitude, as.numeric))#%>% 
  #st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  
  # Plot Nebraska with overlayed points
  gg = ggplot() +
    geom_sf(data = ne_counties, fill = "#0b3474", color = "#999999") +  # Nebraska county map
    geom_jitter(data = points_df, aes(x=longitude, y = latitude), color = "#FFFFFF", size = 2, width = .025, height = .025, alpha = 1/3) +           # Overlay points
    theme_void()
  
  return(gg)
}

mobins2yrs<-function(bins){
  
  return(plyr::mapvalues(bins, from = c("0-11 mo.", "12-23 mo.", "24-35 mo.", "36-47 mo.", "48-59 mo.", "60-71 mo."), to = 0:5))
  
}
