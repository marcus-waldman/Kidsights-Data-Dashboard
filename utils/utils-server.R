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
 if(var == "fpl"){df$v = df$fplcat}
  
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
  map = data.frame(var = c("education", "race/ethnicity", "household income", "federal poverty line"), 
                   lex_ne25 = c("educ4_max", "raceG", "income", "fpl")
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

make_crosstab_table<-function(df, var1, var2, years_filter = NULL){
  
  # Create mapping for variable names to display names
  var_mapping = c(
    "raceG" = "Race/Ethnicity",
    "fplcat" = "Federal Poverty Level", 
    "educ4_max" = "Education"
  )
  
  # Get display names
  var1_label = var_mapping[var1]
  var2_label = var_mapping[var2]
  
  # Filter by years_old if specified
  if (!is.null(years_filter) && length(years_filter) > 0) {
    df = df %>%
      dplyr::mutate(years_old = floor(age_in_days/365.25)) %>%
      dplyr::filter(years_old %in% years_filter)
  }
  
  # Create crosstab with margins
  crosstab = df %>%
    dplyr::select(all_of(c(var1, var2))) %>%
    na.omit() %>%
    dplyr::count(.data[[var1]], .data[[var2]], name = "n") %>%
    tidyr::pivot_wider(names_from = all_of(var2), values_from = n, values_fill = 0)
  
  # Add row totals
  crosstab = crosstab %>%
    dplyr::mutate(Total = rowSums(dplyr::select(., -1), na.rm = TRUE))
  
  # Add column totals
  col_totals = crosstab %>%
    dplyr::summarise(
      across(-1, sum, na.rm = TRUE)
    ) %>%
    dplyr::mutate(!!var1 := "Total", .before = 1)
  
  # Combine main table with totals
  result = dplyr::bind_rows(crosstab, col_totals)
  
  # Rename first column to display name
  names(result)[1] = var1_label
  
  return(result)
}

make_age_distribution_plotly <- function(df, fplcat_filter = NULL, raceG_filter = NULL, educ4_max_filter = NULL) {
  library(plotly)
  
  # Apply filter_include_exclude to only include eligible respondents
  df_filtered = filter_include_exclude(df)
  
  # Apply demographic filters if provided and not empty
  if (!is.null(fplcat_filter) && length(fplcat_filter) > 0) {
    df_filtered = df_filtered %>%
      dplyr::filter(fplcat %in% fplcat_filter)
  }
  
  if (!is.null(raceG_filter) && length(raceG_filter) > 0) {
    df_filtered = df_filtered %>%
      dplyr::filter(raceG %in% raceG_filter)
  }
  
  if (!is.null(educ4_max_filter) && length(educ4_max_filter) > 0) {
    df_filtered = df_filtered %>%
      dplyr::filter(educ4_max %in% educ4_max_filter)
  }
  
  # Create age bins (0-5 years) by sex
  age_sex_data = df_filtered %>%
    dplyr::mutate(age_years = floor(age_in_days/365.25)) %>%
    dplyr::filter(age_years >= 0 & age_years <= 5) %>%
    dplyr::count(age_years, sex, name = "count") %>%
    dplyr::group_by(age_years) %>%
    dplyr::mutate(
      total = sum(count),
      proportion = ifelse(total > 0, round(100 * count / total, 1), 0),
      prop_text = paste0(proportion, "%")
    ) %>%
    dplyr::ungroup()
  
  # Ensure all age bins and sex combinations are represented
  all_combinations = expand.grid(
    age_years = 0:5,
    sex = c("Male", "Female"),
    stringsAsFactors = FALSE
  )
  
  age_sex_data = all_combinations %>%
    dplyr::left_join(age_sex_data, by = c("age_years", "sex")) %>%
    dplyr::mutate(
      count = ifelse(is.na(count), 0, count),
      total = ifelse(is.na(total), 0, total),
      proportion = ifelse(is.na(proportion), 0, proportion),
      prop_text = ifelse(is.na(prop_text), "0%", prop_text)
    ) %>%
    dplyr::group_by(age_years) %>%
    dplyr::mutate(total = max(total, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # Create age labels
  age_sex_data = age_sex_data %>%
    dplyr::mutate(age_label = paste0(age_years, " years old"))
  
  # Get Kidsights colors - use first two colors for Male/Female
  kidsights_colors = color_values_Kidsights_qualitative()
  sex_colors = c("Female" = kidsights_colors[1], "Male" = kidsights_colors[2])
  
  # Create separate traces for each sex
  p = plot_ly()
  
  for(sex_val in c("Female", "Male")) {
    sex_data = age_sex_data %>% dplyr::filter(sex == sex_val)
    
    p = p %>% add_trace(
      data = sex_data,
      x = ~age_label,
      y = ~count,
      type = "bar",
      name = sex_val,
      marker = list(
        color = sex_colors[sex_val],
        line = list(color = kidsights_colors[3], width = 1)
      ),
      text = ~ifelse(count > 0, prop_text, ""),
      textposition = "inside",
      textfont = list(color = "white", size = 11, family = "Century Gothic, Arial, sans-serif"),
      hovertemplate = paste0("<b>%{x}</b><br>", sex_val, ": %{y}<br>Proportion: %{text}<extra></extra>")
    )
  }
  
  # Calculate total sample sizes for display above bars
  total_data = age_sex_data %>%
    dplyr::group_by(age_years, age_label) %>%
    dplyr::summarise(total_sample = sum(count), .groups = "drop")
  
  # Add total sample size text above bars
  p = p %>% add_annotations(
    data = total_data,
    x = ~age_label,
    y = ~total_sample,
    text = ~ifelse(total_sample > 0, as.character(total_sample), ""),
    showarrow = FALSE,
    yshift = 10,
    font = list(
      family = "Century Gothic, Arial, sans-serif",
      size = 12,
      color = "#0b3474"
    )
  ) %>%
  
  layout(
    title = list(
      text = "Sample Sizes by Age and Sex (Eligible Respondents)",
      font = list(
        family = "Century Gothic, Arial, sans-serif",
        size = 16,
        color = "#0b3474"
      )
    ),
    xaxis = list(
      title = "",
      tickfont = list(
        family = "Century Gothic, Arial, sans-serif",
        size = 12
      ),
      categoryorder = "array",
      categoryarray = paste0(0:5, " years old")
    ),
    yaxis = list(
      title = "Sample Size",
      titlefont = list(
        family = "Century Gothic, Arial, sans-serif",
        size = 14
      ),
      tickfont = list(
        family = "Century Gothic, Arial, sans-serif",
        size = 12
      ),
      gridcolor = "#D2D2D2",
      gridwidth = 0.5
    ),
    barmode = "stack",
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hoverlabel = list(
      bgcolor = "#0b3474",
      font = list(
        family = "Century Gothic, Arial, sans-serif",
        size = 12,
        color = "white"
      )
    ),
    legend = list(
      orientation = "h",
      x = 0.5,
      xanchor = "center",
      y = -0.1,
      font = list(
        family = "Century Gothic, Arial, sans-serif",
        size = 12
      )
    ),
    margin = list(t = 60, b = 80, l = 60, r = 40)
  )
  
  return(p)
}
