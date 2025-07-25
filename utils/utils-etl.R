

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
  }) %>% dplyr::bind_rows()
  

  
  
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

recode__<-function(dat, dict, what = NULL, relevel_it = T){
  
  recodes_df = NULL
  
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


    recodes_df = relate_df
    
  }
  
  if(what %in% c("education")){
    #educ_df = dat %>% 
    
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
    
    educ_df = dat %>% 
      dplyr::select(-dplyr::any_of(c("relation1","relation2","mom_a1"))) %>% 
      recode_it(dict=dict, what = "caregiver relationship") %>% 
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
    
    recodes_df = educ_df
      
  }
  
  if(what == "sex"){
    
    sex_df = dat %>% dplyr::select(pid, record_id, cqr009) %>% 
      dplyr::mutate(sex = plyr::mapvalues(cqr009, from =  value_labels(lex = "cqr009",dict = dict)$value, to=  value_labels(lex = "cqr009",dict = dict)$label, warn_missing = F), 
                    female = (sex == "Female")) %>% 
      dplyr::mutate(across(where(is.character), as.factor))
    
    if(relevel_it){sex_df$sex = relevel(sex_df$sex, ref = "Female")}  
    
    recodes_df = sex_df
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
    
    recodes_df = income_df %>% dplyr::select(pid, record_id, income,cpi99:fplcat)
    

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
  if (any(is.na(year_vec))) stop("Invalid dates supplied.")
  

  
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

