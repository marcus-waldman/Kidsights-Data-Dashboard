#utils-eligibility.R


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
  cid7 = dat %>% 
    dplyr::select(pid,record_id,date_complete_check) %>% 
    dplyr::mutate(pass_cid7 = (date_complete_check==1) & !is.na(date_complete_check)) %>% 
    dplyr::relocate(pid,record_id,pass_cid7)
  
  return(cid7)
  
}

passes_cid8 = function(dat){
  #At least 10 net responses other (don't know doesn't count) and Kidsight z-score within 5SD"
  
  
  codebook = KidsightsPublic::internals$codebook
  KMT = KidsightsPublic::internals$forms$apr2025
  #View(codebook)
  
  foo = dat %>% 
    dplyr::rename(days = age_in_days) %>%  
    dplyr::select(pid,record_id, days, dplyr::any_of(KMT$lex_ne25)) %>% 
    dplyr::mutate(years = days/365.25, months = floor(12*(days/365.25))) %>% 
    dplyr::relocate(pid, record_id, years, months, days) %>% 
    tidyr::pivot_longer(-c(1:5), names_to = "lex_ne25") %>% 
    na.omit() %>% 
    dplyr::left_join(KMT, by = "lex_ne25") %>% 
    dplyr::arrange(pid,record_id, item_order) %>% 
    dplyr::relocate(pid:days,item_order,lex_ne25,stem,value,num_code)
    
  response_counts = foo %>% dplyr::group_by(pid,record_id) %>% dplyr::reframe(months=months[1], `Total Responses` = length(value),`No/Never`=sum(value==0), `Yes/Sometimes+`=sum(value>0 & value<9), `Don't Know` = sum(value==9))
  
  response_counts = response_counts %>% dplyr::mutate(`Net Responses` = `Total Responses` - `Don't Know`)
  
  ##
  tmp = dat %>% dplyr::rename(days = age_in_days) %>%  dplyr::select(pid,record_id, days, dplyr::any_of(codebook$lex_ne25))
  
  long = tmp %>% tidyr::pivot_longer(4:ncol(tmp), names_to = "lex_ne25") %>% na.omit() %>% 
    dplyr::left_join(KMT, by = "lex_ne25") %>% 
    dplyr::mutate(years = days/365.25, months = floor(12*(days/365.25))) %>% 
    dplyr::relocate(pid, record_id, years, months, days, item_order,lex_ne25,value)
  
  
  wide = long %>% 
    dplyr::filter(value!=9) %>%  #remove any Don't know responses
    dplyr::left_join(codebook %>% dplyr::select(lex_ne25,lex_kidsight), by = "lex_ne25") %>% 
    dplyr::select(pid:record_id, years, lex_kidsight, value) %>% 
    tidyr::pivot_wider(names_from="lex_kidsight", values_from="value")
  
  
  
  #expanded
  input = KidsightsPublic::calibdat %>% dplyr::mutate(wgt = 1) %>% 
          dplyr::bind_rows(wide %>% dplyr::mutate(wgt = sqrt(.Machine$double.eps))) %>% 
          dplyr::relocate(id,pid,record_id,years,wgt) %>% 
          dplyr::mutate(wgt = wgt/mean(wgt))

  #scores = KidsightsPublic::fscores(input = expanded, est_hyperpriors = F)
  

  fit_kidsights2 <-
    mirt::mirt(
      data = input %>% dplyr::select(starts_with("AA"),starts_with("BB"),starts_with("CC"),starts_with("DD")),
      model = 1,
      itemtype = "Rasch",
      technical = list(theta_lim = c(-6, 12), NCYCLES = 10), 
      quadpts = 2*61, 
      large = F, 
      tol = 1E-2,
      dentype = "EH",
      survey.weights= input$wgt, 
      pars = readr::read_rds("data/pars_Rasch.rds"), 
      optimizer = "NR", 
      accelerate = "squarem"
      )
  
  #write_rds(mirt::mod2values(fit_kidsights), file = "C:/Users/waldmanm/git-repositories/Kidsights-Data-Dashboard/data/pars_Rasch.rds")
  
  
  input = input %>% dplyr::mutate( theta =  mirt::fscores(fit_kidsights2, theta_lim = c(-6,12)) %>% as.numeric() ) %>% 
    dplyr::mutate(sample = ifelse(!is.na(id), "Calib. 2020-24", "Nebraska 2025") %>% as.factor())
  
  
  #ggplot(input, aes(x = years, y = theta))  + geom_point()
  
  loss<-function(delta, dat){
    fit_lm = lm(theta~log(years + delta), data = dat)
    return(as.integer(-2*logLik(fit_lm)))
  }
  
  delta = optim(1, fn = loss, dat = input %>% dplyr::filter(sample=="Calib. 2020-24"), method = "Brent", lower = .01, upper = 10)$par
  
  # input_gamlss = input %>% dplyr::filter(sample == "Calib. 2020-24") %>%  dplyr::select(theta,years) %>% dplyr::mutate(delta = delta)
  # fit_gamlss = gamlss(
  #   theta~log(years + delta),
  #   sigma.formula = ~ pbm(years, mono = "up"),
  #   nu.formula = ~pbm(years, mono = "down"),
  #   data = input_gamlss,
  #   family = gamlss.dist::ST1(),
  #   control = gamlss.control(n.cyc = 20)
  #   #start.from = readr::read_rds(file = "data/fit_gamlss.rds"),
  # )
  # readr::write_rds(input_gamlss, file = "data/input_gamlss.rds")
  # readr::write_rds(fit_gamlss, file = "data/fit_gamlss.rds")
  # 

  input_newdata = input %>% dplyr::mutate(delta = delta) %>% dplyr::select(theta,years, delta)
  yhat_gamlss = gamlss::predictAll( readr::read_rds("data/fit_gamlss.rds"), 
                                    data = readr::read_rds("data/input_gamlss.rds"), 
                                    newdata = input_newdata)

  
  input = input %>% dplyr::mutate(mu = yhat_gamlss$mu, sigma = yhat_gamlss$sigma, nu = yhat_gamlss$nu)
  
  #ggplot(input, aes(x=years, y = nu)) + geom_point()
  
  input = input %>% dplyr::mutate(qtile = gamlss.dist::pST1(q = theta, mu = mu, sigma = sigma, nu = nu))
 
  input = input %>% dplyr::mutate(zscore = qnorm(qtile))
  
  
  response_counts = response_counts %>% dplyr::left_join(input %>% dplyr::filter(sample != "Calib. 2020-24") %>% dplyr::select(pid,record_id, zscore), by = c("pid", "record_id"))
  
  response_counts = response_counts %>% dplyr::relocate(pid,record_id,months,zscore)
  
  df = dat %>% 
    dplyr::select(pid, record_id) %>% 
    dplyr::left_join(response_counts %>% dplyr::select(pid, record_id, `Net Responses`, zscore), by = c("pid", "record_id")) %>%  
    dplyr::mutate(net_responses = ifelse(is.na(`Net Responses`),0,`Net Responses`)) %>% 
    dplyr::mutate(pass_cid8 = (net_responses>=10 & abs(zscore)<5)) %>% 
    dplyr::select(pid,record_id,pass_cid8, net_responses,zscore)
  
  
  return(df)
}

passes_cid9 = function(dat){
  #"Did not complete all modules of survey"
  
  tmp = dat %>% dplyr::select(pid, record_id, dplyr::starts_with("module_"))
  
  # convert to long format
  tmp = tmp %>% tidyr::pivot_longer(module_2_family_information_complete:module_8_followup_information_complete) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(module = as.integer(stringr::str_split_1(name,"_")[2])) %>% 
    dplyr::group_by(pid,record_id,module) %>% 
    dplyr::reframe(value = max(value)) 
  
  # Get rid of follow up information
  tmp = tmp %>% dplyr::filter(module!=8)
  
  # convert back to wide format
  tmp = tmp %>% 
    dplyr::mutate(
      module = paste("module",module,sep = "_"), 
      value = as.integer(value==2)
    ) %>% 
    tidyr::pivot_wider(names_from = module, values_from = value) 
  
  df = tmp %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(pass_cid9 = ( mean(c_across(module_2:module_9)) == 1) ) %>% 
    dplyr::relocate(pid,record_id, pass_cid9) %>% 
    dplyr::rename_at(vars(module_2:module_9), function(x){paste0(x,"_complete")})
  
  return(df)
  
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

check_eligibility_authenticity<-function(dat,dict){
  wide = dat %>% dplyr::select(pid,record_id) %>% 
    dplyr::left_join(passes_cid1(dat) %>% dplyr::select(pid,record_id,pass_cid1), by = c("pid","record_id")) %>% 
    dplyr::left_join(passes_cid2(dat) %>% dplyr::select(pid,record_id,pass_cid2), by = c("pid","record_id")) %>% 
    dplyr::left_join(passes_cid3(dat) %>% dplyr::select(pid,record_id,pass_cid3), by = c("pid","record_id")) %>% 
    dplyr::left_join(passes_cid4(dat) %>% dplyr::select(pid,record_id,pass_cid4), by = c("pid","record_id")) %>% 
    dplyr::left_join(passes_cid5(dat) %>% dplyr::select(pid,record_id,pass_cid5), by = c("pid","record_id")) %>% 
    dplyr::left_join(passes_cid6(dat, dict = dict) %>% dplyr::select(pid,record_id,pass_cid6), by = c("pid","record_id")) %>% 
    dplyr::left_join(passes_cid7(dat) %>% dplyr::select(pid,record_id,pass_cid7), by = c("pid","record_id")) %>% 
    dplyr::left_join(passes_cid8(dat) %>% dplyr::select(pid,record_id,pass_cid8), by = c("pid","record_id")) %>%
    dplyr::left_join(passes_cid9(dat) %>% dplyr::select(pid,record_id,pass_cid9), by = c("pid","record_id")) 
  
  long = tidyr::pivot_longer(wide, pass_cid1:pass_cid9, names_to = "cid", values_to = "pass") %>% 
    dplyr::mutate(cid = stringr::str_remove_all(cid,"pass_cid") %>% as.integer()) %>% 
    dplyr::left_join(init__(what = "respondent eligibility") %>% dplyr::select(cid,category,action, description), by = "cid")
  
  summary = long %>% dplyr::group_by(pid,record_id, category) %>% dplyr::reframe(status = ifelse(!(FALSE %in% pass), "Pass", "Fail")) %>% 
    tidyr::pivot_wider(names_from = "category", values_from = "status") %>% 
    dplyr::relocate(pid,record_id,Eligibility,Authenticity,Compensation) %>% 
    dplyr::rename_all(tolower)
  
  
  return(list(summary = summary, details = long))
  
  
}





