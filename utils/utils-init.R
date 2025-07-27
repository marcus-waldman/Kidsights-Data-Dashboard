init__<-function(what){
  
  if(what == "respondent eligibility"){
    df = tibble(
      category = c(rep("Compensation",1), rep("Eligibility",4), rep("Authenticity",3), rep("Compensation",1)),
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
          "At least 10 responses other than `Don't Know` to KMT and z-score within 5SD",
          "Did not complete all modules of survey"
        )
      ) %>% 
      dplyr::mutate(pass = NA, notes = "")
      return(df)
    
  }
  
  if(what == "demographic recodes"){
    vars = c("race", "caregiver relationship", "education", "sex", "age","income")
    return(vars)
  }
  
  
  if(what == "category descriptions"){
    descriptions <- list(
      "include" = "Meets all criteria for inclusion in the study",
      "race" = "Race and ethnicity variables for children and primary caregivers, including combined race/ethnicity categories",
      "caregiver relationship" = "Variables describing the relationship between caregivers and children, including gender and maternal status indicators", 
      "education" = "Education level variables for caregivers in multiple category systems (4, 6, and 8 categories), including maximum household education and maternal education",
      "sex" = "Child's biological sex and gender indicator variables",
      "age" = "Child an primary caregiver age",
      "income" = "Household income variables including CPI-adjusted values, family size, federal poverty level calculations and categories",
      "survey completion" = "Variables tracking survey completion and attrition patterns across RedCAP projects and study modules"
    )
    return(descriptions)
  }
  
 
  
  return(NULL)

}
