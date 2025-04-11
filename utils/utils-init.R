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
    vars = c("race", "caregiver relationship", "education")
    return(vars)
  }
  
  
  return(NULL)

}
