#utils.R

init_elig_df<-function(){
  df = data.frame(
    category = c(rep("Compensation",2), rep("Eligibility",4), rep("Authenticity",3)),
    action = c(rep("Exclusion",2),rep("Inclusion",3), rep("Exclusion",4)) 
  ) %>% 
    dplyr::mutate(cid = 1:n(), pid = NA, mrwid = NA) %>% dplyr::relocate(pid,mrwid,cid) %>% 
    dplyr::mutate(
      description = c(
        "Failed to acknowledge terms and conditions",
        "Failed to provide informed consent",
        "Respondent is 19 years of older",
        "Child is 2191 days or younger",
        "Nebraska resident as indicated by Nebraska zipcode",
        "Zip code not match for county or surrounding county",
        "Child's birthday failed to be confirmed",
        "Kidsight z-score within 5SD",
        "Did not complete all modules of survey"
      )
    ) %>% 
    dplyr::mutate(pass = NA, notes = "")
  return(df)
}