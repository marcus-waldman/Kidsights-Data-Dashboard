# utils-packages
# Check or download necessary packages


cran_packages<-function(){
  R_utils<-c("plyr","readxl","remotes","stringr","tidyverse", "tools")
  Shiny_pkgs<- c("shiny")
  REDCap_pkgs<-c("httr", "REDCapR")
  Geo_pkgs<-c("zipcodeR")
  regress_pkgs<-c("gamlss")
  plot_pkgs <- c("ggthemes")
  return(c(R_utils,Shiny_pkgs, REDCap_pkgs, Geo_pkgs, regress_pkgs))
}
github_packages<-function(){
  marcus_waldman<-c("marcus-waldman/KidsightsPublic")
 # vubiostat<-c("vubiostat/redcapAPI")
  return(c(marcus_waldman))
}
clean_github<-function(pkg){
  out = stringr::str_remove_all(pkg,"marcus-waldman/", "vubiostat/")
  return(out)
}
not_installed<-function(pkg){return(!(pkg %in% installed.packages()))}
is_installed<-function(pkg){return(pkg %in% installed.packages())}
install_if<-function(pkgs, github = F){
  all_mirrors = getCRANmirrors(local.only = T)
  iowa_mirror = all_mirrors[all_mirrors$City == "Ames", ]
  setRepositories(addURLs = c(Iowa = iowa_mirror$URL), name = "CRAN")
  for(i in 1:length(pkgs)){
    if(not_installed(pkgs[i])){ifelse(github,remotes::install_github(pkgs[i], upgrade=T, quiet = T), install.packages(pkgs[i]))}
  }
}
check_packages_installed<-function(){
  install_if(cran_packages(), github=F)
  install_if(github_packages(), github = T)
  message("\n Success. All necessary packages are installed :) \n")
}

