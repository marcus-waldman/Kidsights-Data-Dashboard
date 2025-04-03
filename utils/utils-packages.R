### R packages to successfully 

cran_packages<-function(){
  R_utils<-c("remotes","stringr","tidyverse")
  Shiny_pkgs<- c("shiny")
  return(c(R_utils,Shiny_pkgs))
}
github_packages<-function(){
  marcus_waldman<-c("marcus-waldman/KidsightsPublic")
  return(c(marcus_waldman))
}
clean_github<-function(pkg){
  out = stringr::str_remove_all(pkg,"marcus-waldman/")
  return(out)
}
not_installed<-function(pkg){return(!(pkg %in% installed.packages()))}
is_installed<-function(pkg){return(pkg %in% installed.packages())}
install_if<-function(pkgs, github = F){
  all_mirrors = getCRANmirrors(local.only = T)
  iowa_mirror = all_mirrors[all_mirrors$City == "Ames", ]
  setRepositories(addURLs = c(Iowa = iowa_mirror$URL), name = "CRAN")
  for(i in 1:length(pkgs)){
    if(not_installed(pkgs[i])){ifelse(github,remotes::install_github(pkgs[i], upgrade=F, quiet = T), install.packages(pkgs[i]))}
  }
}

install_if(cran_packages(), github=F)
install_if(github_packages(), github = T)
