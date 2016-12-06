#  Name: install_dependencies 
#  Objective: Utility to help install some packages/libraries from
# Cran or Github necessary for this project

install_dependencies <- function(){
  print("Installing Project dependencies/libraries... Please wait")
  # Insert dependencies here
  list.of.packages <- c("GGally", "corrplot")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org", dependencies = TRUE)
  
  # Helpers
  install.packages("GGally", repos = "http://cran.us.r-project.org")
  install.packages("corrplot", repos = "http://cran.us.r-project.org")
  install.packages("nloptr", repos = "http://cran.us.r-project.org")
  install.packages("caret", repos = "http://cran.us.r-project.org")
  install.packages("ISLR", repos = "http://cran.us.r-project.org")
  
  # Models
  install.packages("nnet", repos = "http://cran.us.r-project.org")
  
  print("Status: Success")
}