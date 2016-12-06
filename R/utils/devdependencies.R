#  Name: install_dependencies 
#  Objective: Utility to help install some packages/libraries from
# Cran or Github necessary for this project

install_dependencies <- function(){
  print("Installing Project dependencies/libraries... Please wait")
  # Insert dependencies here
  list.of.packages <- c("GGally", "corrplot", "caret", "ISLR", "nnet")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org", dependencies = TRUE)
  
  print("Status: Success")
}