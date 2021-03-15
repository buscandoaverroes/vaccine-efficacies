# main.R
# calls all necessary scripts, sets file paths, etc

library(tidyverse)
library(readr)




        # Users # 

    user <- 1 

if (user == 1) {
  root     <- "/Volumes/projects/vaccines/vaccine-efficacies" # copy path to repo folder here.
    code   <- file.path(root, "vaccine-efficacies")
    data   <- file.path(root, "data")
}




# scripts 

s1  <- 1 # imports data 


if (s1==1) {source(file.path(code, "clinical-data.R"))}