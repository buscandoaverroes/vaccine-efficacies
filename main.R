# main.R
# calls all necessary scripts, sets file paths, etc

library(tidyverse)
library(readr)




        # Users # 

    user <- 1 

if (user == 1) {
  root     <- "/Volumes/projects/vaccines" # copy path to repo folder here.
    data   <- file.path(root, "data")
    code   <- file.path(root, "vaccine-efficacies")
      app  <- file.path(code, "app/vaccines-app")
    
}




# scripts 

s1  <- 1 # imports, calculates data
s2  <- 1 # creates graph/app data

if (s1==1) {source(file.path(code, "clinical-data.R"))}
if (s2==1) {source(file.path(code, "graph-data.R"))}