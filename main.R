# main.R
# calls all necessary scripts, sets file paths, etc

library(tidyverse)
library(readr)
library(rmarkdown)
library(knitr)




        # Users # 

    user <- 1 

if (user == 1) {
  root     <- "/Volumes/projects/vaccines" # copy path to repo folder here.
    data   <- file.path(root, "data")
    code   <- file.path(root, "vaccine-efficacies")
      app  <- file.path(code, "app/vaccines-app")
    
}




# scripts 

s1  <- 0 # imports, calculates data
s2  <- 0 # creates graph/app data
s3  <- 0 # runs infection-data
s4  <- 0 # map-sandbox

m1  <- 0 # about markdown

if (s1==1) {source(file.path(code, "clinical-data.R"))}
if (s2==1) {source(file.path(code, "graph-data.R"))}
if (s3==1) {source(file.path(code, "infection-data.R"))}
if (s4==1) {source(file.path(code, "map-sandbox.R"))}