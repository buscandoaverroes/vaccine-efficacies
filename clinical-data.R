# clinical-data.R
# imports the clinical data csv

vax_data <- read_csv(file.path(data, "clinical-data.csv"), col_names = TRUE, na = "")