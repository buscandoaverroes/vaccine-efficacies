# graph-data.R
# generates teh coordinate data for plotting

library(RColorBrewer)
library(plotly)


# import data
vax_data <- readRDS(file.path(data, "vax_data.rda"))

# make key values ========================================================== 
# Pfizer 
Pfizer_plaCov = vax_data$placebo_covid_rate10k[vax_data$short_name %in% "Pfizer"]
Pfizer_plaSev = vax_data$placebo_severe_rate10k[vax_data$short_name %in% "Pfizer"]
Pfizer_plaMort= vax_data$placebo_mortality_rate10k[vax_data$short_name %in% "Pfizer"]
Pfizer_treCov = vax_data$treatment_covid_rate10k[vax_data$short_name %in% "Pfizer"]
Pfizer_treSev = vax_data$treatment_severe_rate10k[vax_data$short_name %in% "Pfizer"]
Pfizer_treMort= vax_data$treatment_mortality_rate10k[vax_data$short_name %in% "Pfizer"]

# Moderna
Moderna_plaCov = vax_data$placebo_covid_rate10k[vax_data$short_name %in% "Moderna"]
Moderna_plaSev = vax_data$placebo_severe_rate10k[vax_data$short_name %in% "Moderna"]
Moderna_plaMort= vax_data$placebo_covid_rate10k[vax_data$short_name %in% "Moderna"]
Moderna_treCov = vax_data$treatment_covid_rate10k[vax_data$short_name %in% "Moderna"]
Moderna_treSev = vax_data$treatment_severe_rate10k[vax_data$short_name %in% "Moderna"]
Moderna_treMort= vax_data$treatment_mortality_rate10k[vax_data$short_name %in% "Moderna"]




# generate data ============================================================

gen_sim_data <- function(name, max) {
  
  # store values
  f.plaCov = vax_data$placebo_covid_rate10k[vax_data$short_name %in% name]
  f.plaSev = vax_data$placebo_severe_rate10k[vax_data$short_name %in% name]
  f.plaMort= vax_data$placebo_mortality_rate10k[vax_data$short_name %in% name]
  f.treCov = vax_data$treatment_covid_rate10k[vax_data$short_name %in% name]
  f.treSev = vax_data$treatment_severe_rate10k[vax_data$short_name %in% name]
  f.treMort= vax_data$treatment_mortality_rate10k[vax_data$short_name %in% name]

  ## Coordinate data 
  sim_data <- 
    expand_grid(x = 1:max, y = 1:max)  # create all cominations of 100 ^2, or 10,000 obs

  sim_data <-
    sim_data %>%
    mutate(r = runif(length(sim_data$x), min = 0, max = max)) %>% # make 10,000 random obs
    arrange(r)
  
  
  ## covid data
  # the thinking here is to generate a binary variable that turns to "yes" at an equivalent
  # rate over the 10,000 observations that reflects the clinical trial data
  
  sim_data <- sim_data %>% 
    mutate(
      placebo_outcomes = "COVID Negative",
      treatment_outcomes = "COVID Negative"
    )
  
  ## replace values -------------------------
  # replace for covid Positive
  sim_data$placebo_outcomes[1:f.plaCov] <- "COVID Positive" 
  sim_data$treatment_outcomes[1:f.treCov] <- "COVID Positive"
  
  # replace for covid Severe
  if (f.plaSev >0) {
    sim_data$placebo_outcomes[(f.plaCov+1):((f.plaCov+1)+(f.plaSev-1))] <- "Severe COVID" # avoid adding an extra datum
  }
  if (f.treSev >0) {
    sim_data$treatment_outcomes[(f.treCov+1):((f.treCov+1)+(f.treSev-1))] <- "Severe COVID"
  }
  
  
  # replace for mortality only if the stat is non-missing
  if (!is.na(f.plaMort) ) {
    sim_data$placebo_outcomes[1001:f.plaSev] <- "COVID Death" # start from row 1001, assuming that totals above won't add up to 1000
  } 
  if (!is.na(f.treMort) ) {
    sim_data$treatment_outcomes[1001:f.treSev] <- "COVID Death"
  } 
  
  # pivot
  sim_data <- sim_data %>%
    janitor::remove_empty("cols") %>% 
    pivot_longer(cols = c(ends_with("outcomes")),
                 names_to = "group", values_to = "outcome") %>%
    mutate(
      vax_name = name,
      arm = as_factor(case_when(
      group == "placebo_outcomes" ~ "Placebo", 
      TRUE ~ "Treatment"
    ))) %>%
    select(-group)
  
  
  sim_data
}


# Generate simulated clinical data ===========================================================
pfizer_sim_data  <- gen_sim_data("Pfizer", 100)
moderna_sim_data <- gen_sim_data("Moderna", 100)


sim_data <- rbind(pfizer_sim_data, moderna_sim_data)

# Generate plotly objects  ===========================================================

# to be deteremined...will have to write a function
# 
# 

save(
  vax_data, 
  sim_data,
  Pfizer_plaCov,Pfizer_plaSev,Pfizer_plaMort,Pfizer_treCov,Pfizer_treSev,Pfizer_treMort,
  Moderna_plaCov,Moderna_plaSev,Moderna_plaMort,Moderna_treCov, Moderna_treSev,Moderna_treMort,
  file = file.path(data, "app-data.Rdata")
)

### save a copy to the app directory
save(
  vax_data, 
  sim_data,
  Pfizer_plaCov,Pfizer_plaSev,Pfizer_plaMort,Pfizer_treCov,Pfizer_treSev,Pfizer_treMort,
  Moderna_plaCov,Moderna_plaSev,Moderna_plaMort,Moderna_treCov, Moderna_treSev,Moderna_treMort,
  file = file.path(app, "app-data.Rdata")
)
