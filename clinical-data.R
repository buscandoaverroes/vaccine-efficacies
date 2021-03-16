# clinical-data.R
# imports the clinical data, calculates rates

# the published summaries and clinical data present raw numbers of total participants in each of the placebo and 
# treatment groups, along with pre- and post- outcomes for various indicators such as infections. From this data, 
# it should be possible to confirm the stated efficacy rates.



# import the data -----------------------------------------------------------------------------

vax_data_import <- read_csv(file.path(data, "clinical-data.csv"), col_names = TRUE, na = "") 

## subset
drop <- c("covid_efficacy", "severe_efficacy", "mortality_efficacy", "placebo_covid_rate",
 "treatment_covid_rate", "placebo_hospital_rate", "treatment_hospital_rate",
 "placebo_mortality_rate", "treatment_mortality_rate")


vax_data <- vax_data_import %>%
  select(-drop)



# calculations ================================================================================

## rates: this is simply a ratio of people within each treatment arm that experienced a condition ----
# (ie, tested positive for covid19). In fraction terms, the number of people who 
# experienced the condition / the total number of people in that treatment arm.
vax_data <- vax_data %>%
  mutate(
    placebo_covid_rate       = (placebo_n_covid_pos / placebo_n_participants),
    treatment_covid_rate     = (treatment_n_covid_pos / treatment_n_participants),
    placebo_severe_rate      = (placebo_n_severe / placebo_n_participants),
    treatment_severe_rate    = (treatment_n_severe / treatment_n_participants), 
    placebo_mortality_rate   = (placebo_n_mortality / placebo_n_participants),
    treatment_mortality_rate = (treatment_n_mortality / treatment_n_participants)
  )


## efficacy: here [x variable] efficacy is the "effect" of the vaccine between the placebo and ----
# control groups, which is measured by comparing the [treatment/placebo] rates between the
# placebo and control groups, and subtracting that from 1. this comparison of rates is also
# referred to, I believe, as the odds ratio, or the reported figures for efficacy.

vax_data <- vax_data %>%
  mutate(
    covid_efficacy = (1 - (treatment_covid_rate/placebo_covid_rate)),
    severe_efficacy= (1 - (treatment_severe_rate/placebo_severe_rate)),
    mortality_efficacy= (1 - (treatment_mortality_rate/placebo_mortality_rate))
  )


## rate per 10,000: Taking many assumptions into account, particularly the uniformity of the
## clinical rates in 'outside-trial' conditions, how many people per 10,000 could hypotethically
## be expected to develop symptons: [variable_rate] * 10,000

vax_data <- vax_data %>%
  mutate(across(ends_with("_rate"), ~ round((10000 * .x)), .names = "{.col}10k")) 
  


### create percent variables 
vax_data <- vax_data %>%
  mutate(across(ends_with(c("_rate", "_efficacy")), ~ round((100 *.x), 3), .names = "{.col}_pct"))


## export 
saveRDS(vax_data, file.path(data, "vax_data.rda"))
