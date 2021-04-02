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

## incidence of covid in person years: this is the figure that will go into the efficacy rate 
## as both mRna vaccine papers use this figure as the basis for calculating vaccine efficacy rates. some
## papers have this already calculated, so if that is the case, then I will just take that raw value.
vax_data <- vax_data %>%
  mutate(
    placebo_covid_incidence = case_when(
      !is.na(vax_data$placebo_surv_time_pyrs) ~  (1000*(placebo_n_covid_pos / placebo_surv_time_pyrs)),
      TRUE                           ~ placebo_covid_incidence
    ),
    treatment_covid_incidence = case_when(
      !is.na(vax_data$treatment_surv_time_pyrs) ~  (1000*(treatment_n_covid_pos / treatment_surv_time_pyrs)),
      TRUE                           ~ treatment_covid_incidence
    )
  )



## efficacy: here [x variable] efficacy is the "effect" of the vaccine between the placebo and ----
# control groups, which is measured by comparing the [treatment/placebo] rates between the
# placebo and control groups, and subtracting that from 1. this comparison of rates is also
# referred to, I believe, as the odds ratio, or the reported figures for efficacy.

vax_data <- vax_data %>%
  mutate(
    covid_efficacy = (1 - (treatment_covid_incidence/placebo_covid_incidence))
    # severe_efficacy= (1 - (treatment_severe_rate/placebo_severe_rate)),
    # mortality_efficacy= (1 - (treatment_mortality_rate/placebo_mortality_rate))
  )

## check that the calculated efficacy is virtually the same as the stated efficacy 
##  here, we are saying that there is no case where the difference 
##  between the stated and calculated efficacy rates
##  is greater than 0.1 percent-points or 0.001
assertthat::assert_that(
  sum((abs(vax_data$stated_efficacy - vax_data$covid_efficacy) >= 0.001) == TRUE, na.rm = TRUE) == 0
  )


### create percent variables 
vax_data <- vax_data %>%
  mutate(across(ends_with(c("_rate", "_efficacy")), ~ round((100 *.x), 3), .names = "{.col}_pct"))


## export 
saveRDS(vax_data, file.path(data, "vax_data.rda"))
