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

# data specific to CDC/Moderna-Pfizer data
# the CDC recorded the data slightly differently than the Pfizer/Moderna papers:
#   - used person-days and not person years (convert-able)
#   - adjusted for final efficacy based on location fixed-effects. I'm not going to try to reproduce this, I will 
#   just use their final number. I will however convert person days into person years to be consistent with the rest

# convert surveillance time from person-days to person-years 
vax_data <- vax_data %>%
  mutate(
    treatment_surv_time_pyrs = if_else(is.na(treatment_surv_time_pyrs), 
                                       true  = treatment_surv_time_pdays / 365,
                                       false = treatment_surv_time_pyrs),
    placebo_surv_time_pyrs = if_else(is.na(placebo_surv_time_pyrs), 
                                       true  = placebo_surv_time_pdays / 365,
                                       false = placebo_surv_time_pyrs)
  )


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
    covid_efficacy = if_else(alt_name != "CDC",
                               true = (1 - (treatment_covid_incidence/placebo_covid_incidence)), # for non cdc studies 
                               false= covid_efficacy_site_corrected)) # use the provided data for the CDC study
   

## check that the calculated efficacy is virtually the same as the stated efficacy 
##  here, we are saying that there is no case where the difference 
##  between the stated and calculated efficacy rates
##  is greater than 0.1 percent-points or 0.001
assertthat::assert_that(
  sum((abs(vax_data$stated_efficacy - vax_data$covid_efficacy) >= 0.001) == TRUE, na.rm = TRUE) == 0
  )


## rate per 10,000: Taking many assumptions into account, particularly the uniformity of the
## clinical rates in 'outside-trial' conditions, how many people per 10,000 could hypotethically
## be expected to develop symptons: [variable_rate] * 10,000. 
    # note, this is a less suitable measure than covid_incidence because covid_incidence takes 
    # into account person-years, but the rate is still useful for raw descriptive statisitics.
vax_data <- vax_data %>%
  mutate(across(ends_with("_rate"), ~ round((10000 * .x)), .names = "{.col}10k")) 


### create percent variables 
vax_data <- vax_data %>%
  mutate(across(ends_with(c("_rate", "_efficacy")), ~ round((100 *.x), 3), .names = "{.col}_pct"))


## export 
saveRDS(vax_data, file.path(data, "vax_data.rda"))
