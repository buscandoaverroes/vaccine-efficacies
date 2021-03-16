# graph-data.R
# generates teh coordinate data for plotting

# import data
vax_data <- readRDS(file.path(data, "vax_data.rda"))

# generate data ============================================================

## Coordinate data 
set.seed(47)
sim <- tibble(x = runif(10000), # 10,000 obs
              y = runif(10000))


## covid data
# the thinking here is to generate a binary variable that turns to "yes" at an equivalent
# rate over the 10,000 observations that reflects the clinical trial data



sim_data <- sim %>% # for pfizer
  mutate(
    covid_placebo        = F, 
    severe_placebo       = F, 
    mortality_placebo     = F,
    
    covid_vaccinated     = F, 
    severe_vaccinated    = F, 
    mortality_vaccinated  = F
  )

# replace
sim_data$covid_placebo[1:vax_data$placebo_covid_rate10k[vax_data$vaccine_name 
                                                        %in% "Pfizer-BioNTech"]] <- TRUE
sim_data$severe_placebo[1:vax_data$placebo_severe_rate10k[vax_data$vaccine_name 
                                                          %in% "Pfizer-BioNTech"]] <- TRUE
sim_data$mortality_placebo[1:vax_data$placebo_mortality_rate10k[vax_data$vaccine_name 
                                                                %in% "Pfizer-BioNTech"]] <- TRUE

sim_data$covid_vaccinated[1:vax_data$treatment_covid_rate10k[vax_data$vaccine_name 
                                                             %in% "Pfizer-BioNTech"]] <- TRUE
sim_data$severe_vaccinated[1:vax_data$treatment_severe_rate10k[vax_data$vaccine_name 
                                                               %in% "Pfizer-BioNTech"]] <- TRUE
# sim_data$mortality_vaccinated[1:vax_data$treatment_mortality_rate10k[vax_data$vaccine_name # curretnly NA
#                                                                      %in% "Pfizer-BioNTech"]] <- 





ggplot(diamonds, aes(carat, price)) +
  geom_hex()

runif(10, min = 0, max = 10)

case_when( 
  row_number() <= vax_data$placebo_covid_rate10k[1]          ~ TRUE,
  row_number() > vax_data$placebo_covid_rate10k[1]           ~ FALSE)
