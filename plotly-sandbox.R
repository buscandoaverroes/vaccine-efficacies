# plotly-sandbox.R

library(plotly)


data <- filter(pfizer_sim_data, vax_name == "Pfizer-BioNTech") # arm=="covid_placebo"



# make data again, with two groups, 
sim_data <- 
  expand_grid(x = 1:100, y = 1:100)  # create all cominations of 100 ^2, or 10,000 obs

sim_data <-
  sim_data %>%
  mutate(r = runif(length(sim_data$x), min = 0, max = 100)) %>% # make 10,000 random obs
  arrange(r)



## covid data
# the thinking here is to generate a binary variable that turns to "yes" at an equivalent
# rate over the 10,000 observations that reflects the clinical trial data

sim_data <- sim_data %>% 
  mutate(
    placebo_outcomes = "COVID Negative",
    treatment_outcomes = "COVID Negative"
  )

# subset for ease, not the most beautiful now but saves eyesores later when subsetting

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

# replace for covid Positive
sim_data$placebo_outcomes[1:Pfizer_plaCov] <- "COVID Positive"
sim_data$treatment_outcomes[1:Pfizer_treCov] <- "COVID Positive"

# replace for covid Severe
sim_data$placebo_outcomes[(Pfizer_plaCov+1):((Pfizer_plaCov+1)+Pfizer_plaSev)] <- "Severe COVID"
sim_data$treatment_outcomes[(Pfizer_treCov+1):((Pfizer_treCov+1)+Pfizer_treSev)] <- "Severe COVID"


# replace for mortality only if the stat is non-missing
if (!is.na(Pfizer_plaMort) ) {
  sim_data$placebo_outcomes[(((Pfizer_plaCov+1)+Pfizer_plaSev)+1):((((Pfizer_plaCov+1)+Pfizer_plaSev)+1)+1)] <- "COVID Death"
} 
if (!is.na(Pfizer_treMort) ) {
  sim_data$treatment_outcomes[(((Pfizer_treCov+1)+Pfizer_treSev)+1):((((Pfizer_treCov+1)+Pfizer_treSev)+1)+1)] <- "COVID Death"
} 

data <- sim_data %>% 
  janitor::remove_empty("cols") %>% 
  pivot_longer(cols = c(ends_with("outcomes")),
               names_to = "group", values_to = "outcome") %>%
  mutate(arm = as_factor(case_when(
    group == "placebo_outcomes" ~ "Placebo", 
    TRUE ~ "Treatment"
  ))) %>%
  select(-group, -r)

  

plot_ly(data = data) %>%
  add_trace(type= 'scatter', mode='markers',
            marker=list(size=5),
            x = ~x, y=~y, #z=~outcome,
            color=~outcome,
            frame=~arm
  ) %>%
  animation_opts(frame = 500, transition = 0, easing = "linear", redraw = TRUE)
