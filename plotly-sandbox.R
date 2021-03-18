# plotly-sandbox.R

library(plotly)


data <- filter(pfizer_sim_data, vax_name == "Pfizer-BioNTech") # arm=="covid_placebo"

data <- pfizer_sim_data %>% 
  janitor::remove_empty("cols") %>% 
  pivot_longer(cols = c(starts_with("covid"), starts_with("severe")),
               names_to = "arm", values_to = "outcome") %>%
  mutate(
    outcome = as_factor(case_when(
      (arm == "covid_placebo" | arm == "covid_vaccinated") & outcome == TRUE ~ "COVID Positive",
      (arm == "covid_placebo" | arm == "covid_vaccinated") & outcome == FALSE ~ "COVID Negative",
      (arm == "severe_placebo"| arm == "severe_vaccinated")  & outcome == TRUE ~ "Severe COVID",
      (arm == "severe_placebo"| arm == "severe_vaccinated") & outcome == FALSE ~ "COVID Negative",
      (arm == "mortality_placebo" | arm == "mortality_vaccinated") & outcome == TRUE ~ "COVID Death",
      (arm == "mortality_placebo" | arm == "mortality_vaccinated") & outcome == FALSE ~ "COVID Negative"
    )),
    outcome = fct_relevel(outcome, c("COVID Negative", "COVID Positive", "Severe COVID")) # reshuffle
  )
  

levels(data$outcome)

plot_ly(data = data) %>%
  add_trace(type= 'scatter', x = ~x, y=~y, color=~outcome
            #frame=~arm
  )
