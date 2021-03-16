# graph-data.R
# generates teh coordinate data for plotting

library(RColorBrewer)
library(plotly)


# import data
vax_data <- readRDS(file.path(data, "vax_data.rda"))

# generate data ============================================================

gen_sim_data <- function(name, max, long) {
  
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
  
  sim_data <- sim_data %>% # for pfizer
    mutate(
      covid_placebo        = F, 
      severe_placebo       = F, 
      mortality_placebo     = F,
      
      covid_vaccinated     = F, 
      severe_vaccinated    = F, 
      mortality_vaccinated  = F
    )
  
  # replace
  sim_data$covid_placebo[1:vax_data$placebo_covid_rate10k[vax_data$vaccine_name  %in% as.character(name)]] <- TRUE
  sim_data$severe_placebo[1:vax_data$placebo_severe_rate10k[vax_data$vaccine_name  %in% as.character(name)]] <- TRUE
  # only replace if the stat is nonmissing
  if (!is.na(vax_data$placebo_mortality_rate10k[vax_data$vaccine_name  %in% as.character(name)])) {
    sim_data$mortality_placebo[1:vax_data$placebo_mortality_rate10k[vax_data$vaccine_name %in% as.character(name)]] <- TRUE
  } else {sim_data$placebo_mortality_rate10k <- NA}
  
  sim_data$covid_vaccinated[1:vax_data$treatment_covid_rate10k[vax_data$vaccine_name %in% as.character(name)]] <- TRUE
  sim_data$severe_vaccinated[1:vax_data$treatment_severe_rate10k[vax_data$vaccine_name  %in% as.character(name)]] <- TRUE
  
  # only replace if the stat is non-missing
  if (!is.na(vax_data$treatment_mortality_rate10k[vax_data$vaccine_name %in% as.character(name)] )) {
    sim_data$mortality_vaccinated[1:vax_data$treatment_mortality_rate10k[vax_data$vaccine_name   %in% as.character(name)]] <- TRUE
  } else {sim_data$mortality_vaccinated <- NA}
  
  
  # pivot, optionally 
  if (long==TRUE) {
    sim_data <- sim_data %>%
      select(c(starts_with("covid"), starts_with("severe"), starts_with("mortality")),
               x, y) %>%
      pivot_longer(cols = c(starts_with("covid"), starts_with("severe"), starts_with("mortality")),
                   names_to = 'arm', values_to = 'outcome') %>%
      mutate(vax_name = as.character(name))

    
  }
  
  
  sim_data
}


# Generate simulated clinical data ===========================================================
pfizer_sim_data  <- gen_sim_data("Pfizer-BioNTech", 100, long = TRUE)
moderna_sim_data <- gen_sim_data("Moderna", 100, long = TRUE)


sim_data <- rbind(pfizer_sim_data, moderna_sim_data)

# Generate plotly objects  ===========================================================

# to be deteremined...will have to write a function
# 
# 

save(
  vax_data, 
  moderna_sim_data,
  pfizer_sim_data,
  sim_data,
  file = file.path(data, "app-data.Rdata")
)

### save a copy to the app directory
save(
  vax_data, 
  sim_data,
  file = file.path(app, "app-data.Rdata")
)

# 
# p1 <- ggplot(sim_data, aes(x, y, color = covid_placebo)) +
#   geom_point(alpha = 0.7, size = 0.8, shape=16) +
#   scale_color_manual(values = c("TRUE"="red", "FALSE"="lightblue")) +
#   theme_minimal()
# p1
# 
# plot_ly() %>%
#   add_trace(data = sim_data, x = ~x, y = ~y, type = 'scatter', mode = 'markers',
#             color = ~covid_placebo, alpha = 0.6)
# 
# 
# RColorBrewer::display.brewer.all(type = 'qual', n = 2)
