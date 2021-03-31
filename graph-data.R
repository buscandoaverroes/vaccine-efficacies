# graph-data.R
# generates teh coordinate data for plotting

library(RColorBrewer)
library(plotly)
library(scales)
library(gghighlight)


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



# clinical trial outcome data ================================================================
## outcome data ------------------------------------------------------------------------------

# tilt long from vax_data
vax_data_long <-
  select(vax_data, short_name,
               ends_with("n_covid_pos"),
               ends_with("covid_rate"),
               ends_with("covid_rate_pct"),
               ends_with("rate10k")
          ) %>%
  pivot_longer(
    cols = c(ends_with("n_covid_pos"),
             ends_with("covid_rate"),
             ends_with("covid_rate_pct"),
             ends_with("rate10k")),
    names_to = "indicator",
    values_to = "value")  %>%
  separate(indicator, into = c("arm", "indicator"), sep = '_', extra = 'merge') %>%
  mutate(arm = stringr::str_to_title(arm)) %>% # convert to title case
  filter(indicator == "covid_rate10k" | indicator == "severe_rate10k") #  

## Generate plotly objects  --------------------------------------------------------------

ui_outcome_plot <- function(name, ymax, bgcolor) {

  p <-   
  vax_data_long %>%
    filter(short_name == as.character(name)) %>%
    ggplot(., aes(arm, value)) +
    geom_col(aes(fill = indicator), position = 'dodge', width = 0.8) +
    scale_fill_viridis_d(
      aesthetics = "fill",
      begin = 0.4,
      option = "plasma", direction = 1,
      labels = c("Covid",
                 "Severe Covid"
                 )
    ) +
    scale_x_discrete(labels=c("Placebo", "Vaccine")) +
    scale_y_continuous(limits = c(0,ymax)) +
    labs(y = "Rate per 10k", x = NULL, fill = NULL) +
    theme_minimal() + 
    theme(
      axis.title.x = NULL,
      axis.title.y = element_text(size=14, margin = margin(t=0,r=8,b=0,l=0),
                                  face = 'bold', color = 'white'),
      axis.text.x = element_text(size=16, face = 'bold', color = 'white'),
      axis.text.y = element_text(size=10, color = 'white'),
      axis.line = element_blank(),
      legend.title = NULL,
      legend.text = element_text(size=12, colour = 'white'),
      legend.key.size = unit(5,"mm"),
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.justification = "center",
      legend.spacing.x = unit(4, 'mm'),
      legend.box.margin = margin(t=0,r=0,b=0,l=0),
      legend.box.spacing = unit(0, 'mm'),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#2c3e50", color = NA),
      plot.background = element_rect(fill = "#2c3e50", color = NA),
      panel.border = element_rect(fill = NA, linetype = 'dashed', size = 0)
    ) +
    gghighlight(value >= 0) + 
    geom_label(aes(label = value),
               position = position_dodge2(0.8), # this width matces colwidth above
               vjust = -0.2,
               label.size = 0.25,
               fill = "#525252", color = 'white', alpha = 0.4)
  
  
  p
}


### ggplot function call ----
ui_plot_pfizer <- ui_outcome_plot("Pfizer", 180)
ui_plot_moderna <- ui_outcome_plot("Moderna", 180)


save(
  vax_data, 
  sim_data,
  Pfizer_plaCov,Pfizer_plaSev,Pfizer_plaMort,Pfizer_treCov,Pfizer_treSev,Pfizer_treMort,
  Moderna_plaCov,Moderna_plaSev,Moderna_plaMort,Moderna_treCov, Moderna_treSev,Moderna_treMort,
  ui_plot_moderna, ui_plot_pfizer,
  file = file.path(data, "app-data.Rdata")
)

### save a copy to the app directory
save(
  vax_data, 
  sim_data,
  Pfizer_plaCov,Pfizer_plaSev,Pfizer_plaMort,Pfizer_treCov,Pfizer_treSev,Pfizer_treMort,
  Moderna_plaCov,Moderna_plaSev,Moderna_plaMort,Moderna_treCov, Moderna_treSev,Moderna_treMort,
  ui_plot_moderna, ui_plot_pfizer,
  file = file.path(app, "app-data.Rdata")
)

ui_plot_moderna
