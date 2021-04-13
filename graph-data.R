# graph-data.R
# generates the plots for app

library(RColorBrewer)
library(plotly)
library(scales)
library(gghighlight)


# import data
vax_data <- readRDS(file.path(data, "vax_data.rda"))


# clinical trial outcome data ================================================================
## outcome data ------------------------------------------------------------------------------

ind <- c("treatment_n_participants", "placebo_n_participants", 
         "placebo_n_covid_pos", "treatment_n_covid_pos", 
         "placebo_n_severe", "treatment_n_severe")

# tilt long from vax_data
vax_data_long <-
  select(vax_data, short_name, ind
          ) %>%
  pivot_longer(
    cols = ind,
    names_to = "indicator",
    values_to = "value") %>%
  separate(indicator, into = c("arm", "indicator"), sep = '_', extra = 'merge') %>%
  mutate(arm = stringr::str_to_title(arm)) # convert to title case
  # filter(indicator == "treatment_n_participants" | indicator == "placebo_n_participants" |
  #          indicator == "placebo_n_covid_pos" | indicator == "treatment_n_covid_pos" |
  #          indicator == "placebo_n_severe" | indicator == "treatment_n_severe")

## Generate ggplot objects  --------------------------------------------------------------

ui_outcome_plot <- function(name, ymax, bgcolor) {

  p <-   
  vax_data_long %>%
    filter(short_name == as.character(name) & indicator != "n_participants") %>%
    ggplot(., aes(arm, value)) +
    geom_col(aes(fill = indicator), position = 'stack', width = 0.8) +
    scale_fill_viridis_d(
      aesthetics = "fill",
      begin = 0.42, end = 0.92,
      option = "plasma", direction = 1,
      labels = c("Covid", "Severe Covid")) +
    scale_x_discrete(labels=c("No Vaccine", paste(as.character(name), " Vaccine"))) + 
    scale_y_continuous(limits = c(0,ymax)) +
    labs(y = "Cases", x = NULL, fill = NULL,
         caption = paste0("Placebo n = ", prettyNum(
                          vax_data$placebo_n_participants[vax_data$short_name %in% as.character(name)], big.mark = ','),
                          "; Treatment n = ", prettyNum(
                          vax_data$treatment_n_participants[vax_data$short_name %in% as.character(name)],
                          big.mark = ','))) +
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
      plot.margin = margin(t=0,r=0,b=0,l=0),
      plot.caption = element_text(color = 'white', size = 10, margin = margin(t=10,r=5,b=5,l=5)),
      plot.caption.position = 'panel',
      panel.border = element_rect(fill = NA, linetype = 'dashed', size = 0)
    ) +
    gghighlight(value > 0) + 
    geom_label(aes(label = value),
               position = position_dodge2(width = 0.25), 
               label.size = 0.25, 
               fill = "#525252", color = 'white', alpha = 0.7) 
    # annotate("text", x= 1, y= -0.2, label = paste0( "n = ", # placebo
    #            vax_data$placebo_n_participants[vax_data$short_name %in% as.character(name)]),
    #          colour = "white", size = 4) +
    # annotate("text", x= 2, y = 0, label = paste0( "n = ", # treatment
    #           vax_data$treatment_n_participants[vax_data$short_name %in% as.character(name)]),
    #          colour = "white", size = 4) +
    # coord_cartesian(ylim = c(-20,ymax), expand = TRUE, default = TRUE, clip = "off")
  
  
  p
}


### ggplot function call ----
ui_plot_pfizer <- ui_outcome_plot("Pfizer", 250)
ui_plot_moderna <- ui_outcome_plot("Moderna", 250)

ui_plot_moderna
ui_plot_pfizer

# generate data for rainbow plot ----
eff_data <- expand_grid(
  pop    = seq(from = 0, to = 200, by = 1),
  eff    = seq(from = 0, to = 1, by = 0.01)
) %>% mutate(
  p_safe   = 1-((pop/1000)*(1-eff))
)

# for actual clinical data ----
eff_clinical_data <- tibble(
  name = c("Pfizer", "Moderna"),
  name_abb = c("Pfz", "Mod"),
  pop  = c(vax_data$placebo_covid_incidence[vax_data$short_name %in% "Pfizer"],
           vax_data$placebo_covid_incidence[vax_data$short_name %in% "Moderna"]),
  eff  = c(vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"],
           vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"]),
  p_safe = 1-((pop/1000)*(1-eff))
)

# store key values
breaks     <- c(0, seq(from = 0.90, to = 1.00, by = 0.02))
break_labs <- c("0"="0", "0.9"="90", "0.92"="92", "0.94"="94", "0.96"="96", "0.98"="98", "1"="")
break_lvls <- unique(base::cut(eff_data$p_safe,
                               breaks = breaks,
                               include.lowest = TRUE,
                               ordered_result = TRUE, right = TRUE))




# export ----
save(
  vax_data, eff_data,
  ui_plot_moderna, ui_plot_pfizer, breaks, break_labs, eff_clinical_data, break_lvls,
  file = file.path(data, "app-data.Rdata")
)

### save a copy to the app directory
save(
  vax_data, eff_data, 
  ui_plot_moderna, ui_plot_pfizer, breaks, break_labs, eff_clinical_data, break_lvls, 
  file = file.path(app, "data/app-data.Rdata")
)

ui_plot_moderna
