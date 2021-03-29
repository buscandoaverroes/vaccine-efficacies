# uiplot-sandbox.R
# testing different versions of the uiplot outside of shinyapp
# make sure to load vax_data rdata

library(tidyverse)
library(scales)
library(plotly)
library(gghighlight) # might be useful for individual dot labels



# data ============================ 
# ensure the data are in a way that are easy to graph 

# tilt long
data <- select(vax_data, short_name,
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
  pivot_wider( id_cols = c("short_name", "arm"), names_from = "indicator", values_from = "value") %>%
  filter(short_name == "Pfizer")


# ggplot ====================
ggplot(data, aes(arm, covid_rate_pct)) +
  geom_col() 

# works for covid rate, how to incorporate severe and mortality?
