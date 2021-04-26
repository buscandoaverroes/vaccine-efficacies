# infection-data.R
# imports historical/recent covid-19 infection data

library(tidyverse)
library(coronavirus)
library(COVID19)

library(mapview)
library(sf)
library(tigris)
library(lubridate)


# 1. Figure out how to import the infection data ----

## coronavirus ----
View(coronavirus) # contains raw new cases per day only, not population rates. 
                  # but could be combined with isc codes to get estimate of incidence 
                  
## COVID19 ----
## note: "confirmed" is cumulative count of confirmed cases.
x <- covid19(country = c("US"), level = 3,
             start = "2021-04-01", end = Sys.Date()) # yay! has population


# create cumulative incidence, set up time series, make 2 week incidence
# note: key_numeric in USA corresponds to FIPS code.
data <- x %>%
  mutate(date = ymd(date)) %>%
  group_by(id) %>% # keep only the most recent 2 weeks
  arrange(desc(date)) %>% 
  filter(row_number() <= 14) %>%
  mutate(
    confirmed_2wk = max(confirmed) - min(confirmed), # new cases in last 2 weeks
    incidence_2wk = confirmed_2wk / population,
    incidence_2wk_1000py = ((incidence_2wk) * 1000 * (365/14))
    ) %>% # incidence in last 2 weeks
  filter(row_number() == 1) %>% # only keep one place entry
  ungroup() %>%
  mutate(
    incidence_cum = confirmed / population
  ) %>%
  select(date, id, vaccines, tests, ends_with("2wk"), incidence_cum, everything())

save(
  data, x, 
  file = "/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")
