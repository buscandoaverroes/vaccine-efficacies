# infection-data.R
# imports historical/recent covid-19 infection data

library(tidyverse)
library(COVID19)

library(mapview)
library(sf)
library(tigris)
library(lubridate)
library(assertthat)
library(janitor)

library(mapview)
library(tigris)

options(tigris_use_cache = TRUE) # set to redownload if FALSE


# 1. import infection data ----
## import ----
## note: "confirmed" is cumulative count of confirmed cases.
x <- covid19(country = c("US"), level = 3,
             start = "2021-04-01", end = Sys.Date()) # yay! has population


## create cumulative incidence ----
data <- x %>%
  mutate(date = ymd(date)) %>%
  group_by(id) %>% # keep only the most recent 2 weeks
  arrange(desc(date)) %>% 
  filter(row_number() <= 14) %>%
  mutate(
    confirmed_2wk = max(confirmed) - min(confirmed), # new cases in last 2 weeks
    incidence_2wk = confirmed_2wk / population,
    incidence_2wk_10k    = incidence_2wk * 10000,
    incidence_2wk_1000py = ((incidence_2wk) * 1000 * (365/14)),
    protection_66    = 1-((incidence_2wk_1000py/1000)*(1-0.90)),
    protection_90    = 1-((incidence_2wk_1000py/1000)*(1-0.90)),
    protection_95    = 1-((incidence_2wk_1000py/1000)*(1-0.95)),
    ) %>% # incidence in last 2 weeks
  filter(row_number() == 1) %>% # only keep one place entry
  ungroup() %>%
  mutate(
    incidence_cum = confirmed / population
  ) %>%
  select(date, id, vaccines, tests, ends_with("2wk"), incidence_cum, everything())


## check quality ----

### missings in confirmed ----
# there are 2 cities in Alaska (US) that have no data, assure this remains true.
n_missing_county_USA <-
  filter(data, is.na(confirmed)) %>%
  distinct(., id, .keep_all = TRUE) %>%
  nrow()

assert_that(n_missing_county_USA <= 2)



# MAP -----------------------------------------------------
## subset us infection data
infection_us <- select(data, 
             date, id, vaccines, tests, population, confirmed, recovered, deaths, hosp, vent, icu,
             starts_with("incidence"), starts_with("admin"), starts_with("prote"), key_numeric)


## load US shapefiles ----
us_2_raw <- counties(state = "Delaware",
                     cb = TRUE, # this downloads very low res version
                     year = 2020, 
                     refresh = TRUE) # refersh = redownload  


us_adm2_sf <- us_2_raw %>%
  mutate(
    fips = as.numeric(GEOID)
  ) %>% 
  select(fips, geometry) %>%
  left_join(us,
            by = c("fips" = "key_numeric")
  )


rappdirs::user_cache_dir("tigris")

# export ----
save(
  data, x, 
  file = "/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")
