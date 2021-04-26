# infection-data.R
# imports historical/recent covid-19 infection data

library(tidyverse)
library(coronavirus)
library(COVID19)

library(mapview)
library(tmap)
library(sf)
library(zoo)
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

data <- x %>%
  mutate(date = ymd(date)) %>%
  group_by(id) %>% # keep only the most recent 2 weeks
  arrange(desc(date)) %>% 
  filter(row_number() <= 14) %>%
  mutate(
    confirmed_2wk = max(confirmed) - min(confirmed), # new cases in last 2 weeks
    incidence_2wk = confirmed_2wk / population) %>% # incidence in last 2 weeks
  filter(row_number() == 1) %>% # only keep one place entry
  ungroup() %>%
  mutate(
    incidence_cum = confirmed / population
  ) %>%
  select(date, id, vaccines, tests, ends_with("2wk"), incidence_cum, everything())


# virginia sample
va <- filter(data, administrative_area_level_2 == "Virginia") %>%
  select(id, date, vaccines, tests, incidence_cum, confirmed, recovered, deaths, population,
         ends_with("2wk"),
         starts_with("admin"), starts_with("iso"), latitude, longitude, starts_with("key"))


va_sf <- st_as_sf(va, coords = c("longitude", "latitude"))
us_sf <- st_as_sf(data, coords = c("longitude", "latitude"), na.fail = FALSE)

## mapview
mapview(us_sf, zcol = "incidence_cum",
        at = c(0, 0.05, 0.10, 0.20, 0.30, 1)) # , at = c(0, 0.005, 0.010, 0.015, 0.020, 1) 

# look at tigris for importing US census shapefiles
