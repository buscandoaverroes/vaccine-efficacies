# infection-data.R
# imports historical/recent covid-19 infection data

library(COVID19)
library(mapview)
library(sf)
library(tigris)
library(lubridate)
library(assertthat)
library(janitor)

library(mapview)
library(tigris)

options(tigris_use_cache = FALSE) # set to redownload if FALSE

import   = TRUE  # reimports daily covid data. TRUE = redownload
download = FALSE # downloads tiger files from census. TRUE = redownload

us       = TRUE 
world2   = FALSE 
world1   = FALSE 

# 1. import infection data ----
## import ----
## note: "confirmed" is cumulative count of confirmed cases.

# create now and 2 weeks ago time variable
now    <- Sys.Date()  
ago2wk <- ymd(now) - weeks(2)


# update data according to settings
if (import == TRUE) {
  if (us == TRUE) {
    x <- covid19(country = c("US"), level = 3,
                 start = ago2wk, end = now, cache = FALSE) # don't use cache to download    
  }
  if (world2 == TRUE) {
    y <- covid19(country = NULL, level = 2,
                 start = ago2wk, end = now, cache = FALSE) # don't use cache to download
  }
  if (world1 == TRUE) {
    z <- covid19(country = NULL, level = 1,
                 start = ago2wk, end = now, cache = FALSE) # don't use cache to download
  }

} 
if (import == FALSE) { # else reload the previously saved data
  # load old data
  load("/Volumes/PROJECTS/vaccines/data/xy.Rdata")
}



## create cumulative incidence ----
data <- x %>%
  mutate(date = ymd(date)) %>%
  group_by(id) %>% # keep only the most recent 2 weeks
  arrange(desc(date)) %>% 
  filter(row_number() <= 14) %>%
  mutate(
    confirmed_2wk = max(confirmed) - min(confirmed), # new cases in last 2 weeks
    incidence_2wk = confirmed_2wk / population,
    incidence_2wk_100k    = incidence_2wk * 100000,
    incidence_2wk_1000py = ((incidence_2wk) * 1000 * (365/14)),
    protection_66    = 1-((incidence_2wk_1000py/1000)*(1-0.66)),
    protection_90    = 1-((incidence_2wk_1000py/1000)*(1-0.90)),
    protection_95    = 1-((incidence_2wk_1000py/1000)*(1-0.95)),
    lab_incidence_2wk_10k = paste0(round(incidence_2wk_10k),
                                   " per 1k"),
    lab_incidence_2wk_10k_long = paste0(administrative_area_level_3, ", ", 
                                   administrative_area_level_2, "<br>",
                                   "<b>",
                                  round(incidence_2wk_10k)," per 10k</b>"),
    lab_protection_66 = paste0(administrative_area_level_3, ", ", 
                               administrative_area_level_2, "<br>",
                               "<b>", round(100*protection_66,1), "% protect probability<b>"),
    lab_protection_90 =  paste0(administrative_area_level_3, ", ", 
                               administrative_area_level_2, "<br>",
                               "<b>", round(100*protection_90,1), "% protect probability<b>"),
    lab_protection_95 = paste0(administrative_area_level_3, ", ", 
                               administrative_area_level_2, "<br>",
                               "<b>", round(100*protection_95,1), "% protect probability<b>")
    
    ) %>% # incidence in last 2 weeks
  filter(row_number() == 1) %>% # only keep one place entry
  ungroup() %>%
  mutate(
    incidence_cum = confirmed / population
  ) %>%
  mutate(across(starts_with("protect"), ~round(100*.x, 1), .names = "{.col}_pct")) %>%
  select(date, id, vaccines, tests, ends_with("2wk"),
         incidence_cum, everything())


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
             starts_with("incidence"), starts_with("admin"), starts_with("prote"), starts_with("lab"),
             key_numeric)


## load US shapefiles ----
if (download == TRUE) {
  us2_raw <- counties(state = NULL,
                  cb = TRUE, 
                  resolution = '20m', 
                  year = 2019) # true = redownload
  
  saveRDS(us2_raw, "/Volumes/PROJECTS/vaccines/data/tigris-us2.Rda")
} else {
  us2_raw <- readRDS("/Volumes/PROJECTS/vaccines/data/tigris-us2.Rda")
}


#me <- counties("Maine", cb = TRUE)
#rappdirs::user_cache_dir("tigris")

## join with infection data ----
us_adm2_sf <- us2_raw %>% # use lowest resolution data
  mutate(
    fips = as.numeric(GEOID)
  ) %>%
  select(fips, geometry) %>%
  left_join(infection_us,
            by = c("fips" = "key_numeric")
  ) %>%
  st_transform(crs = st_crs(., 4326)) # set crs


# save the most recent date object
recent_date <- range(x$date)[2]


# create citation objects
x.cite <- covid19cite(x = x)


## check ----

### duplicates 
assert_that( nrow(us2_raw) == nrow(us_adm2_sf)) # no dups from shape files
assert_that( anyDuplicated(us_adm2_sf$fips) == 0) # no extras from infection_us

### missings \
### there will be some missings invariably because not all regions will have associated
### infection data, but we need to check that this number is very small.
assert_that(sum(is.na(us_adm2_sf$confirmed))/nrow(us_adm2_sf) <= 0.005) 



# export ----
save(
  #data, x, # not needed?
  us_adm2_sf, recent_date, x.cite, now, ago2wk, 
  file = file.path(root, "data/infection-data.Rdata")
  )

# save data
save(x, file = file.path("/Volumes/PROJECTS/vaccines/data/covid-raw.Rdata"))



