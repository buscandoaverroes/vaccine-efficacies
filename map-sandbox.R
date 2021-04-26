# map-sandbox.R

library(mapview)
library(sf)
library(tigris)

load("/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")

# select
us_sf <- st_as_sf(data, coords = c("longitude", "latitude"), na.fail = FALSE)
us <- select(data, 
             date, id, vaccines, tests, population, confirmed, recovered, deaths, hosp, vent, icu,
             starts_with("incidence"), starts_with("admin"), starts_with("prote"), key_numeric)

mapview(us_sf, zcol = "incidence_cum", 
        at = c(0, 0.05, 0.10, 0.20, 0.3, 1))


# load US shapefiles ----
us_2_raw <- counties(state = NULL,
                 cb = TRUE, # this downloads very low res version
                 year = 2019) 

us_adm2_sf <- us_2_raw %>%
  mutate(
    fips = as.numeric(GEOID)
  ) %>% 
  select(fips, geometry) %>%
left_join(us,
  by = c("fips" = "key_numeric")
)

# mapping recent infection rates in person years
# This is sort of meaningless to the user, mostly for the final stat
mapview(us_adm2_sf, zcol = "incidence_2wk_1000py"
        #at = c(0, 0.95, 0.98, 0.99, 1)
)

# same scale, in new infections per 10k, most easily interpretable
mapview(us_adm2_sf, zcol = "incidence_2wk_10k")


# mapping protection chances
mapview(us_adm2_sf, zcol = "protection_66", 
        at = c(0, 0.95, 0.98, 0.99, 1)
        )
