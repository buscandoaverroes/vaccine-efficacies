# map-sandbox.R

library(mapview)
library(sf)
library(tigris)

load("/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")

# select
us_sf <- st_as_sf(data, coords = c("longitude", "latitude"), na.fail = FALSE)

mapview(us_sf, zcol = "incidence_cum", 
        at = c(0, 0.05, 0.10, 0.20, 0.3, 1))

