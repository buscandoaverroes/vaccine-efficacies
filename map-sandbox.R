# map-sandbox.R

library(mapview)
library(sf)
library(tigris)
library(leaflet)
library(leafsync)

load("/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")


# mapping recent infection rates in person years
# This is sort of meaningless to the user, mostly for the final stat
mapview(us_adm2_sf, zcol = "incidence_2wk_1000py"
        #at = c(0, 0.95, 0.98, 0.99, 1)
)

# same scale, in new infections per 10k, most easily interpretable
m1 <- mapview(us_adm2_sf, zcol = "incidence_2wk_10k",
              at = c(0, 10, 20, 50, 100, 500))


# mapping protection chances
m2 <- mapview(us_adm2_sf, zcol = "protection_66", 
        at = c(0, 0.95, 0.98, 0.99, 1)
        ) 
      # be able to toggle back and forth between protection variables, either in mv or shiny

sync(m1, m2, 
     ncol = 1)
