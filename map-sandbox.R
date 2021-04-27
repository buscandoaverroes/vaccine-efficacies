# map-sandbox.R

library(mapview)
library(sf)
library(tigris)

load("/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")


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
