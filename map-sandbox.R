# map-sandbox.R

library(mapview)
library(sf)
library(tigris)
library(leaflet)
library(leafsync)
library(htmltools)

load("/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")


# leaflet =========================================
leaflet(us_adm2_sf) %>% 
  addPolygons(fillColor = ~incidence_2wk_10k)

# mapview =========================================
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

# sync
sync(m1, m2, 
     ncol = 1)

# overlay 
m3 <- mapview(us_adm2_sf, zcol = "incidence_2wk_10k",
              at = c(0, 10, 20, 50, 100, 500)) +
  mapview(us_adm2_sf, zcol = "protection_66", 
          at = c(0, 0.95, 0.98, 0.99, 1)
  ) 
m3

# final maps ----
## Incidence ----
m1 <- mapview(us_adm2_sf, zcol = "incidence_2wk_10k",
         col.regions = mapviewColors(us_adm2_sf, us_adm2_sf$incidence_2wk_10k,
                                     colors = hcl.colors(5, palette = "OrRd", 
                                                         rev = TRUE)),
         at = c(0, 10, 20, 50, 100, 500),
         legend.opacity = 0.9, lwd = 0.1, color = "black", 
         layer.name = "New Infections<br>per 10k people",
         label =  "lab_incidence_2wk_10k_long", 
         popup = NULL
         ) 


## Protection ----
m2 <-mapview(us_adm2_sf, zcol = "protection_90",
        col.regions = mapviewColors(us_adm2_sf, us_adm2_sf$protection_90,
                                    colors = hcl.colors(5, palette = "Viridis", 
                                                        rev = FALSE)),
        legend.opacity = 0.9, lwd = 0.1, color = "black",
        layer.name = "Protection<br>Probability",
        label = "lab_protection_90", 
        popup = NULL,
        labFormat = leaflet::labelFormat(suffix = "%", digits = 3,
                                         transform = function(x) 100*x)
        #at = c(0, 0.95, 0.98, 0.99, 1)
        ) 
  

sync(m1, m2, ncol = 1)
