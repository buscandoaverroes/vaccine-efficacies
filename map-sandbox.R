# map-sandbox.R

library(mapview)
library(sf)
library(tigris)
library(leaflet)
library(leafsync)
library(htmltools)
library(tmap)


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
      # 
 

# overlay 
m3 <- mapview(us_adm2_sf, zcol = "incidence_2wk_10k",
              at = c(0, 10, 20, 50, 100, 500)) +
  mapview(us_adm2_sf, zcol = "protection_66", 
          at = c(0, 0.95, 0.98, 0.99, 1)
  ) 
m3

# final maps ----
## Incidence ----

mapviewOptions(
  fgb = TRUE, viewer.suppress = FALSE, na.color = '#00000000'
  )

m1 <- mapview(us_adm2_sf, zcol = "incidence_2wk_10k",
              map.types = "CartoDB.DarkMatter",
         col.regions = mapviewColors(us_adm2_sf, us_adm2_sf$incidence_2wk_10k,
                                     colors = hcl.colors(5, palette = "OrRd", 
                                                         rev = TRUE)),
         at = c(0, 10, 20, 50, 100, 500),
         legend.opacity = 0.9, lwd = 0.1, color = "#969696", 
         layer.name = "New Infections<br>per 10k people",
         label =  "lab_incidence_2wk_10k_long", 
         popup = NULL,
         legend = F, 
         alpha.regions = 0.7, alpha = 0.6,
         highlight = leaflet::highlightOptions(stroke = TRUE, color = "black", weight = '3px', bringToFront = T, fillColor = 'white')
  )
  

## Protection ----
m2 <- mapview(us_adm2_sf, zcol = "protection_90",
              map.types = "CartoDB.DarkMatter",
        col.regions = mapviewColors(us_adm2_sf, us_adm2_sf$protection_90,
                                    colors = hcl.colors(5, palette = "Spectral", rev = F)),
        legend.opacity = 0.9, lwd = 1, color = "#969696", #RColorBrewer::brewer.pal(9, "Greys")
        layer.name = "Protection<br>Probability",
        label = "lab_protection_90", 
        popup = NULL, 
        legend = F, 
        alpha.regions = 0.7, alpha = 0.6,
        highlight = leaflet::highlightOptions(stroke = TRUE, color = "black", weight = '3px', bringToFront = T, fillColor = 'white'), # function not exist
        labFormat = leaflet::labelFormat(suffix = "%", digits = 3,
                                         transform = function(x) 100*x)
        ) 


# determine mean coords
cntr_crds <- c(mean(st_coordinates(us_adm2_sf)[ ,1]),
               mean(st_coordinates(us_adm2_sf)[ ,2]))

# set zoom before sync
map.a <- m1@map %>% setView(cntr_crds[1], cntr_crds[2], zoom = 4) %>%
  addLegend(na.label = NULL, title = "<font size=3>New Infections<br>per 10k people</font>",
            pal = colorBin(palette = "OrRd", domain = us_adm2_sf$incidence_2wk_10k, na.color = "#00000000",reverse = F),
            values = us_adm2_sf$incidence_2wk_10k,
            opacity = 0.4)
map.b <- m2@map %>% setView(cntr_crds[1], cntr_crds[2], zoom = 4) %>%
  addLegend(na.label = NULL, title = "<font size=3>Vaccinated<br>Protection<br>Probability</font>",
            pal = colorNumeric(palette = "Spectral", domain = us_adm2_sf$protection_90, na.color = "#00000000", reverse = F),
            values = us_adm2_sf$protection_90, 
            opacity = 0.4,
            labFormat = labelFormat(suffix = "%", digits = 3, transform = function(x) 100*x))
  
map.a
map.b
# sync
map <- sync(map.a, map.b, ncol = 1)

map




# tmap ===============================
# This is fine for static things but not for interacitve, and the map probably needs to be
# interactive.
tm_shape(us_adm2_sf, projection = ) +
  tm_fill(col = "incidence_2wk_10k", title = "New Infections\nper 10k people",
          palette = "OrRd", style = 'pretty', showNA = FALSE, legend.show = FALSE) 
  tm_borders()


  
# raw leaflet ========================
## functions ----
pal.bin = colorBin(palette = "OrRd",
                   bins = c(0, 10, 20, 50, 100, 300),
                   domain = us_adm2_sf$incidence_2wk_10k,
                   na.color = "#00000000",
                   reverse = F)
pal.num= colorNumeric(palette = "Spectral",
                      domain = us_adm2_sf$protection_90,
                      na.color = "#00000000",
                      reverse = F)
  

labs.infections <- sprintf(
  "<strong>%s, %s </strong><br>
  %.1f per 10k", 
  us_adm2_sf$administrative_area_level_3,
  us_adm2_sf$administrative_area_level_2,
  us_adm2_sf$incidence_2wk_10k
)  %>%
  lapply(htmltools::HTML)
labs.protection <- sprintf(
  "<strong>%s, %s </strong><br><b>%.1f%%</b> protection probability", 
  us_adm2_sf$administrative_area_level_3,
  us_adm2_sf$administrative_area_level_2,
  us_adm2_sf$protection_90_pct
)  %>%
  lapply(htmltools::HTML)

# determine mean coords
cntr_crds <- c(mean(st_coordinates(us_adm2_sf)[ ,1]),
               mean(st_coordinates(us_adm2_sf)[ ,2]))

  

## leaflet calls ----
### infections
l1 <- leaflet(data = us_adm2_sf, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
  addPolygons(
      stroke = T, color = "#969696", weight = 0.2, opacity = 0.4, smoothFactor = 0,
      fillColor = ~pal.bin(incidence_2wk_10k), fillOpacity = 0.9,
      label = ~labs.infections, labelOptions = labelOptions(textsize = 20, sticky = F, 
                                                            direction = "top",
                                                            offset = c(0, -7),
                                                            style = list(padding = "3px 3px")),
      highlightOptions = highlightOptions(stroke = TRUE, color = "black", weight = 2, opacity = 1, 
                                          fill = T, bringToFront = T
                                          )
    ) %>%
  addLegend(
    na.label = NULL, title = "<font size=3>New Infections<br>per 10k people</font>",
    pal = colorBin(palette = "OrRd", domain = us_adm2_sf$incidence_2wk_10k, bins = c(0, 10, 20, 50, 100, 300), 
                   na.color = "#00000000",reverse = F),
    values = us_adm2_sf$incidence_2wk_10k,
    opacity = 0.4) 

### protection
l2 <- leaflet(data = us_adm2_sf, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
  addPolygons(
    stroke = T, color = "#969696", weight = 0.2, opacity = 0.4, smoothFactor = 0,
    fillColor = ~pal.num(protection_90), fillOpacity = 0.9,
    label = ~labs.protection, labelOptions = labelOptions(textsize = 20, sticky = F, 
                                                          direction = "top",
                                                          offset = c(0, -7),
                                                          style = list(padding = "3px 3px")),
    highlightOptions = highlightOptions(stroke = TRUE, color = "black", weight = 2, opacity = 1, 
                                        fill = T, bringToFront = T
    )
  ) %>%
  addLegend(
    na.label = NULL, title = "<font size=3>Protection<br>Chance if<br>Vaccinated</font>",
    pal = colorNumeric(palette = "Spectral",
                       domain = us_adm2_sf$protection_90,
                       na.color = "#00000000",
                       reverse = F),
    values = us_adm2_sf$protection_90, 
    opacity = 0.4,
    labFormat = labelFormat(suffix = "%", digits = 3, transform = function(x) 100*x))

map <- sync(l1, l2, ncol = 1)
map
l2
