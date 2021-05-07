# infection-graph.R
# creates the static graph objects and loads them into the shiny app data 

library(mapview)
library(sf)
library(tigris)
library(leaflet)
library(leafsync)
library(htmltools)
library(lubridate)


load("/Volumes/PROJECTS/vaccines/data/infection-data.Rdata")

## functions ----
num.dom = c(0.80, 1)
pal.bin = colorBin(palette = "OrRd",
                   bins = c(0, 10, 20, 50, 100, 300),
                   domain = us_adm2_sf$incidence_2wk_10k,
                   na.color = "#00000000",
                   reverse = F)
pal.num= colorNumeric(palette = "Spectral",
                      domain = num.dom, # us_adm2_sf$protection_90
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
    na.label = NULL, title = "<font size=2>New Cases<br>per 10k</font>",
    pal = colorBin(palette = "OrRd", domain = us_adm2_sf$incidence_2wk_10k, bins = c(0, 10, 20, 50, 100, 300), 
                   na.color = "#00000000",reverse = F),
    values = us_adm2_sf$incidence_2wk_10k, 
    #labels = c("0-10", "10-20", "20-50", "50-100", "100+"), # this wont' generate
    opacity = 0.4) 

# adjustments to legend
l1 <- browsable(
  tagList(
    list(
      tags$head(
        tags$style( # i{} controls colored boxes
          '.leaflet .legend {
          line-height: 12px;
          font-size: 12px;
          }',
          '.leaflet .legend i{ 
          width: 12px;
          height: 12px;
          float: left
          }'
        )
      ),
      l1)))


save(
  #l1,
  cntr_crds,
  labs.infections, labs.protection,
  pal.num, pal.bin, num.dom, 
  us_adm2_sf, recent_date,
  file = file.path(app, "data/map-data.Rdata")
)