# make css
tag.map.title <- tags$style(HTML("
         .leaflet-control.map-title {
          transform: translate(-50%,20%);
          position: fixed;
          left: 50%;
          text-align: center;
          padding-left: 5px;
          padding-right: 5px;
          margin: 3px;
          background-color: #F0F0F080;
          border-radius: 5px;
          font-weight: bold;
          font-size: 14px;
          z-index: 100;
        } "))

title.infections <- tags$div(
  tag.map.title, HTML("Recent Infections:<br>14 April - 7 May")
)

title.protection <- tags$div(
  tag.map.title, HTML("Protection Chance with<br>90% Effective Vaccine")
)

m1<- leaflet() %>%
  addTiles() %>%
  addControl(title.infections, position = "topleft", className = "map-title")
m2<- leaflet() %>%
  addTiles() %>%
  addControl(title.infections, position = "topright", className = "map-title")

#sync(m1, m2, ncol = 1)





## infections ----
top <- 
  leaflet(data = us_adm2_sf, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
    addPolygons(
      stroke = T, color = "#969696", weight = 0.2, opacity = 0.4, smoothFactor = 0,
      fillColor = ~pal.bin(incidence_2wk_100k), fillOpacity = 0.9,
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
      pal = colorBin(palette = "OrRd", domain = us_adm2_sf$incidence_2wk_100k, bins = c(0, 10, 20, 50, 100, 300), 
                     na.color = "#00000000",reverse = F),
      values = us_adm2_sf$incidence_2wk_100k, 
      opacity = 0.4) %>%
    addControl(title.infections, position = "topleft", className = 'map-title')



## protection ----
bottom <- 
  leaflet(data = us_adm2_sf, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
    addProviderTiles(providers$CartoDB.DarkMatter) %>%
    setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
    addPolygons(
      stroke = T, color = "#969696", weight = 0.2, opacity = 0.4, smoothFactor = 0,
      fillColor = ~pal.num(protection_90), fillOpacity = 0.9,
      label = ~labs.protection,
      labelOptions = labelOptions(textsize = 20, sticky = F, 
                                  direction = "top",
                                  offset = c(0, -7),
                                  style = list(padding = "3px 3px")),
      highlightOptions = highlightOptions(stroke = TRUE, color = "black", weight = 2, opacity = 1, 
                                          fill = T, bringToFront = T
      )
    ) %>%
    addLegend(
      na.label = NULL, 
      title = paste0("<font size=2>Protection<br>Chance</font>"),
      pal = pal.num,
      values = c(0.90, 1), 
      opacity = 0.4,
      labFormat = labelFormat(suffix = "%", digits = 3, transform = function(x) 100*x)
      ) %>%
    addControl(title.protection, position = "topleft", className = 'map-title')

#combine map
sync(top, bottom, ncol = 1)
