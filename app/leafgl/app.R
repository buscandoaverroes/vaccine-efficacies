library(shiny)
library(leafgl)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(sf)

# cast geometry from multipolygon to polygon 
us <- st_cast(us_adm2_sf, "POLYGON", warn = T, do_split = T)

ui <- fluidPage(

    wellPanel(  
                align= 'center',
                style = 'background: #2c3e5075; padding: 0px; border-width: 1px; border-color: #2c3e50;
                             margin-left: 0px; margin-right: 0px; margin-bottom:20px; margin-top:50px;
                             padding-top:0em; width: 100%',
                HTML("<font size=5><b>Geography</b></font>"),
                radioGroupButtons( 
                    'mapProtect', label = "Vaccine Efficacy", width = '100%', 
                    choiceNames = c("66%", "90%", "95%"),
                    choiceValues = c("protection_66", "protection_90", "protection_95"),
                    status = 'myclass',  selected = "protection_90",
                    size = "normal", direction = 'horizontal', individual = F
                )),
    
        
    uiOutput('map')
   
)

server <- function(input, output) {

    
    
    
    
    # title panels 
    
    # make css
    tag.map.title <- tags$style(HTML("
         .leaflet-control.map-title {
          transform: translate(0%,-140%);
          position: float;
          left: 80%;
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
        tag.map.title, HTML(paste0("Recent Infections:<br>",
                                   day(ago2wk), " ", month(ago2wk, label = T), " - ",
                                   day(now), " ", month(now, label = T), " - "
        ))
    )
    
    title.protection <- reactive({
        tags$div(
            tag.map.title, HTML(paste0("Protection Chance with<br>",
                                       case_when(input$mapProtect == "protection_66" ~ "66%",
                                                 input$mapProtect == "protection_90" ~ "90%",
                                                 input$mapProtect == "protection_95" ~ "95%"),
                                       " Effective Vaccine"))
        )
    })
    
    
    ## infections ----
    top <- reactive({
        leaflet(data = us, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
            addGlPolygons( data = us,
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
                pal = pal.bin, 
                values = us$incidence_2wk_10k, 
                opacity = 0.4) %>%
            addControl(title.infections, position = "topleft", className = 'map-title')
    })
    
    
    ## protection ----
    bottom <- reactive({
        leaflet(data = us, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
            addProviderTiles(providers$CartoDB.DarkMatter) %>%
            setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
            addGlPolygons( data = us,
                stroke = T, color = "#969696", weight = 0.2, opacity = 0.4, smoothFactor = 0,
                fillColor = ~pal.num(eval(as.symbol(input$mapProtect))), fillOpacity = 0.9,
                label = ~case_when(input$mapProtect == "protection_66" ~ labs.protection66,
                                   input$mapProtect == "protection_90" ~ labs.protection90,
                                   input$mapProtect == "protection_95" ~ labs.protection95),
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
                title = "<font size=2>1-Year<br>Protection<br>Chance",
                pal = pal.num,
                values = c(0.90, 1),  
                opacity = 0.4,
                labFormat = labelFormat(suffix = "%", digits = 3, transform = function(x) 100*x)) %>%
            addControl(title.protection(), position = "topleft", className = 'map-title')
    })
    
    map <- reactive({sync(top(), bottom(), ncol = 1)})
    
    output$map <- renderUI({map()})
    #output$map <- renderLeaflet({bottom()}) # fine but hover and polylines won't generate.
    
    
}

shinyApp(ui = ui, server = server)
