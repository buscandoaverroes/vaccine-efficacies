library(shiny)
library(htmltools)
library(bslib)
library(leaflet)
library(shinyWidgets)

theme <- bslib::bs_theme(
    version = "4",
    bootswatch = "cosmo", 
    spacer = '0.5rem',
    enable_rounded = TRUE,
    primary = "#7C36B0"
)

# define css class for map title.
tag.map.title <- tags$style(HTML("
          .leaflet-control.map-title {
          left: 80px;
          text-align: center;
          position: relative;
          padding-left: 5px;
          padding-right: 5px;
          margin: 3px;
          background-color: #F0F0F080;
          border-radius: 5px;
          font-weight: bold;
          font-size: 14px;
          z-index: 100;
        }"))



# define text for map title.
map.title <- tags$div(tag.map.title, HTML("Map Title"))
    


ui <- fluidPage(

    # make useless button
    
    leafletOutput("map")
)


server <- function(input, output) {
    
    # leaflet object
    m <- leaflet() %>%
        addTiles() %>%
        addControl(map.title, position = "topleft", className = 'map-title')
    
    output$map <- renderLeaflet({m})
}

# Run the application 
shinyApp(ui = ui, server = server)
