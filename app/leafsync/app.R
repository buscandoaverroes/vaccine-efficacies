library(shiny)
library(mapview)
library(leaflet)
library(leafsync)


ui <- fluidPage(
    
    
    
    
    # edit css on ui side, for example line height in legend
    tags$head(tags$style(
        type = "text/css",
        ".leaflet .legend {
                  line-height: 2px;
                  font-size: 2px;
                }")),
    
selectInput('map1var', 'Top Map Variable', 
            choices = c("Founded" = "founded",
                        "Types"   = "number.of.types",
                        "Seasonal"= "number.of.seasonal.beers"),
            selected = "founded"),

selectInput('map2var', 'Bottom Map Variable', 
            choices = c("Founded" = "founded",
                        "Types"   = "number.of.types",
                        "Seasonal"= "number.of.seasonal.beers"),
            selected = "number.of.types"),


    
tags$h3("Synced Map"),    
uiOutput("syncMap"),

tags$h3("First map only"),
leafletOutput("top")

)

server <- function(input, output) {
    
# palettes
# pal.num1 = reactive({colorNumeric("Greens", domain = eval(as.symbol(input$map1var)))})
# pal.num2 = reactive({colorNumeric("Blues", domain = eval(as.symbol(input$map2var)))})
        
# First map 
# m1 <- reactive({
#     leaflet(data = breweries) %>%
#         addTiles() %>%
#         addCircleMarkers(color = ~pal.num1()(input$map1var)) %>%
#         addLegend(pal = pal.num1(),
#                   values = ~eval(as.symbol(input$map1var)))
# })

v1 <- reactive({mapview(breweries, zcol = input$map1var)})
v2 <- reactive({mapview(breweries, zcol = input$map2var)})


# second map
# m2 <- reactive({
#     leaflet(data = breweries) %>%
#         addTiles() %>%
#         addCircleMarkers(color = ~pal.num2()(input$map2var)) %>%
#         addLegend(pal = pal.num2,
#                   values = ~eval(as.symbol(input$map1var)))
# })
   


# sync 
map <- reactive({sync(v1(), v2(), ncol = 1)})  

output$syncMap <- renderUI({map()})
output$top     <- renderLeaflet({v1()@map})


}


shinyApp(ui = ui, server = server)
