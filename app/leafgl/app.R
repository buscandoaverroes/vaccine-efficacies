library(shiny)
library(leafgl)
library(leaflet)
library(htmlwidgets)
library(htmltools)



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
    
        
    leafglOutput('map')
   
)

server <- function(input, output) {

   
}

shinyApp(ui = ui, server = server)
