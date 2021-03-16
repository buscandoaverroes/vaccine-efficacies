
library(RColorBrewer)
library(tidyverse)
library(shiny)
library(shinyWidgets)

# load data 
load

# UI =====================================================================================
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
    

)

# SERVER =====================================================================================
server <- function(input, output) {

}




# Run the application 
shinyApp(ui = ui, server = server)
