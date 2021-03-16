
library(RColorBrewer)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)

# load data 
load("app-data.Rdata")

# UI =====================================================================================
ui <- fluidPage(

    # Application title
    titlePanel("Title"),
    
    tags$body("a few sentences that will go here but idk what yet exactly."), tags$br(),
    
    wellPanel( # input panel --------------------------------------------------
      column(12, align='center',
             fluidRow(
             radioGroupButtons('vaxname', "Vaccine",
                           choiceNames = c("Pfizer", "Moderna", "J&J"), 
                           choiceValues = c("Pfizer-BioNTech", "Moderna","Johnson&Johnson"),
                           selected = "Pfizer", size = "lg", width = '300px',
                            justified = TRUE, individual = TRUE, direction = 'horizontal')),
             fluidRow(
               awesomeRadio('indicator', "Indicator",
                            choices = c("Covid Infections" = 'covid',
                                            "Severe Covid Infections" = 'severe',
                                            "Deaths" = 'mortality'),
                            selected = "Covid Infections", inline = TRUE)
             ), 
             
            

      )
    ),
    
    fluidRow(column(12, align='center', # effectiveness value boxs -------------------------
                    valueBoxOutput('valuebox'),
    fluidRow(column(6, align='center',
                    htmlOutput('placeboText'),           
            ),
            column(6, align='center',
                   htmlOutput('treatmentText'),           
            )
    )
    )),
    
    fluidRow( # placebo/treatment ---------------------------------------------------------
      
      plotlyOutput('plotly', height = '400px')
    ),

    #tags$body("Adn this is :"), htmlOutput('text')
) # end fluidpage

# SERVER =====================================================================================
server <- function(input, output) {

  # key reactive values -------------------------------------------------------------------
  
  
  
  
  # output values ---------------------------------------------------------------------------
  output$valuebox <- renderValueBox({ valueBox("Effectiveness", 47, color = 'aqua')})
  
  output$placeboText <- renderText({ 
    paste("the key stat is:", "<b><font color=\"#DEEBF7\" size=20>","47", "</b></font>"
          )})
  
  output$treatmentText <- renderText({ 
    paste("the key stat is:", "<b><font color=\"#DEEBF7\" size=20>","47", "</b></font>"
    )})
  
  
  
  # data work -------------------------------------------------------------------------------

  
  
  
  
  # graphs ----------------------------------------------------------------------------------
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)
