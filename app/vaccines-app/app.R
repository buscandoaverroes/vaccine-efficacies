
library(RColorBrewer)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(reactlog)


options(shiny.reactlog = TRUE)


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
                           selected = "Pfizer-BioNTech", size = "lg", width = '300px',
                            justified = TRUE, individual = TRUE, direction = 'horizontal')),
             fluidRow(
               awesomeRadio('indicator', "Indicator",
                            choices = c("Covid Infections" = 'covid',
                                            "Severe Covid Infections" = 'severe',
                                            "Deaths" = 'mortality'),
                            selected = "covid", inline = TRUE)
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
      column(12, align='center',
      withSpinner(plotlyOutput('plotly', height = '400px', width = '400px'), type = 1)
    )),

    #tags$body("Adn this is :"), htmlOutput('text')
) # end fluidpage

# SERVER =====================================================================================
server <- function(input, output) {

  # key reactive values -------------------------------------------------------------------
  vaccine   <- reactive({ input$vaxname})
  indicator <- reactive({ input$indicator})
  
  
  
  # output values ---------------------------------------------------------------------------
  output$valuebox <- renderValueBox({ valueBox("Effectiveness", 47, color = 'aqua')})
  
  output$placeboText <- renderText({ 
    paste("the key stat is:", "<b><font color=\"#DEEBF7\" size=20>","47", "</b></font>"
          )})
  
  output$treatmentText <- renderText({ 
    paste("the key stat is:", "<b><font color=\"#DEEBF7\" size=20>","47", "</b></font>"
    )})
  
  
  
  # data work -------------------------------------------------------------------------------

  data <- reactive({
    sim_data %>%
      filter(vax_name == input$vaxname,
             arm == paste0(input$indicator, "_placebo") | arm == paste0(input$indicator, "_vaccinated")) 
  })
  
    
  
  
  
  # graphs ----------------------------------------------------------------------------------
  
  p1 <- reactive({
    plot_ly(data = data()) %>%
      add_trace(type = 'scatter', mode = 'markers', x = ~x, y = ~y, color=~outcome, alpha=0.8, frame = ~arm) %>%
      animation_opts(frame = 1000, transition = 100, easing = "linear", redraw = F)
  })
  
  output$plotly <- renderPlotly({p1()})
  
}




# Run the application 
shinyApp(ui = ui, server = server)
