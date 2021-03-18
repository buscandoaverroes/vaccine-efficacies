
library(RColorBrewer)
library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(reactlog)
library(plotly)

reactlog_enable()

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
                    htmlOutput('oddsratio'))),
    fluidRow(column(6, align='center',
                    htmlOutput('placeboText'),           
            ),
            column(6, align='center',
                   htmlOutput('treatmentText'),           
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
  suffix    <- reactive({ case_when(
      input$indicator == "covid" ~ paste0("of people tested positive","<br>","for COVID-19"),
      input$indicator == "severe"~ paste0("of people experienced","<br>", "severe COVID symptoms"),
      input$indicator == "mortality"~ paste0("of people died after","<br>", "COVID symptoms"))
  })
  
  stat_eff  <- reactive({ case_when(
    input$indicator=="covid" ~ vax_data$stated_efficacy_pct[vax_data$vaccine_name == as.character(input$vaxname)],
    input$indicator=="severe"~vax_data$severe_efficacy_pct[vax_data$vaccine_name == as.character(input$vaxname)],
    input$indicator=="mortality"~vax_data$mortality_efficacy_pct[vax_data$vaccine_name == as.character(input$vaxname)]
  )})
  stat_placebo  <- reactive({ case_when(
    input$indicator=="covid" ~ vax_data$placebo_covid_rate_pct[vax_data$vaccine_name == as.character(input$vaxname)],
    input$indicator=="severe"~vax_data$placebo_severe_rate_pct[vax_data$vaccine_name == as.character(input$vaxname)],
    input$indicator=="mortality"~vax_data$placebo_mortality_rate_pct[vax_data$vaccine_name == as.character(input$vaxname)]
  )})
  stat_treatment  <- reactive({ case_when(
    input$indicator=="covid" ~ vax_data$treatment_covid_rate_pct[vax_data$vaccine_name == as.character(input$vaxname)],
    input$indicator=="severe"~vax_data$treatment_severe_rate_pct[vax_data$vaccine_name == as.character(input$vaxname)],
    input$indicator=="mortality"~vax_data$treatment_mortality_rate_pct[vax_data$vaccine_name == as.character(input$vaxname)]
  )})
  
  
  # output values ---------------------------------------------------------------------------
  output$oddsratio <- renderText({
    paste0("<b><font color=\"#737373\" size=5>","Overall Efficacy",
           "</b></font>", "<br>",
           "<b><font color=\"#737373\" size=6>",stat_eff(), "%",
           "</b></font>", "<br>"
    )
    })
  
  output$placeboText <- renderText({ 
    paste0("<font color=\"#000000\" size=2>",
           "In the Placebo group, about",
           "</font>", "<br>",
           "<b><font color=\"#CB181D\" size=12>",stat_placebo(), "%",
          "</b></font>", "<br>",
          "<font color=\"#000000\" size=2>",
          suffix(),
          "</font>"
          )
    })
  
  output$treatmentText <- renderText({ 
      paste0("<font color=\"#000000\" size=2>",
             "In the Vaccinated group, about",
             "</font>", "<br>",
             "<b><font color=\"#41AB5D\" size=12>",stat_treatment(), "%",
             "</b></font>", "<br>",
             "<font color=\"#000000\" size=2>",
             suffix(),
             "</font>"
              )
    })
  
  
  
  # data work -------------------------------------------------------------------------------

  data <- reactive({ withProgress(message = "Fetching the Data",
    sim_data %>%
      filter(vax_name == input$vaxname,
             arm == paste0(input$indicator, "_placebo") | arm == paste0(input$indicator, "_vaccinated"))) 
  })
  
    
  
  
  
  # graphs ----------------------------------------------------------------------------------
  
  p1 <- reactive({ withProgress(message = "Building the Graph",
    plot_ly(data = data()) %>%
      add_trace(type = 'scatter', mode = 'markers',
                x = ~x, y = ~y,
                color=~outcome, alpha=0.8,
                frame = ~arm) %>%
      animation_opts(frame = 300, transition = 100, easing = "linear", redraw = F))
  })
  
  output$plotly <- renderPlotly({p1()})
  
}




# Run the application 
shinyApp(ui = ui, server = server)
