
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
ui <- navbarPage("App Title",
  tabPanel("Page1", # PAGE1 ----------------------------------------------------------------------
  fluidPage(

    # Application title
    titlePanel("Title"),
    
    tags$body("a few sentences that will go here but idk what yet exactly."), tags$br(),
    
    wellPanel( # input panel --------------------------------------------------
      column(12, align='center',
             fluidRow(
             radioGroupButtons('vaxname', "Vaccine",
                           choices = c("Pfizer", "Moderna", "J&J"), 
                           #choiceValues = c("Pfizer-BioNTech", "Moderna","Johnson&Johnson"),
                           selected = "Pfizer", size = "lg", width = '300px',
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
                    htmlOutput('oddsratio'))), tags$br(),
    fluidRow(column(6, align='center',
                    htmlOutput('placeboText'),           
            ),
            column(6, align='center',
                   htmlOutput('treatmentText'),           
            )),
    tags$br(),
    
    fluidRow( # placebo/treatment ---------------------------------------------------------
      column(12, align='center',
      withSpinner(plotOutput('plotly', height = '400px', width = '400px'), type = 1)
    )),

    #tags$body("Adn this is :"), htmlOutput('text')
)), # end fluidpage, tabpanel for page1

tabPanel("Page2", # PAGE2 ----------------------------------------------------------------------
  fluidPage(
    wellPanel(
    fluidRow(align='center',
      actionBttn(inputId = 'reset_pfizer', label = "Pfizer data", size = 'sm',
                 style = 'fill', block = F, no_outline = T),
      actionBttn(inputId = 'reset_moderna', label = "Moderna data", size = 'sm',
                 style = 'fill', block = F, no_outline = T)
    ),
    splitLayout( ## begin main input panel ----------------------------------------
        wellPanel( align='center',
                   
        tags$h4(tags$b("Infection Rate")),         
        sliderInput("poprate", "",
                      width = '150px',
                      min = 0.001, max = 0.1, value = 0.03, step = 0.001),
          htmlOutput('right_poprate', width = 6),
    ),  # end first element of splitpanel
    wellPanel( align='center',
      
      tags$h4(tags$b("Efficacy Rate")),
      sliderInput("effrate", "",
                  width = '150px',
                  min = 0, max = 1, value = 0.8, step = 0.01),
      htmlOutput('right_effrate', width = 6, )
    ))), # end main input panel, end second element
      
        
    verticalLayout(
      wellPanel(align='center',
          tags$h3(tags$b("Chance of Covid Protection")),
          htmlOutput("center_protectrate")
                ),
      #plotlyOutput("pct_protected", height = '100px'),
      
      plotOutput("effplot")
    )
    
    
    
  )) # end tab panel, fluid page
) # end navbarpage



# SERVER =====================================================================================
server <- function(input, output, session) {
  
  # page 2 data ---------------------------------------------------------------------
  # f.efficacy   = 1 - (input$protectrate/input$poprate)
  # f.population = (input$protectrate / (100 - input$effrate))
  # f.vaccine     = 1- input$poprate*(100 - input$effrate)

  
  ## update efficacy rate
  # observeEvent(input$poprate, { 
  #   updateSliderInput(inputId = "effrate", value =  (input$protectrate / (1 - input$effrate))) })
  # observeEvent(input$protectrate, { 
  #   updateSliderInput(inputId = "effrate", value =  (input$protectrate / (1 - input$effrate))) })
  
  ## update protected rate
  observeEvent(input$poprate, { 
    updateSliderInput(inputId = "protectrate", value = 1- (input$poprate*(1 - input$effrate))) })
  observeEvent(input$effrate, { 
    updateSliderInput(inputId = "protectrate", value = 1- (input$poprate*(1 - input$effrate))) })
  
  ## update population rate
  # observeEvent(input$protectrate, {
  #   updateSliderInput(inputId = "poprate", value = 1 - (input$protectrate/input$poprate)) })
  # observeEvent(input$effrate, {
  #   updateSliderInput(inputId = "poprate", value = 1 - (input$protectrate/input$poprate)) })
  #   
  
  ## update sliders with clinical data presets 
  observeEvent(input$reset_pfizer, {
    updateSliderInput('poprate', session = session, value = vax_data$placebo_covid_rate[vax_data$short_name %in% "Pfizer"])
    updateSliderInput('effrate', session = session, value = vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"])
  })
  observeEvent(input$reset_moderna, {
    updateSliderInput('poprate', session = session, value = vax_data$placebo_covid_rate[vax_data$short_name %in% "Moderna"])
    updateSliderInput('effrate', session = session,  value = vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"])
  })

  
  
  # for now, generate this data in-app
  eff_data <- expand_grid(
    pop    = seq(from = 0, to = 0.1, by = 0.05),
    eff    = seq(from = 0, to = 1, by = 0.01),
  ) %>% mutate(
    p_safe   = 1-(pop*(1-eff))
  )
  
  
  # reactive data 
  eff_pop <- reactive({input$poprate})
  eff_eff <- reactive({input$effrate})
  
  # for hypothetical point data
  eff_point <- reactive({
    tibble(
      pop = eff_pop(), 
      eff = eff_eff(), 
      p_safe = 1-(pop*(1-eff))
    )
  })
  
  # for actual clinical data
  eff_clinical_data <- tibble(
    name = c("Pfizer", "Moderna"),
    pop  = c(vax_data$placebo_covid_rate[vax_data$short_name %in% "Pfizer"],
             vax_data$placebo_covid_rate[vax_data$short_name %in% "Moderna"]),
    eff  = c(vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"],
             vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"]),
    p_safe = 1-(pop*(1-eff))
  )
  
  
  
  
    
  

  # key reactive values -------------------------------------------------------------------
  vaccine   <- reactive({ input$vaxname})
  indicator <- reactive({ input$indicator})
  suffix    <- reactive({ case_when(
      input$indicator == "covid" ~ paste0("tested positive","<br>","for COVID-19"),
      input$indicator == "severe"~ paste0("experienced","<br>", "severe COVID symptoms"),
      input$indicator == "mortality"~ paste0("died after","<br>", "COVID symptoms"))
  })
  
  stat_eff  <- reactive({ case_when(
    input$indicator=="covid" ~ vax_data$stated_efficacy_pct[vax_data$short_name == as.character(input$vaxname)],
    input$indicator=="severe"~vax_data$severe_efficacy_pct[vax_data$short_name == as.character(input$vaxname)],
    input$indicator=="mortality"~vax_data$mortality_efficacy_pct[vax_data$short_name == as.character(input$vaxname)]
  )})
  stat_placebo  <- reactive({ case_when(
    input$indicator=="covid" ~ vax_data$placebo_covid_rate_pct[vax_data$short_name == as.character(input$vaxname)],
    input$indicator=="severe"~vax_data$placebo_severe_rate_pct[vax_data$short_name == as.character(input$vaxname)],
    input$indicator=="mortality"~vax_data$placebo_mortality_rate_pct[vax_data$short_name == as.character(input$vaxname)]
  )})
  stat_treatment  <- reactive({ case_when(
    input$indicator=="covid" ~ vax_data$treatment_covid_rate_pct[vax_data$short_name == as.character(input$vaxname)],
    input$indicator=="severe"~vax_data$treatment_severe_rate_pct[vax_data$short_name == as.character(input$vaxname)],
    input$indicator=="mortality"~vax_data$treatment_mortality_rate_pct[vax_data$short_name == as.character(input$vaxname)]
  )})
  
  
  right_covid_per_10k <- reactive({ input$poprate * 10000 })
  right_covid_efficacy<- reactive({ round(input$effrate*100, 2) })
  protectrate         <- reactive({ round((1- (input$poprate*(1 - input$effrate)))*100,2)  })
  
  
  # output values ---------------------------------------------------------------------------
  output$oddsratio <- renderText({ # containers should go for each of these three?
    paste0("<b><font color=\"#737373\" size=5>","Efficacy",
           "</b></font>", "<br>",
           "<b><font color=\"#737373\" size=6>",stat_eff(), "%",
           "</b></font>", "<br>"
    )
    })
  
  output$placeboText <- renderText({ 
    paste0(
           "<b><font color=\"#CB181D\" size=6>",
           "Without the Vaccine:<br>",
           stat_placebo(), "%",
          "</b></font>", "<br>",
          "<font color=\"#000000\" size=2>",
          "",
          suffix(),
          "</font>"
          )
    })
  
  output$treatmentText <- renderText({ 
      paste0(
             "<b><font color=\"#41AB5D\" size=6>",
             "With the Vaccine:<br>",
             stat_treatment(), "%",
             "</b></font>", "<br>",
             "<font color=\"#000000\" size=2>",
             "",
             suffix(),
             "</font>"
              )
    })
  
  output$right_poprate <- renderText({
    paste0(
      # "<b><font color=\"#41AB5D\" size=2>",
      # "Covid Infection Rate:<br>",
      # "</b></font>",
      "<b><font color=\"#000000\" size=6>",
      right_covid_per_10k(), " in 10,000",
      "</b></font>"
    )
  })
  
  output$right_effrate <- renderText({
    paste0(
      # "<b><font color=\"#41AB5D\" size=2>",
      # "Vaccine Effiacy:<br>",
      # "</b></font>",
      "<b><font color=\"#000000\" size=6>",
      right_covid_efficacy(), "%",
      "</b></font>"
    )
  })
  
  output$center_protectrate <- renderText({
    paste0(
      "<b><font color=\"#000000\" size=6>",
      protectrate(), "%",
      "</b></font>"
    )
  })
  
  
  
  # data work -------------------------------------------------------------------------------

  data <- reactive({ withProgress(message = "Fetching the Data",
    sim_data %>%
      filter(vax_name == input$vaxname)) 
  })
  
    
  
  
  
  # graphs ----------------------------------------------------------------------------------
  
  # page1 graph 
  p1 <- reactive({ withProgress(message = "Building the Graph",
        ggplot(data = data(), aes(x, y)) + # use raster since high perferfmance and all same size
          geom_raster(aes(fill = outcome, alpha = outcome)) +
          scale_fill_manual(values = c(
            "COVID Negative" = brewer.pal(9, "Blues")[2],
            "COVID Positive" = brewer.pal(9, "Purples")[6],
            "Severe COVID"   = brewer.pal(9, "Set1")[1]),
            name = NULL,
            labels = c("COVID Negative" = "No COVID",
                       "COVID Positive" = "COVID",
                       "Severe COVID"   = "Severe COVID")
          ) +
          scale_alpha_manual(values = c(0.4, 1.0, 1.0), guide = NULL) +
          facet_grid(cols = vars(arm)) +
          theme_void() +
          theme(legend.position = 'top',
                legend.key.size = unit(5,'mm'),
                legend.spacing.x = unit(5,'mm'),
                legend.justification = 'center',
                legend.margin = margin(2,0,0,0),
                panel.spacing.x = unit(0,'mm'),
                plot.margin = margin(0,0,0,0))
  )
  })
  
  output$plotly <- renderPlot({p1()})
  



  # page2 graph 
  p2 <- reactive({
    ggplot(data = eff_data, aes(x = eff, y = pop, color = eff)) +
      geom_contour_filled(aes(z = p_safe)) +
      scale_fill_viridis_d(name = "Chance of Protection",
                           option = 'plasma', direction = 1,
                           alpha = 0.8,
                           labels = c("90-91%", "91-92%", "92-93%", "93-94%", "94-95%",
                                      "95-96%", "96-97%", "97-98%", "98-99%", "99-100%")) +
      geom_vline(aes(xintercept = eff_eff()), linetype= "dotdash", alpha = 0.5) +
      geom_hline(aes(yintercept = eff_pop()), linetype = "dotdash", alpha = 0.5) + 
      geom_point(data = eff_point(), aes(x = eff, y = pop),
                 size = 2, shape = 5, alpha=1, color = "blue", stroke = 2) +
      # {Pfizer data}
      geom_vline(aes(xintercept = vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"]),
                 linetype= "solid", alpha = 0.3) +
      geom_hline(aes(yintercept = vax_data$placebo_covid_rate[vax_data$short_name %in% "Pfizer"]),
                 linetype = "solid", alpha = 0.3) + 
      geom_point(data = eff_clinical_data[eff_clinical_data$name =="Pfizer",], aes(x = eff, y = pop),
                 size = 2, shape = 20, alpha=1, color = "purple", stroke = 2) +
      # {Moderna data}
      geom_vline(aes(xintercept = vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"]),
                 linetype= "dotted", alpha = 0.4) +
      geom_hline(aes(yintercept = vax_data$placebo_covid_rate[vax_data$short_name %in% "Moderna"]),
                 linetype = "dotted", alpha = 0.4) + 
      geom_point(data = eff_clinical_data[eff_clinical_data$name =="Moderna",], aes(x = eff, y = pop),
                 size = 2, shape = 20, alpha=1, color = "red", stroke = 2) +
      labs(x = "Vaccine Efficacy Rate",
           y = "COVID-19 Rate in general population") +
      theme_minimal()
    
  })
    
  output$effplot <- renderPlot({p2()})
  
  
  
  # page2 plotly percent protected indicator
  
  # threshold   <- reactive({ 1 - input$poprate})
  # 
  # p3 <- reactive({
  #   plot_ly(type = "indicator", mode = 'number', title = "",
  #           value = protectrate(),
  #           number = list(valueformat = '.2%'),
  #           gauge = list(
  #             bar = list(color = "#4292C6"),
  #             axis = list(
  #               range = c(0,1),
  #               tickmode = 'array',
  #               tickvals = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
  #             ),
  #             threshold = list(
  #               thickness = 1,
  #               line = list(width=4, color="#CB181D"),
  #               value = threshold()
  #             )
  #           )
  #   )
  # })
  # 
  # output$pct_protected <- renderPlotly({p3()})
  

} # end server ------------------------------------------------------------------------


# Run the application 
shinyApp(ui = ui, server = server)
