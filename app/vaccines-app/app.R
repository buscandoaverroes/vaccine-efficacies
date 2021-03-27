
library(RColorBrewer)
library(tidyverse)
library(scales)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(reactlog)
library(plotly)
library(bslib)
#library(mathjaxr)

reactlog_enable()

#options(shiny.reactlog = TRUE)

theme <- bslib::bs_theme(
  version = 4, bootswatch = "cosmo",
  spacer = '1.5rem',
  enable_rounded = TRUE,
  primary = "#7C36B0"
                         )

# load data 
load("app-data.Rdata")

# default input values 
dflt_poprate = 0.03
dflt_effrate = 0.8


# UI =====================================================================================
ui = navbarPage("Vaccines",

# Application title

                
  tabPanel("Vaccine Efficacies", # PAGE: efficacies ----------------------------------------------------------------------
     fluidPage(
       theme = theme,
       
       tags$h4(tags$b("The Efficacy Rate is not your chance of being protected")), 
       tags$body("It's just math! Your chances of Covid protection depend on:"),
       tags$li("the vaccine efficacy rate"),
       tags$li("how fast the virus spreads"),
       tags$li("and other individual risk factors"),

       tags$h4("Estimate your Chances"),
       tags$body("Select a vaccine preset to explore the clinical trial data or use the sliders to adjust to a hypothetical scenario.
                 Worried about variants? Lower the efficacy rate."),
       
     

       
       br(), hr(),
       tags$body("variants:"),textOutput('out'),
       tags$body("beta:"), textOutput('out2'),
       prettySwitch('showmath', 'Show Math', slim = T, inline = T, status = 'info'),
       wellPanel(align='center',
                 style= 'background: #2c3e50',

                actionBttn(inputId = 'reset_pfizer', label = "Pfizer data", size = 'sm',
                          color = 'primary',  style = 'fill', block = F, no_outline = T),
                actionBttn(inputId = 'reset_moderna', label = "Moderna data", size = 'sm',
                           color = 'primary', style = 'fill', block = F, no_outline = T),
                checkboxGroupButtons(
                  'variants', label = NULL,
                  choices = c("Variant A", "Variant B"), 
                   status = 'primary',
                  size = "sm", direction = 'horizontal'
                )
           
       ),
       splitLayout( ##  main input panels ----------------------------------------
                    wellPanel( align='center',
                               
                               tags$h4(tags$b("Infection Rate")),         
                               sliderInput("poprate", label = NULL,
                                           width = '150px', ticks = F,
                                           min = 0.001, max = 0.1, value = 0.03, step = 0.001),
                               htmlOutput('right_poprate', width = 6)
                    ),  # end first element of splitpanel
                    wellPanel( align='center',
                               
                               tags$h4(tags$b("Efficacy Rate")),
                               sliderInput("effrate", label = NULL,
                                           width = '150px', ticks = F, 
                                           min = 0, max = 1, value = 0.8, step = 0.01),
                               htmlOutput('right_effrate', width = 6 )
                    )), # end main input panel, end second element
       

       verticalLayout(  ## math ----
        conditionalPanel(
          condition = 'input.showmath',
          
          wellPanel(align='center',
                    style='background: #D9F9E5',
                    
                    tags$ul(htmlOutput('math', container = tags$h4))
                    # htmloutput same as renderui
          )
        ),
         wellPanel(align='center', ## protection rate ----
                   style= 'background: #D9F9E5',
                   
                   
                   tags$h3(tags$b("Estimated Chance of Covid Protection")),
                   htmlOutput("center_protectrate")
         ), # end wellpanel
         #plotlyOutput("pct_protected", height = '100px'),
         
         plotOutput("effplot") ## rainbow curve plot ----
       ),
       
       tags$h4("Key Takeaways"),
       tags$li("Your protection chances from Covid are much higher than the stated efficacy rate."),
       tags$li("Vaccines work. All approved COVID-19 vaccines reduce the average person's chances of
             contracting covid to near 0."),
       tags$li("Differences in efficacy rates between vaccines don't matter much in practicality -- it's 
               more important just to get vaccinated."),
       
       tags$h4("Keep in Mind"),
       tags$body("The clinical trials occured in different places and stages of the pandemic, so 
                 they aren't perfectly comparable.
                 Also, these studies revealed slightly different efficacies based on demographic factors like age (see sources)."),
       tags$body("Finally, if you're vaccinated, it's very important to continue following CDC guidelines on masking and social
                 distancing to protect those still waiting for a vaccine.") 
       

     )), # end tab panel, fluid page              
          
                
                
                
  tabPanel("Clinical Data", # PAGE: clinical data ----------------------------------------------------------------------
  fluidPage(


    tags$body("a few sentences that will go here but idk what yet exactly."), tags$br(),
    
    wellPanel( ## input panel --------------------------------------------------
      column(12, align='center',
             fluidRow(
             radioGroupButtons('vaxname', "Vaccine",
                           choices = c("Pfizer", "Moderna"), 
                           #choiceValues = c("Pfizer-BioNTech", "Moderna","Johnson&Johnson"),
                           selected = "Pfizer", size = "lg", width = '300px',
                            justified = TRUE, individual = TRUE, direction = 'horizontal')),
             fluidRow(
               awesomeRadio('indicator', "Indicator",
                            choices = c("Covid Infections" = 'covid'
                                            # "Severe Covid Infections" = 'severe',
                                            # "Deaths" = 'mortality'
                                        ),
                            selected = "covid", inline = TRUE)
             ) 
             
            

      )
    ),
    
    fluidRow(column(12, align='center', ## effectiveness value boxs -------------------------
                    htmlOutput('oddsratio'))), tags$br(),
    fluidRow(column(6, align='center',
                    htmlOutput('placeboText')           
            ),
            column(6, align='center',
                   htmlOutput('treatmentText')          
            )),
    tags$br(),
    
    fluidRow( ## placebo/treatment ---------------------------------------------------------
      column(12, align='center',
      withSpinner(plotOutput('plotly', height = '400px', width = '400px'), type = 1)
    ))

    #tags$body("Adn this is :"), htmlOutput('text')
)) # end fluidpage, tabpanel for page1

) # end navbarpage, taglist



# SERVER =====================================================================================
server <- function(input, output, session) {
  
  # efficacy data ---------------------------------------------------------------------

  ## update protected rate ----
  observeEvent(input$poprate, { 
    updateSliderInput(inputId = "protectrate", value = 1- (input$poprate*(1 - input$effrate))) })
  observeEvent(input$effrate, { 
    updateSliderInput(inputId = "protectrate", value = 1- (input$poprate*(1 - input$effrate))) })
  
  ## update sliders with clinical data presets ----
  observeEvent(input$reset_pfizer, {
    updateSliderInput('poprate', session = session, value = vax_data$placebo_covid_rate[vax_data$short_name %in% "Pfizer"])
    updateSliderInput('effrate', session = session, value = vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"])
  })
  observeEvent(input$reset_moderna, {
    updateSliderInput('poprate', session = session, value = vax_data$placebo_covid_rate[vax_data$short_name %in% "Moderna"])
    updateSliderInput('effrate', session = session,  value = vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"])
  })
  
  
  usr_poprate <- reactive({input$poprate}) 
  usr_effrate <- reactive({input$effrate})
  

  
  ## variant adjustments ----
  ### Variant A: reduces efficacy by 0.15 (15%), increases population prevalence by 0.1 (10%)
  ### Variant B: reduces efficacy by 0.10 (10%), increases population prevalence by 0.4 (40%)
  ### beta ----
  beta <- reactiveValues(e = 0, p = 0) # set scalar values initially to 0
    
    # 
    # case_when(
    #   is.null(input$variants)                ~ beta$a = 0 <-, # standard scenario
    #   input$variants[1] == "Variant A"        ~ a = 0.15,          # A only
    #   input$variants[1] == "Variant B"        ~ a = 0.1,           # B only 
    #   input$variants[1] == "Variant A" & 
    #     input$variants[2] == "Variant B"      ~ a = 0.5           # A and B (to math figure)
    # )

  
  ### variant scalers ----
  observeEvent(input$variants, {
    
    # first determine scale factor based on combination of variant inputs
    ## set scale factor, where first element will be for effrate, second for poprate
    ##  beta[effrate, poprate]

    
    
    
    # then update input values based on scale factors
    
    if (input$variants == "Variant A") {
      # store value?? this requires going "back" to reset to the previous number so we can do the calc from there.

      updateSliderInput('effrate', session = session, value = input$effrate - (input$effrate*beta$e))
      updateSliderInput('poprate', session = session, value = input$poprate + (input$poprate*beta$p))
      #updateCheckboxGroupButtons('variants', session = session, disabledChoices = "Variant A")
      
    }
    
    if (input$variants == "Variant B") {
      updateSliderInput('effrate', session = session, value = input$effrate - (input$effrate*beta$e))
      updateSliderInput('poprate', session = session, value = input$poprate + (input$poprate*beta$p))
      #updateCheckboxGroupButtons('variants', session = session, disabledChoices = "Variant B")
      
    }
    
    if (is.null(input$variants)) { # this is not update properly
      updateSliderInput('effrate', session = session, value = 0)
      updateSliderInput('poprate', session = session, value = 0)
      #updateCheckboxGroupButtons('variants', session = session, disabledChoices = "Variant B")
      
    }
      

  }, ignoreInit = T)
  
  #observeEvent()
  
  
  ## eff data for plot ----
  # for now, generate this data in-app
  eff_data <- expand_grid(
    pop    = seq(from = 0, to = 0.1, by = 0.05),
    eff    = seq(from = 0, to = 1, by = 0.01)
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
  
  
  right_covid_per_1k <- reactive({ input$poprate * 1000 })
  right_covid_efficacy<- reactive({ round(input$effrate*100, 2) })
  protectrate         <- reactive({ round((1- (input$poprate*(1 - input$effrate)))*100,2)  })
  
  
  # text output  ---------------------------------------------------------------------------
  ## clinical data ----
  output$oddsratio <- renderText({ 
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
  ## efficacies ----
  output$right_poprate <- renderText({
    paste0(
      # "<b><font color=\"#41AB5D\" size=2>",
      # "Covid Infection Rate:<br>",
      # "</b></font>",
      "<b><font color=\"#000000\" size=6>",
      right_covid_per_1k(), " per ", "1,000",
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
      "<b><font color=\"#41AB5D\" size=8>",
      protectrate(), "%",
      "</b></font>"
    )
  })
  math_eq <- reactive({
    paste0(
      #"<b><font color=\"#000\" size=6", # can't use with renderUI??
      round(protectrate()/100,4), " = 1 - (",round(input$poprate,3),"*(1 - ",
           round(input$effrate,3),"))"
      #"</b></font>"
                 )
  })
  output$math <- renderUI({
    withMathJax(helpText(math_eq()))
  })
  
  
  # data work -------------------------------------------------------------------------------

  data <- reactive({ withProgress(message = "Fetching the Data",
    sim_data %>%
      filter(vax_name == input$vaxname)) 
  })
  
    
  
  
  
  # graphs ----------------------------------------------------------------------------------
  
  ## placebo/vaccine graph ----
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
  



  ## rainbow curve graph ---- 
  p2 <- reactive({
    ggplot(data = eff_data, aes(x = eff, y = pop, color = eff)) +
      geom_contour_filled(aes(z = p_safe), breaks = c(0, 0.9,
                                                      0.95, 0.99, 0.995, 0.999,
                                                      1)) +
      scale_fill_viridis_d(name = "Protection",
                           option = 'plasma', direction = 1,
                           alpha = 0.9,
                           labels = c("90 - 95%", "95 - 99%", "99 - 99.5%", "99.5-99.9%", "over 99.9%")) +
      geom_vline(aes(xintercept = eff_eff()), linetype= "dotdash", alpha = 0.5) +
      geom_hline(aes(yintercept = eff_pop()), linetype = "dotdash", alpha = 0.5) + 
      geom_point(data = eff_point(), aes(x = eff, y = pop), 
                 size = 2, shape = 5, alpha=1, color = 'blue', stroke = 2) +
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
      labs(x = "Vaccine Efficacy",
           y = "Pct of Population with Covid") +
      scale_x_continuous(labels = label_percent()) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      theme(
        legend.key.size = unit(8,'mm'),
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=13),
        axis.title = element_text(size=15),
        axis.text = element_text(size=12)
        
        
      )
    
  })
    
  output$effplot <- renderPlot({p2()})

  
  output$out <- renderPrint({input$variants})
  output$out2 <- renderPrint({beta()})

} # end server ------------------------------------------------------------------------


# Run the application 


run_with_themer(shinyApp(ui = ui, server = server))


