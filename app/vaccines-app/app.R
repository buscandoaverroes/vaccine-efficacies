
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

       tags$h4("Estimate Your Chances"),
       tags$body("Select a vaccine preset to explore the clinical trial data or adjust
                 the buttons and sliders to explore a hypothetical scenario."),
       
     

       
       br(), hr(),
       prettySwitch('showmath', 'Show Math', slim = T, inline = T, status = 'info'),
       wellPanel(align='center',
                 style= 'background: #2c3e50',
                 
                 radioGroupButtons(
                   'presets', label = NULL,
                   choices = c("Explore Own", "Pfizer", "Moderna"),
                   status = 'primary',  selected = "Explore Own",
                   size = "lg", direction = 'horizontal', individual = T
                 ),

                radioGroupButtons(
                  'variants', label = NULL, disabled = FALSE,
                  choices = c("Variant A", "Variant B", "No Variants"),
                  status = 'primary',  selected = "No Variants",
                  size = "sm", direction = 'horizontal', individual = FALSE
                ),

       conditionalPanel( ## ui clinical data plot -----------------------------------
              condition = 'input.presets != "Explore Own"', # this needs to be chaned over to radio (3 button)
              
            
                  align='center',
                  style= 'background: #2c3e50',
                  
              plotOutput('uiclinical', height = '150px')
         
       )),
       

       splitLayout( ##  main input panels ----------------------------------------
                    wellPanel( align='center', 

                               tags$h4(tags$b("Infection Rate")),

                 conditionalPanel(    condition = 'input.presets == "Explore Own"',
                   sliderInput("poprate", label = NULL,
                                           width = '120px', ticks = F,
                                           min = 0.001, max = 0.1, value = 0.03, step = 0.001)),
                               htmlOutput('right_poprate', width = 6)
                    ),  # end first element of splitpanel
                    wellPanel( align='center',

                               tags$h4(tags$b("Efficacy Rate")),
                               
                  conditionalPanel(    condition = 'input.presets == "Explore Own"',
                               sliderInput("effrate", label = NULL,
                                           width = '150px', ticks = F, 
                                           min = 0, max = 1, value = 0.8, step = 0.01)),
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
        wellPanel( align = 'center',
                   style = 'background: #FFF',
         plotOutput("effplot", click = clickOpts(id = "plot_click"))), ## rainbow curve plot ----
        plotOutput('see')
       ),
       
       ## After plot text ----
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
       

     )) # end tab panel, fluid page              
          
                
                


) # end navbarpage, taglist



# SERVER =====================================================================================
server <- function(input, output, session) {
  
  # input chain ---------------------------------------------------------------------
  ## step1: user input ----
  effrate_A <- reactive({input$effrate})
  poprate_A <- reactive({input$poprate})
  
  ## step2: multiplier ----
  scaler <- reactiveValues(e = 0, p = 0)
  scaler_e <- reactive({
    case_when(
      input$variants[1] == "Variant A"   ~ 0.15,
      input$variants[1] == "Variant B"   ~ 0.10,
      input$variants[1] == "No Variants" ~ 0
    )
  })
  
  scaler_p <- reactive({
    case_when(
      input$variants[1] == "Variant A"   ~ 0.10,
      input$variants[1] == "Variant B"   ~ 0.40,
      input$variants[1] == "No Variants" ~ 0
    )
  })
    

  
  ## step3: calculate new values with scalars ----
  effrate_B <- reactive({effrate_A() - (effrate_A()*scaler_e() )})
  poprate_B <- reactive({poprate_A() + (poprate_A()*scaler_p() )})
  
  ## step4: take these B values into the equations. ----
  protectrate <- reactive({
    1 - ( poprate_B() * ( 1 - effrate_B() ))
  })

  ## step5: calculate user-friendly values ----
  ## poprate per 1k 
  poprate_B_per1k <- reactive({ round(poprate_B() * 1000) })
  effrate_B_pct   <- reactive({ round(effrate_B()*100, 2)})
  protectrate_pct <- reactive({ round(protectrate()*100,2) })
  
  
  # vaccine buttons ----

  observeEvent(input$presets, { 
    if (input$presets[1] == "Explore Own") {
      # enable/disable rest of inputs 
      updateRadioGroupButtons('variants', session = session,
                              disabledChoices = NULL, selected = "No Variants")
    }
    if (input$presets[1] == "Pfizer") {
      # set variants to 'no variants', disable....
      updateRadioGroupButtons('variants', session = session,
                              disabledChoices = c("Variant A", "Variant B"), selected = "No Variants")

      updateSliderInput('poprate', session = session,
                        value = vax_data$placebo_covid_rate[vax_data$short_name %in% "Pfizer"])
      updateSliderInput('effrate', session = session,
                        value = vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"])
      
    }
    if (input$presets[1] == "Moderna") {
      updateSliderInput('poprate', session = session,
                        value = vax_data$placebo_covid_rate[vax_data$short_name %in% "Moderna"])
      updateSliderInput('effrate', session = session,
                        value = vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"])
      updateRadioGroupButtons('variants', session = session,
                              disabledChoices = c("Variant A", "Variant B"), selected = "No Variants")
      
    }
    
    
  }, label = "update from vaccine presets")
  

  
  # eff data for plot ----
  #### for now, generate this data in-app
  eff_data <- expand_grid(
    pop    = seq(from = 0, to = 0.1, by = 0.05),
    eff    = seq(from = 0, to = 1, by = 0.01)
  ) %>% mutate(
    p_safe   = 1-(pop*(1-eff))
  )
  

  
  # for hypothetical point data
  eff_point <- reactive({
    tibble(
      pop = poprate_B(), 
      eff = effrate_B(), 
      p_safe = 1-(pop*(1-eff))
    )
  })
  
  # for actual clinical data
  eff_clinical_data <- tibble(
    name = c("Pfizer", "Moderna"),
    var1 = 1,
    pop  = c(vax_data$placebo_covid_rate[vax_data$short_name %in% "Pfizer"],
             vax_data$placebo_covid_rate[vax_data$short_name %in% "Moderna"]),
    eff  = c(vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"],
             vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"]),
    p_safe = 1-(pop*(1-eff))
  )
  
  
  
  
    
  


  
  

  # text output  ---------------------------------------------------------------------------

  ## efficacies ----
  output$right_poprate <- renderText({
    paste0(
      "<b><font color=\"#000000\" size=6>",
      poprate_B_per1k(), " per ", "1,000",
      "</b></font>"
    )
  })
  
  output$right_effrate <- renderText({
    paste0(
      "<b><font color=\"#000000\" size=6>",
      effrate_B_pct(), "%",
      "</b></font>"
    )
  })
  
  output$center_protectrate <- renderText({
    paste0(
      "<b><font color=\"#41AB5D\" size=8>",
      protectrate_pct(), "%",
      "</b></font>"
    )
  })
  math_eq <- reactive({
    paste0(
      protectrate(), " = 1 - (",round(poprate_B(),3),"*(1 - ",
           round(effrate_B(),3),"))"
                 )
  })
  output$math <- renderUI({
    withMathJax(helpText(math_eq()))
  })
  
  # html styles ----
  style_infectionrate <- reactive({
    case_when(
      input$variants[1] == "No Variants" ~ paste0(""),
      input$variants[1] == "Variant A"   ~ paste0('background: #FEE6CE'),
      input$variants[1] == "Variant B"   ~ paste0('background: #FDAE6B')
    )
  })
  
  style_efficacyrate <- reactive({
    case_when(
      input$variants[1] == "No Variants" ~ paste0(""),
      input$variants[1] == "Variant A"   ~ paste0('background: #FDD0A2'),
      input$variants[1] == "Variant B"   ~ paste0('background: #FEE6CE')
    )
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
                           begin = 0.2, end = 0.95,
                           alpha = 1,
                           labels = c("90 - 95%", "95 - 99%", "99 - 99.5%", "99.5-99.9%", "over 99.9%"),
                           aesthetics = "fill") +
      geom_vline(aes(xintercept = effrate_B()), linetype= "dotdash", alpha = 0.5) +
      geom_hline(aes(yintercept = poprate_B()), linetype = "dotdash", alpha = 0.5) + 
      geom_point(data = eff_point(), aes(x = eff, y = pop), 
                 size = 4, shape = 4, alpha=1, color = 'black', stroke = 4) +
      # {Pfizer data}
      geom_vline(aes(xintercept = vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"]),
                 linetype= "solid", alpha = 0.3) +
      geom_hline(aes(yintercept = vax_data$placebo_covid_rate[vax_data$short_name %in% "Pfizer"]),
                 linetype = "solid", alpha = 0.3) + 
      # {Moderna data}
      geom_vline(aes(xintercept = vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"]),
                 linetype= "dotted", alpha = 0.4) +
      geom_hline(aes(yintercept = vax_data$placebo_covid_rate[vax_data$short_name %in% "Moderna"]),
                 linetype = "dotted", alpha = 0.4) + 
      # {{point}}
      geom_point(data = eff_clinical_data,
                 aes(x = eff, y = pop, colour = name),
                 size = 2, shape = 20, alpha=1, stroke = 2) + 
      scale_color_brewer(palette = "Set1", aesthetics = "colour",
                         name = "Vaccine") +
      guides(color=guide_legend(override.aes = list(fill=NA, stroke=NA))) +
      labs(x = "Vaccine Efficacy",
           y = "Pct of Population with Covid") +
      scale_x_continuous(labels = label_percent()) +
      scale_y_continuous(labels = label_percent()) +
      theme_minimal() +
      theme(
        legend.key.size = unit(8,'mm'),
        legend.key = element_rect(linetype = 'solid', fill = 'white', color = "#525252", size = 0.5),
        legend.title = element_text(size=15, face = "bold"),
        legend.text = element_text(size=13),
        axis.title = element_text(size=15),
        axis.text = element_text(size=12)
        
      )
    
  })
    
  output$effplot <- renderPlot({p2()})

  
  
  
  ## ui clinical data bar ------------------------------------------------------------

    # note that the graphs were already generated in outside of the app, we are just selecting
  ui_plot <- eventReactive(input$presets, {
    if (input$presets[1] == "Moderna") {
      ui_plot_moderna
      }
    else if (input$presets[1] == "Pfizer") {
      ui_plot_pfizer
      }
    else {
      NULL
    }
})


  output$uiclinical <-  renderPlot({ui_plot()})

    


} # end server ------------------------------------------------------------------------


# Run the application 


#bslib::run_with_themer(
  shinyApp(ui = ui, server = server, options = list("launch.browswer" = TRUE))
 
# )


