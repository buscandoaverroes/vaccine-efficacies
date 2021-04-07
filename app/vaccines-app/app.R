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
library(gghighlight)
library(ggrepel)

reactlog_enable()

theme <- bslib::bs_theme(
  version = 4, bootswatch = "cosmo",
  spacer = '1rem',
  enable_rounded = TRUE,
  primary = "#7C36B0"
                         )

# load data 
load("data/app-data.Rdata")

# default input values 
dflt_poprate = 100
dflt_effrate = 0.7


# UI =====================================================================================
ui = navbarPage(title = NULL, 
                position = "fixed-top",
                collapsible = TRUE,
                theme = theme,
                header = list(tags$br(), tags$br(), tags$br()),

# Application title

                
  
tabPanel("Data Explorer", # PAGE1: efficacies ----------------------------------------------------------------------
     fluidPage( title = "Covid-19 Vaccine Data Explorer",
                
                
                HTML(markdown::markdownToHTML(file = 'md/page1-intro.md',
                                              fragment.only = TRUE
                                              
                )),
     

       
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
                 
            conditionalPanel(
              condition = 'input.presets == "Explore Own"',
              
              radioGroupButtons(
                'variants', label = NULL, disabled = FALSE,
                choices = c("Variant A", "Variant B", "No Variants"),
                status = 'primary',  selected = "No Variants",
                size = "sm", direction = 'horizontal', individual = FALSE
              )
            ),

       conditionalPanel( ## ui clinical data plot -----------------------------------
              condition = 'input.presets != "Explore Own"',
              
            
                  align='center',
                  style= 'background: #2c3e50',
                  
              plotOutput('uiclinical', height = '150px')
         
       )),
       

       splitLayout( ##  main input panels ----------------------------------------
                    wellPanel( align='center', 

                               tags$h5(tags$b("Covid Infections")),
                               tags$body(("in population")),

                 conditionalPanel(  
                   condition = 'input.presets == "Explore Own"',
                   
                   sliderInput("poprate", 
                               label = NULL,
                                           width = '100%', ticks = F,
                                           min = 1, max = 200, value = dflt_poprate, step = 1)),
                               htmlOutput('right_poprate', width = 6)
                    ),  # end first element of splitpanel
                    
                 wellPanel( align='center',
                  tags$h5(tags$b("Efficacy Rate")),
                  tags$body(("Risk reduction")),
                  
                  conditionalPanel(    
                    condition = 'input.presets == "Explore Own"',
                    
                   sliderInput("effrate",
                               label = NULL,
                               width = '100%', ticks = F, 
                               min = 0, max = 1, value = dflt_effrate, step = 0.01)),
                   htmlOutput('right_effrate', width = 6 )
                    )), # end main input panel, end second element
       

       verticalLayout(  ## math ----
        conditionalPanel(
          condition = 'input.showmath',
          
          wellPanel(align='center',
                    style='background: #F6F1FB',
                    
                    htmlOutput('math', container = tags$b)
          )
        ),
         wellPanel(align='center', ## protection rate ----
                   style= 'background: #D9F9E5',
                   
                   
                   tags$h3(tags$b("Estimated Chance of Protection")),
                   htmlOutput("center_protectrate")
         ), # end wellpanel
        wellPanel( align = 'center',
                   style = 'background: #FFF',
         plotOutput("effplot", click = "plot_click")), ## rainbow curve plot ----
        tags$source("Sources: Baden, Lindsey R et al. (2021) and Polack, Fernando P et al. (2020)")
       ),
       
       ## After plot text ----
       HTML(markdown::markdownToHTML(file = 'md/page1-end.md',
                                     fragment.only = TRUE
                                     
       ))
       

     )), # end tab panel, fluid page              
          
         


       
tabPanel("About", # PAGE2: about ----------------------------------------------------------------------
         fluidPage( title = "About Covid-19 Vaccine Data Explorer",
          tags$br(),
          
           withMathJax(),
           
           HTML(markdown::markdownToHTML(file = 'md/about.md',
                                         fragment.only = TRUE,
                                         options = c('latex_math')
                                         ))
           
         )) # end page2 tabpanel, fluidpage


) # end navbarpage, taglist




# SERVER =====================================================================================
server <- function(input, output, session) {
  
  # input chain ---------------------------------------------------------------------
  
  ## step0: data origin ----
  origin <- reactiveValues(src = NULL) # start with null data source
  observeEvent(input$plot_click, { origin$src <- "plot"}, label = "origin plot" )
  observeEvent(input$effrate,    { origin$src <- "user"}, label = "origin effrate")
  observeEvent(input$poprate,    { origin$src <- "user"}, label = "origin poprate")

  ## step1: user input ----
  
  ### save click values ----
  ### (idk why plot input auto-nullifies after ~0.5 seconds...)
  
  
  ### define reactive values 
  click <- reactiveValues(x = NULL, y = NULL) 
  
  ### update with non-null plot click
  observeEvent(input$plot_click, {
    
    click$x <- input$plot_click$x
    click$y <- input$plot_click$y
    
    # also resets presets to explore own to avoid confusion with clinical data 
    updateRadioGroupButtons(session = session, inputId = 'presets',
                            selected = "Explore Own" )
    
  }, ignoreNULL = TRUE, label = 'preseve last plot click') 
  
  
  ## calculate values based on origin source above 
  effrate_A <- reactive({
    if (origin$src == "user") {
      input$effrate
    } 
    else {
      click$x
    }
  })
  
  
  poprate_A <- reactive({
    if (origin$src == "user") {
      input$poprate
    } 
    else {
      click$y
    }
  })


  ## step2: multiplier ----
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
  effrate_B <- reactive({  effrate_A() - (effrate_A()*scaler_e()) })
  
  poprate_B <- reactive({ poprate_A() + (poprate_A()*scaler_p()) })
  
  
  
  ## step4: take these B values into the equations. ----
  protectrate <- reactive({ 
    1 - ( poprate_B()/1000 * ( 1 - effrate_B() ))
  })
  

  ## step5: calculate user-friendly values ----
  ## poprate per 1k 
  poprate_B_per1k <- reactive({ round(poprate_B())})
  effrate_B_pct   <- reactive({ round(effrate_B()*100, 2)})
  protectrate_pct <- reactive({ round(protectrate()*100,2) })
  
  
  # vaccine buttons ----

  observeEvent(input$presets, { 
    # if (input$presets[1] == "Explore Own") {
    #  
    # }
    if (input$presets[1] == "Pfizer") {
      # set variants to 'no variants', disable....
      # updateRadioGroupButtons('variants', session = session,
      #                         disabledChoices = c("Variant A", "Variant B"), selected = "No Variants")

      updateSliderInput('poprate', session = session,
                        value = vax_data$placebo_covid_incidence[vax_data$short_name %in% "Pfizer"])
      updateSliderInput('effrate', session = session,
                        value = vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"])
      
    }
    if (input$presets[1] == "Moderna") {
      updateSliderInput('poprate', session = session,
                        value = vax_data$placebo_covid_incidence[vax_data$short_name %in% "Moderna"])
      updateSliderInput('effrate', session = session,
                        value = vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"])
    }
    
    
  }, label = "update from vaccine presets")
  

  
  # eff data for plot ----
  # for hypothetical point data
  eff_point <- reactive({
    tibble(
      pop = poprate_B(), 
      eff = effrate_B(), 
      p_safe = 1-(pop/1000*(1-eff))
    )
  })
  

    
  


  
  

  # text output  ---------------------------------------------------------------------------

  ## efficacies ----
  output$right_poprate <- renderText({
    paste0(
      "<b><font color=\"#000000\" size=5>",
      poprate_B_per1k(), " per ", "1,000",
      "</b></font>"
    )
  })
  
  output$right_effrate <- renderText({
    paste0(
      "<b><font color=\"#000000\" size=5>",
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
    paste0( "$$", 
      round(protectrate(), 4), " = 1 - (", "\\frac{", round(poprate_B()), "}{1000}","*(1 - ",
           round(effrate_B(),3),"))", '$$'
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
  
  
  
  # graphs ----------------------------------------------------------------------------------

  ## rainbow curve graph ---- 
  p2 <- reactive({
    ggplot(data = eff_data, aes(x = eff, y = pop)) +
      geom_contour_filled(aes(z = p_safe), stat = "contour_filled",
                          breaks = breaks
      ) +
     # labs(level = break_lvls ) +
      scale_fill_viridis_d(name = "Protection",
                           option = 'plasma', direction = 1,
                           begin = 0.28, end = 0.90,
                           alpha = 1,
                           labels = break_labs,
                           aesthetics = "fill", 
                           guide = guide_colorsteps(show.limits =TRUE,
                                                    direction = 'horizontal',
                                                    barheight = unit(6,'mm'),
                                                    barwidth  = unit(60,'mm'),
                                                    label = TRUE, 
                                                    label.position = 'top',
                                                    title.position = 'left',
                                                    title = paste0("Protection", '\n', "Chances (%)")
                           )) +
      # [user point stuff]
      geom_vline(aes(xintercept = effrate_B()), linetype= "dotdash", alpha = 0.5) +
      geom_hline(aes(yintercept = poprate_B()), linetype = "dotdash", alpha = 0.5) +
      geom_point(data = eff_point(), aes(x = eff, y = pop),
                 size = 4, shape = 4, alpha=1, color = 'black', stroke = 4) +
      # {Pfizer data}
      geom_vline(aes(xintercept = vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"]),
                 linetype= "solid", alpha = 0.3) +
      geom_hline(aes(yintercept = vax_data$placebo_covid_incidence[vax_data$short_name %in% "Pfizer"]),
                 linetype = "solid", alpha = 0.3) +
      # {Moderna data}
      geom_vline(aes(xintercept = vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"]),
                 linetype= "dotted", alpha = 0.4) +
      geom_hline(aes(yintercept = vax_data$placebo_covid_incidence[vax_data$short_name %in% "Moderna"]),
                 linetype = "dotted", alpha = 0.4) +
      # {{point}}
      geom_point(data = eff_clinical_data,
                 aes(x = eff, y = pop, color = name),
                 size = 2, shape = 20, alpha=1, stroke = 2, show.legend = NA) +
      scale_color_brewer(palette = "Set1", aesthetics = "colour",
                         guide = 'none') + # remove guide in legend
      labs(x = "Vaccine Efficacy",
           y = "Covid Cases per 1,000",
           title = "Protection Chance", 
           subtitle = paste0("Yellow areas indicate the best\n", "chances of covid protection")) +
      scale_x_continuous(labels = label_percent()) +
      scale_y_continuous() +
      geom_label_repel(data = eff_clinical_data,
                       aes(label = paste0(name)), # ' protection: ', as.character(round(p_safe*100,1)), "%"
                       position = position_nudge_repel(),
                       hjust='outward', vjust = 1,
                       label.padding = 0.25, box.padding = 0.1,
                       size = 4, show.legend = F,
                       label.size = 0.1, seed = 47, min.segment.length = 10, arrow = NULL,
                       fill = "#737373", color = "#FFFFFF", alpha = 0.8) +
      geom_label_repel(data = eff_point(),
                       aes(label = paste0(as.character(round(p_safe*100,1)), "% \n",
                                          'Protection Chance')),
                       position = position_nudge_repel(), # position_nudge(y = 4, x = 0)
                       hjust='middle', direction = "y",
                       label.padding = 0.25, box.padding = 1, 
                       size = 5, show.legend = F, 
                       label.size = 0.1, seed = 47, min.segment.length = 10, arrow = NULL,
                       fill = "#525252", color = "#FFFFFF", alpha = 1) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(t=0,r=0,b=10,l=0)),
        legend.key.size = unit(5,'mm'),
        legend.margin = margin(t=0,r=0,b=0,l=0),
        legend.position = 'top',
        legend.title.align = 0.5,
        legend.spacing.x = unit(2,'mm'),
        legend.spacing.y = unit(2,'mm'),
        legend.direction = 'horizontal',
        legend.key = element_rect(linetype = 'solid', fill = 'white', color = "#525252", size = 0.5),
        legend.title = element_text(size=13, face = "bold"),
        legend.text = element_text(size=12),
        axis.ticks.length = unit(0,'mm'),
        plot.margin = margin(t=0,r=0,b=0,l=0),
        panel.grid = element_blank(), # how to reduce space between grid/axis labels and plot area?
        axis.title = element_text(size=15),
        axis.text = element_text(size=12,margin = margin(t=0,r=0,b=0,l=0)) # is inside another element??
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
#)


