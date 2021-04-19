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
library(htmlwidgets)
library(bsplus)
library(shinyBS)
library(lubridate)

reactlog_enable()
use_bs_tooltip() # must call once
use_bs_popover()

theme <- bslib::bs_theme(
  version = "4", bootswatch = "cosmo", 
  spacer = '0.5rem',
  enable_rounded = TRUE,
  primary = "#7C36B0"
                         )

# load data 
load("data/app-data.Rdata")

# default input values 
dflt_poprate = 100
dflt_effrate = 0.7


# UI =====================================================================================
ui = navbarPage(title = "Covid-19 Vaccine Explorer", 
                position = "static-top", selected = "Data Explorer", windowTitle = "Vaccine Explorer",
                collapsible = TRUE,
                theme = theme,

               # tags$style(type='text/css', "body{padding-top: 70px;}"),


          
  
tabPanel("Data Explorer", # PAGE1: efficacies ----------------------------------------------------------------------
     fluidPage( title = "Covid-19 Vaccine Data Explorer",
               
              bsAlert("disclaimer"),
              HTML("<h2><b>Covid-19 Vaccine Explorer</b></h2>"), 
              br(),
    
              HTML("
                   
                   <font size=4>
                   The efficacy rate is not the chance you'll be protected. Use
                   the tool below to estimate an average person's chances of protection
                   from Covid-19 based on actual clinical data.
                   </font>
                   "),
              # wellPanel(align = 'left', 
              #           style = 'background:#FFF; padding: 5px',
              # HTML(markdown::markdownToHTML(file = 'md/page1-intro.md',
              #                               fragment.only = TRUE)),
     
           br(),br(),br(),     
       
       absolutePanel(  

         align='center',
         width = '100%', height = '65px',
         top = 0, left = 0,
         style= 'background: #ffffff; opacity: 1; z-index: 10; position: sticky;
         padding: 0px; border-radius: 5px; border-color: #2c3e50; border-width: 1px',
         
         fixed = TRUE, 
         
       wellPanel(align='center',
                 style= 'background: #2c3e5075; height: 65px; border-color: #2c3e50; border-width: 1px;
                        padding-top: 12px; padding-bottom: 0px',
                 
                 radioGroupButtons(
                   'presets', label = NULL, width = '100%',
                   choices = c("Explore", "Pfizer", "Moderna", "mRNA"),
                   status = 'primary',  selected = "Moderna",
                   size = "normal", direction = 'horizontal', individual = F)
                 
                     
       )),
       
       
       br(), 
     
     htmlOutput('summary'), ## summary ----
     
     ### aux buttons and els ------
     conditionalPanel(condition = 'input.presets == "Explore"',
      helpText("Adjust the sliders or click the graph to explore your chances of protection.")                  
                      ),
     
     conditionalPanel(condition = 'input.presets != "Explore"',
     #### panel
     span( align = 'center', style = 'padding: 0px',
       h6(
       bs_button(label = "why?", button_type = 'default', button_size = 'small') %>%
            bs_attach_collapse(id_collapse = 'el_explanation'),
       bs_button(label = "variants", button_type = 'default', button_size = 'small') %>%
         bs_attach_collapse(id_collapse = 'el_variants'),
       bs_button(label = 'data', button_type = 'default', button_size = 'small') %>%
         bs_attach_collapse(id_collapse = 'el_uiclinical'),
       bs_button(label = 'math', button_type = 'default', button_size = 'small') %>%
         bs_attach_collapse(id_collapse = 'el_math')
     )), br(),
     
     #### elements
     bs_collapse(id = 'el_explanation', 
                 wellPanel(align = 'left',
                           style = 'background: #FFF; padding: 3px; border-color:#373a3c; border-width: 1px',
                 htmlOutput('explanation'))),
     bs_collapse(id = 'el_variants', 
                 wellPanel(align = 'left',
                           style = 'background: #FFF; padding: 3px; border-color:#373a3c; border-width: 1px',
                  htmlOutput('variants'))),
     bs_collapse(id = 'el_uiclinical', 
                 wellPanel(align='center',
                           style='background: #2c3e50; padding: 1px',
                 plotOutput('uiclinical', height = '210px'))),
     bs_collapse(id = 'el_math', 
                 wellPanel(align='center',
                           style='background: #F5F1F9; padding:3px',
                           
                           htmlOutput('math', container = tags$b)
                 )
     )),
     
     br(),
     
          ##  main input panels ----------------------------------------
           wellPanel( align='center', 
                       style = 'background:#F5F1F9; padding: 5px; border-width: 1px; border-color: #9954bb;
                             margin-left: 0px; margin-right: 0px; 
                             padding:0.2em; width: 100%',  
            splitLayout( 
              wellPanel( ### Cases ----
                style = 'background:#00000000; padding: 5px; border-width: 0px; border-color: #fff;
                             margin-left: 0px; margin-right: 0px; padding:0em; width: 100%',
                             
                             tags$h5(tags$b("Covid Cases"),
                                     icon("question-circle")) %>%
                               bs_embed_tooltip(title = "The rate of covid-infections in the general population",
                                                placement = "top"),
                             
                             
                             htmlOutput('right_poprate', width = 6, inline = FALSE),
              
              conditionalPanel(  
                condition = 'input.presets == "Explore"', 
                style = 'margin-top: 20px',
                
                HTML(" <button id='popratelow' type='button'
                                class='btn action-button btn-sm' 
                                style='padding: 2px; margin-right: 1%; background-color:#EBDDF9; width: 30px;
                                border-color: #9954bb'; border-width: 3px'>Low</button> "), # to make action-able, add action-button to class list
                HTML(" <button id='popratemed' type='button'
                                class='btn action-button btn-sm' 
                                style='padding: 2px; margin-right: 1%; background-color:#EBDDF9; width: 30px;
                                border-color: #9954bb'; border-width: 3px'>Med</button> "), # to make action-able, add action-button to class list
                HTML(" <button id='popratehigh' type='button'
                                class='btn action-button  btn-sm' 
                                style='padding: 2px; background-color:#EBDDF9; width: 30px;
                                border-color: #9954bb'; border-width: 3px'>High</button> "), # to make action-able, add action-button to class list
                
                
                sliderInput("poprate", 
                            label = NULL,
                            width = '90%', ticks = F,
                            min = 5, max = 600, value = dflt_poprate, step = 5))),
              
              
              
             wellPanel( ### Efficacy ----
                    style = 'background:#00000000; padding: 0px; border-width: 0px; border-color: #fff;
                             margin-left: 0px; margin-right: 0px; 
                             padding:0em; width: 100%',

                          tags$h5(tags$b("Efficacy Rate"), icon("question-circle")) %>%
                               bs_embed_tooltip(title = "The vaccine's reduction of your risk from getting covid",
                                                placement = "top"),
                             
                             
                             htmlOutput('right_effrate', width = 6 ),
                    
                    conditionalPanel(    
                      condition = 'input.presets == "Explore"',
                      style = 'margin-top: 20px',
                      
                      HTML(" <button id='effrate90' type='button'
                                class='btn action-button btn-sm' 
                                style='padding: 2px; margin-right: 1%; width: 30px; background-color:#EBDDF9;
                                border-color: #9954bb'; border-width: 3px'>90</button> "),
                      HTML(" <button id='effrate94' type='button'
                                class='btn action-button btn-sm' 
                                style='padding: 2px; margin-right: 1% ; width: 30px; background-color:#EBDDF9;
                                border-color: #9954bb'; border-width: 3px'>94</button> "), 
                      HTML(" <button id='effrate95' type='button'
                                class='btn action-button btn-sm' 
                                style='padding: 2px; width: 30px; background-color:#EBDDF9;
                                border-color: #9954bb'; border-width: 3px'>95</button> "),
                      # to make action-able, add action-button to class list
                      # color=font color
                      # 
                      
                      
                      
                      sliderInput("effrate",
                                  label = NULL,
                                  width = '90%', ticks = F, 
                                  min = 0, max = 1, value = dflt_effrate, step = 0.01)))
                  )), # end main input panel, end second element
     
     
     verticalLayout( ### Protection ----
       wellPanel(align='center',
                 style= 'background: #D9F9E5; padding: 0px; border-width: 1px; border-color: #41AB5D',
                 
                 
                 tags$h4(tags$b("Chance of Protection"), icon('question-circle')) %>%
                   bs_embed_tooltip(
                     title = "An estimate of the chance that you won't get infected with covid, once fully vaccinated"),
                 htmlOutput("center_protectrate")
       ), # end wellpanel
       
       
       span(align = 'center',
            h3(tags$b("Protection Comparison"),
               bs_button(label = icon('question'), button_type = "default", button_size = "sm") %>%
            bs_embed_popover(title = "title",
                             content = "We can estimate our chances of protection from the vaccine 
                             effiacy rate and how prevalent covid is in our communities. The numbers 
                             on the bottom of the graph correspond to the vaccine efficacy rate, and the 
                             numbers on the left to how many people per year become infected with covid.
                             The colors tell us are estimated chances of protection from covid: we want to be
                             in the green zone, where we have at least a 98% chance of staying protected
                             if fully vaccinated."))),
       
       
       plotlyOutput("effplot", height = "100%"), br(), ## rainbow curve plot ----
      HTML("<font size=2>Data Sources: Baden, Lindsey R et al. (2021), Polack, Fernando P et al. (2020), and 
           Thompson MG, Burgess JL, Naleway AL, et al (2021)</font>"),
      br(), 

       
       ## After plot text ----
       HTML(markdown::markdownToHTML(file = 'md/page1-end.md',
                                     fragment.only = TRUE
                                     
       ))
       
     
     ))), # end tab panel, fluid page              
   
tabPanel("Q+A",
         fluidPage(
          br(),
          h1("Q+A"), 
          HTML(markdown::markdownToHTML(file = 'md/page2.md',
                                        fragment.only = TRUE,
                                        options = c('latex_math', 'toc')
          ))
          
          
         
           
  )), # end fluidpage, tabPanel
         


       
tabPanel("About", # PAGE3: about ----------------------------------------------------------------------
         fluidPage( title = "About Covid-19 Vaccine Data Explorer",
          tags$br(),
          
           withMathJax(),
           
           HTML(markdown::markdownToHTML(file = 'md/about.md',
                                         fragment.only = TRUE,
                                         options = c('latex_math', 'toc')
                                         ))
           
         )) # end page2 tabpanel, fluidpage


) # end navbarpage, taglist




# SERVER =====================================================================================
server <- function(input, output, session) {
  
  # input chain ---------------------------------------------------------------------
  
  ## step0: data origin ----
  ### set the type of interaction 
  interaction1 <- "plotly_hover"
  interaction2 <- "plotly_click"
  interaction3 <- "plotly_doubleclick"
  
  origin <- reactiveValues(src = "") # start with null data source
  
  # if user clicks or double clicks on plot, it will switch to plot source
  observeEvent(event_data(interaction2), { origin$src <- "plot"}, label = "origin plot3" )
  
  observeEvent(input$effrate,    {
    #if ()
    origin$src <- "user"
    }, label = "origin effrate")
  observeEvent(input$poprate,    { origin$src <- "user"}, label = "origin poprate")
  
  ### similarly, explore, preset mode 
  mode <- reactiveValues(preset = NULL)
  observeEvent(input$presets, {
      if (input$presets == "Pfizer") {
        mode$preset <- TRUE
      }
      else if (input$presets == "Moderna") {
        mode$preset <- TRUE
      } else if (input$presets == "mRNA") {
        mode$preset <- TRUE
      } else if (input$presets == "Explore") {
        mode$preset <- FALSE
      }
  }, label = "mode_presets")
  
  observeEvent(event_data(interaction2), { mode$preset <- FALSE }, label = "mode_plot-doubleclick")
  
  ## step1: user input ----
  
  ### save click values ----
  # save event data
  d <- reactive({event_data(interaction2)}) 
  
  ### define reactive values 
  click <- reactiveValues(x = NULL, y = NULL, z = NULL) 
  
  ### update with non-null plot click
  observeEvent(event_data(interaction2), {
    
    click$x <- event_data(interaction2)$x
    click$y <- event_data(interaction2)$y
    click$z <- event_data(interaction2)$z
    
    # also resets presets to Explore to avoid confusion with clinical data
    updateRadioGroupButtons(session = session, inputId = 'presets',
                            selected = "Explore"  )
    #  this may trigger the switch, yes, what happens is the user clicks the plot, it 
    #  switches to explore mode, then it changes back input to 'user' because the switch
    #  changes the value of input$effrate and input$poprate.
    #  how to distinguish the moving of data from going to 'explore' b/c of plotclick 
    #  and how because of someone actually moving
    
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
  scaler_e <- reactive({0})
  #   reactive({
  #   case_when(
  #     input$variants[1] == "Variant A"   ~ 0.15,
  #     input$variants[1] == "Variant B"   ~ 0.10,
  #     input$variants[1] == "No Variants" ~ 0
  #   )
  # })
  # 
  scaler_p <- reactive({0})
  #   reactive({
  #   case_when(
  #     input$variants[1] == "Variant A"   ~ 0.10,
  #     input$variants[1] == "Variant B"   ~ 0.40,
  #     input$variants[1] == "No Variants" ~ 0
  #   )
  # })
    

  
  ## step3: calculate new values with scalars ----
  effrate_B <- reactive({  effrate_A() - (effrate_A()*scaler_e()) })
  poprate_B <- reactive({ poprate_A() + (poprate_A()*scaler_p()) })
  
  
  
  ## step4: take these B values into the equations. ----
  protectrate <- reactive({ 
    1 - ( poprate_B()/1000 * ( 1 - effrate_B() ))
  })
  

  ## step5: calc user-friendly values ----
  ## poprate per 1k 
  poprate_B_per1k <- reactive({ round(poprate_B())})
  effrate_B_pct   <- reactive({ round(effrate_B()*100, 2)})
  protectrate_pct <- reactive({ round(protectrate()*100,2) })
  protectrate_pct1 <- reactive({ round(protectrate()*100,1) })
  

  
  
  # preset buttons ----
  ## vaccines ----

  observeEvent(input$presets, { 
    # if (input$presets[1] == "Explore") {
    #  
    # }
    if (input$presets[1] == "Pfizer") {
      # set variants to 'no variants', disable....
      # updateRadioGroupButtons('variants', session = session,
      #                         disabledChoices = c("Variant A", "Variant B"), selected = "No Variants")

      updateSliderInput('poprate', session = session,
                        value = round(vax_data$placebo_covid_incidence[vax_data$short_name %in% "Pfizer"]))
      updateSliderInput('effrate', session = session,
                        value = round(vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"], 2))
      
    }
    if (input$presets[1] == "Moderna") {
      updateSliderInput('poprate', session = session,
                        value = round(vax_data$placebo_covid_incidence[vax_data$short_name %in% "Moderna"]))
      updateSliderInput('effrate', session = session,
                        value = round(vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"], 2))
    }
    
    if (input$presets[1] == "mRNA") {
      updateSliderInput('poprate', session = session,
                        value = round(vax_data$placebo_covid_incidence[vax_data$short_name %in% "Pfizer or Moderna"]))
      updateSliderInput('effrate', session = session,
                        value = round(vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer or Moderna"], 2))
    }
    
    
  }, label = "update from vaccine presets")
  
  
  ## explore values ----
  observeEvent(input$effrate90, {updateSliderInput('effrate', session = session, value = 0.9)})
  observeEvent(input$effrate94, {updateSliderInput('effrate', session = session, value = 0.94)})
  observeEvent(input$effrate95, {updateSliderInput('effrate', session = session, value = 0.95)})
  
  observeEvent(input$popratelow, {updateSliderInput('poprate', session = session, value = 25)})
  observeEvent(input$popratemed, {updateSliderInput('poprate', session = session, value = 100)})
  observeEvent(input$popratehigh, {updateSliderInput('poprate', session = session, value = 250)})
  
  
  
  # eff data for plot ----
  # for hypothetical point data
  eff_point <- reactive({
    tibble(
      pop = poprate_B(), 
      eff = effrate_B(), 
      p_safe = 1-(pop/1000*(1-eff))
    )
  })
  

    
  # create name of selected vaccine 
  selected_vax_name <- reactive({
    if (input$presets == "Pfizer") {
      "Pfizer"
    }
    else if (input$presets == "Moderna") {
      "Moderna"
    } else if (input$presets == "mRNA") {
      "Pfizer or Moderna"
    } else if (input$presets == "Explore") {
      "hypothetical"
    }
  })
  
  
  

  # text output  ---------------------------------------------------------------------------

  ## efficacies ----
  output$right_poprate <- renderText({
    paste0(
      "<b><font color=\"#000000\" size=5>",
      poprate_B_per1k(), " per ", "1,000",
      "</b></font></style>"
    )
  })
  
  output$right_effrate <- renderText({
    paste0(
      "<b><font color=\"#2171B5\" size=5>",
      effrate_B_pct(), "%",
      "</b></font>"
    )
  })
  
  output$center_protectrate <- renderText({
    paste0(
      "<b><font color=\"#41AB5D\" size=6>",
      protectrate_pct(), "%",
      "</b></font>"
    )
  })
  math_eq <- reactive({
    paste0( "$$", 
   "\\text{Protection} = 1 - (\\frac{\\text{Infect Rate}}{1000}*(1 - \\text{Efficacy}))$$",
   "$$",
      round(protectrate(), 4), " = 1 - (", "\\frac{", round(poprate_B()), "}{1000}","*(1 - ",
           round(effrate_B(),3),"))", '$$'
                 )
  })
  output$math <- renderUI({
    withMathJax(helpText(math_eq()))
  })
  
  
  ## summary + expl ----
  sum <- reactive({if (input$presets == "Explore") {
    paste0(
      "<font size=4>The <b><font color= \"#54278F\">", as.character(selected_vax_name()), "</font></b>",
      " vaccine would protect people from Covid-19 infections about ", "<b><font color=\"#41AB5D\">", 
      protectrate_pct1(), "%", "</font></b> of the time given the selected infection rate and efficacy rate
      below.<br><br>"
    )
  } else if (input$presets == "mRNA") {
    paste0(
      "<font size=4>The CDC conducted a trial of only <b>frontline and essential workers</b>. Vaccinated 
      participants received either the Pfizer or the Moderna vaccine.
      Since both companies' vaccines use mRNA technology, the results were reported together. <br><br>
      In the CDC's trial, the <b><font color= \"#54278F\"> mRNA vaccines </font></b>",
      " protected fully-vaccinated people from Covid-19 infections about ", "<b><font color=\"#41AB5D\">", 
      protectrate_pct1(), "%", "</font></b> of the time, even in frontline situations.<br><br>"
    )
  }
    else {
      paste0(
        "<font size=4>During clinical trials, the <b><font color= \"#54278F\">", as.character(selected_vax_name()), "</font></b>",
        " vaccine protected people from Covid-19 infections about ", "<b><font color=\"#41AB5D\">", 
        protectrate_pct1(), "%", "</font></b> of the time.<br><br>"
      )
    }
    })
  output$summary <- renderText({ 
    sum()
  })
  
  output$explanation <- renderText({
    paste0(
      "This figure is based on ", selected_vax_name(), "'s efficacy rate and how frequently
      non-vaccinated people became infected with covid during the clinical trial.
      In ", vax_data$trial_name[vax_data$short_name %in% selected_vax_name()], "'s trials, non-vaccinated people got covid at a rate equivalent
      to ", "<b><font color=\"#000000\">",
      as.character(poprate_B_per1k()), "</font>", " per ", "1,000", "</b>", " people years. However, participants with the ",
      as.character(selected_vax_name()), " vaccine got infected only ",
      "<b><font color=\"#000000\">",
      as.character(round(vax_data$treatment_covid_incidence[vax_data$short_name %in% selected_vax_name()],1)),
      "</font>", " per ", "1,000", "</b>", ", a rate that was ",
      "<font color=\"#2171B5\"><b>", round(effrate_B_pct(),1), "%", "</b></font>",
      " lower. About ", "<b><font color=\"#41AB5D\">", 
      protectrate_pct1(), "%", "</font></b>", " of the time, participants with the ",selected_vax_name(), 
      " vaccine did not test positive for covid."
    )
  })
  
  # variant text explanation --
  variant_text <- reactive({
    paste0(
      "The ", vax_data$trial_name[vax_data$short_name %in% selected_vax_name()],
      " trial ran from <b>", day(vax_data$start_date[vax_data$short_name %in% selected_vax_name()]), " ",
      month(vax_data$start_date[vax_data$short_name %in% selected_vax_name()], label = TRUE, abbr = FALSE), " ", 
      year(vax_data$start_date[vax_data$short_name %in% selected_vax_name()]),
      "</b> to <b>",
      day(vax_data$end_date[vax_data$short_name %in% selected_vax_name()]), " ",
      month(vax_data$end_date[vax_data$short_name %in% selected_vax_name()], label = TRUE, abbr = FALSE), " ", 
      year(vax_data$end_date[vax_data$short_name %in% selected_vax_name()]),
     "</b> in the United States
      and ", vax_data$n_countries[vax_data$short_name %in% selected_vax_name()]-1, " other countries. 
      Research is ongoing to understand how or if varaints affect vaccine efficacy."
    )
  })
  
  
  output$variants <- renderText({
    variant_text()
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

  texttemplate <- reactive({
    if (mode$preset) {
      NULL
    } else {
      "<b>My Point<b>"
    }
  })
  
  # textfont <- reactive({
  #   if (mode$preset) {
  #     NULL
  #   } else {
  #     list(size = 14, color = "black")
  #   }
  # })
  point_colors <- c(
    brewer.pal(9, "Oranges")[6],
    brewer.pal(5, "RdBu")[5],                
    brewer.pal(3, "RdPu")[3]
  )
  ## rainbow curve graph ---- 
  p2 <- reactive({
    ### main data ----
    plot_ly(eff_data, type = 'contour', 
            x = ~eff, y = ~pop, z = ~p_safe,
            colorscale = "Viridis", zauto = F, zmin = 0.8, zmax = 1, #scaling doesn't seem to work.
            opacity = 0.8, reversescale = F,
            colorbar = list(
              thicknessmode = 'fraction', thickness = 0.04,
              lenmode = 'fraction', len = 0.5, xpad = 0,
              tickmode = 'array', tickvals = breaks,
              tickformat = '%', tickfont = list(size=10),
              title = list(text="", font=list(size=14)) # for now, no title.
            ),
            autocontour = F, contours = list(
              type = "levels",
              start = 0.9, end = 1, size = 0.02,
              coloring = 'fill', showlabels = F, # fill or heatmap
              labelfont = list(size=12, color = 'black'),
              labelformat = '%'),
            line = list(color='black', width=0.5),
            hovertemplate = paste0(
              "<span style='color:white'><b>Protection: %{z:.1%}</b></span><br>",
              "<span style='color:lightgrey'>Covid: %{y} per 1000</span><br>",
              "<span style='color:lightgrey'>Vaccine Efficacy: %{x:%}</span>",
              "<extra></extra>"
            ),
            hoverlabel = list(
              bgcolor = RColorBrewer::brewer.pal(9, "Greys")[8],
              font = list(color='white'),
              align = 'left'
            )
    ) %>% ### clinical data ----
      add_trace(data = eff_clinical_data, type = "scatter", mode = 'markers',
                uid = "clinical_data",
                x = ~eff, y = ~pop, color = ~name, opacity = 1,
                texttemplate = paste0("<b>", as.character(eff_clinical_data$name), "</b>"),
                textposition = 'top left', textfont = list(size = 14, color=point_colors),
                marker = list(
                  size = 8, color = point_colors #c("#1F78B4", "#6A3D9A") 
                ),
                text=paste0( 
                  #"<span style='white'><b>", as.character(eff_clinical_data$name), "</span></b><br>",
                  "<b>Protection: ", as.character(round(eff_clinical_data$p_safe*100,1)),"%</b><br>",
                  "<span style='color:#F0F0F0'>Covid: ", as.character(round(eff_clinical_data$pop)), " per 1000</span><br>",
                  "<span style='color:#F0F0F0'>Vaccine Efficacy: ",
                      as.character(round(eff_clinical_data$eff*100,0)), "%</span>"),
                showlegend = FALSE, hoverinfo="text", 
                hoverlabel=list(bgcolor=~name),
                hovertemplate = NULL
      ) %>% ### user point ----
      add_trace(data = eff_point(), type = "scatter", mode = 'markers',
                uid = "user_point", visible = TRUE,
                x = ~eff, y = ~pop, opacity = 1,
                # texttemplate = "<b>My Point<b>", 
                # textposition = 'top middle', textfont = list(size = 14, color = "black"),
                marker = list(
                  size = 12, color = "black", symbol = 'circle-open',
                  line = list(width=4)), 
                text=paste0(
                  "<b>Protection: ", as.character(round(eff_point()$p_safe*100,1)), "%</b><br>",
                  "<span style='color:lightgrey'>Covid: ", as.character(eff_point()$pop), " per 1000</span><br>",
                  "<span style='color:lightgrey'>Vaccine Efficacy: ",
                      as.character(round(eff_point()$eff*100,0)), "%</span>"),
                showlegend = FALSE, hoverinfo="text", 
                hoverlabel=list(bgcolor=RColorBrewer::brewer.pal(9, "Greys")[8]),
                hovertemplate = NULL
      ) %>% ### layout ----
      layout( 
        font = list(family="Arial"),
        dragmode = FALSE, # disable click/drag
        uniformtext = list(mode='hide', minsize=8),
        title = list(
          text = "Lighter colors indicate better chances of protection",
          font = list(size=14),
          pad = list(t=1,r=0,b=1,l=0)
        ),
        height = 400,
        margin = list(t=40,r=2,b=20,l=10),
        paper_bgcolor = "", plot_bgcolor = "",
        xaxis = list(
          title = list(
            text = "Vaccine Efficacy",
            font = list(size=15),
            standoff = 12),
          tickmode = 'linear', tick0 = 0, dtick = 0.2,
          tickformat = "%",
          showline = FALSE, showgrid = FALSE
        ),
        yaxis = list(
          title = list(
            text = "Covid Cases per 1,000",
            font = list(size=15),
            standoff = 4),
          showline = FALSE, showgrid = FALSE
        )
      ) %>%
      config(displayModeBar = FALSE) %>%
      onRender("function(el, x)
           {Plotly.d3.select('.cursor-crosshair').style('cursor',
           'default')}")
    
  })
    
  output$effplot <- renderPlotly({p2()})

  
  
  
  ## ui clinical data bar ------------------------------------------------------------

    # note that the graphs were already generated in outside of the app, we are just selecting
  ui_plot <- eventReactive(input$presets, {
    if (input$presets[1] == "Moderna") {
      ui_plot_moderna
      }
    else if (input$presets[1] == "Pfizer") {
      ui_plot_pfizer
    }
    else if (input$presets[1] == "mRNA") {
      ui_plot_cdc
    }
    else {
      NULL
    }
})


  output$uiclinical <-  renderPlot({ui_plot()})
  

  # bs alert ----
  createAlert(session = session, anchorId = 'disclaimer', title = "Welcome", dismiss = TRUE, style = 'info',
              content = "This is an early development version of the app. Please do write me with 
                        any constructive feedback, new feature requests, or if something isn't working.
                        Contact info is in <b>About</b> tab. "
              )

} # end server ------------------------------------------------------------------------


# Run the application 


#bslib::run_with_themer(
  shinyApp(ui = ui, server = server, options = list("launch.browswer" = TRUE))
#)


