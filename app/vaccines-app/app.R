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
library(htmltools)
library(htmlwidgets)
library(bsplus)
library(shinyBS)
library(lubridate)
library(mapview)
library(leaflet)
library(leafsync)
library(leafgl)
library(emo)
library(colourvalues)


reactlog_enable()
use_bs_tooltip() # must call once
use_bs_popover()

theme <- bslib::bs_theme(
  version = "4",
  bootswatch = "cosmo", 
  spacer = '0.5rem',
  enable_rounded = TRUE,
  primary = "#7C36B0"
  )


# load data 
load("data/app-data.Rdata")
load("data/map-data.Rdata")


# default input values 
dflt_poprate = 100
dflt_effrate = 0.7



# UI =====================================================================================
ui = navbarPage(title = "Covid-19 Vaccine Explorer", 
                position = "static-top", selected = "Data Explorer", windowTitle = "Vaccine Explorer",
                collapsible = TRUE,
                theme = theme,

          
  
tabPanel("Data Explorer", # PAGE1: efficacies ----------------------------------------------------------------------
     fluidPage( title = "Covid-19 Vaccine Data Explorer",
               
              # import css file 
              tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "app_css.css")
              ), 
              
              
              
              
              bsAlert("disclaimer"),
              HTML("<h1><b>Covid-19 Vaccine Explorer</b></h1>"), 
              br(),
              
              HTML("
                   <font size=5>
                   The efficacy rate is not the chance you'll be protected from covid. Estimate
                   your chances below.
                   </font>
                   "), 
     
           br(),br(),
         
           wellPanel( # intro ----
             align= 'center',
             style = 'background: #2c3e5075; padding: 0px; border-width: 1px; border-color: #2c3e50;
                             margin-left: 0px; margin-right: 0px; margin-bottom:20px; margin-top:50px;
                            padding-top:15px; padding-bottom:25px; width: 100%',
             HTML("<font size=5><b>Protection Chances</b></font>")
           ),  
           
           includeHTML("www/intro.html"),
           
           HTML(paste0("

                <body>
                  <font size=4>Average protection chances depend on:</font>
              
                  <div class='container'>
                      <div>", "<font size=30>",emo::ji("microbe"),"</font>", "</div>
                      <div><b><h3>Baseline Risk:</b></h3><br>how many people around you are infected </div>",
                      "<div>", "<font size=30>", emo::ji("shield"),"</font>", "</div>
                      <div><b><h3>Vaccine Efficacy:</b></h3><br>or the reduction in baseline risk</div>",
                  "</div>
              
              </body>
                
                
                
                ")), 
           
           
           
       absolutePanel(  # pt1 ----

         align='center',
         width = '100%', height = '85px',
         top = 0, left = 0,
         style= 'background: #ffffff; opacity: 1; z-index: 100; position: static;
         padding: 0px; border-radius: 5px; border-color: #2c3e50; border-width: 1px',
         
         fixed = TRUE, 
         
       wellPanel(align='center',
                 style= 'background: #2c3e5075; height: 85px; border-color: #2c3e50; border-width: 1px;
                        padding-top:0px; padding-bottom: 0px; margin-top:50px',
                 HTML("<font size=5><b>Clinical Data</b></font>"),
                 radioGroupButtons(
                   'presets', label = NULL, width = '100%',
                   choices = c("Explore", "Pfizer", "Moderna", "mRNA"),
                   status = 'primary',  selected = "Moderna",
                   size = "normal", direction = 'horizontal', individual = F)
                 
                     
       )),
       
       
       br(),                   
     wellPanel( align='center', ## info panel ----
                style = 'background:#F5F1F9; padding: 5px; border-width: 1px; border-color: #9954bb;
                             margin-left: 0px; margin-right: 0px; 
                             width: 100%',
                
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
     ))), # end 1st panel, well panel
     
       ##  main input panels ----------------------------------------
           wellPanel( align='center', 
                       style = 'background:#F5F1F9; padding: 5px; border-width: 1px; border-color: #9954bb;
                             margin-left: 0px; margin-right: 0px; 
                             padding:0.2em; width: 100%',  
            splitLayout( 
              wellPanel( ### Cases ----
                style = 'background:#00000000; padding: 5px; border-width: 0px; border-color: #fff;
                             margin-left: 0px; margin-right: 0px; padding:0em; width: 100%',
                             
                             tags$h6(tags$b("Baseline Risk"),
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

                          tags$h6(tags$b("Efficacy Rate"), icon("question-circle")) %>%
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
                 style= 'background: #6BAED650; padding: 0px; border-width: 1px; border-color: #4292C6;
                         margin-bottom: 20px',
                 
                 
                 tags$h4(tags$b("Chance of Protection"), icon('question-circle')) %>%
                   bs_embed_tooltip(
                     title = "An estimate of the chance that you won't get infected with covid, once fully vaccinated"),
                 htmlOutput("center_protectrate")
       ), # end wellpanel
       
                   # pt2 ----
       wellPanel(  ## map ----
         align= 'center',
         style = 'background: #2c3e5075; padding: 0px; border-width: 1px; border-color: #2c3e50;
                             margin-left: 0px; margin-right: 0px; margin-bottom:0px; margin-top:50px;
                             padding-top:0em; padding-bottom: 1em; width: 100%',
         HTML("<font size=5><b>Geography</b></font>"),
         radioGroupButtons( 
           'mapProtect', label = "Vaccine Efficacy", width = '100%', 
           choiceNames = c("66%", "90%", "95%"),
           choiceValues = c(66, 90, 95),
           status = 'myclass',  selected = 90,
           size = "normal", direction = 'horizontal', individual = F
           ),
         actionBttn("simulate", label = "simulate!", size = "sm", style = "fill", color = "default")
         ), # end well panel
      
       
       HTML("<font size=3>Your protection chances depend on local infection rates. But even in the 
            worst hotspots, vaccinated people are very likely to remain protected.</font>"),
       br(),
       uiOutput('map'),
       htmlOutput('mapupdate'),
       HTML("<font size =3>
            Sources: COVID-19 Data Hub, John's Hopkins, New York Times, Oxford, World Bank 
            </font>"),
       
       br(),     
                    # pt3 ----
       wellPanel(  
                   align= 'center',
                   style = 'background: #2c3e5075; padding: 0px; border-width: 1px; border-color: #2c3e50;
                             margin-left: 0px; margin-right: 0px; margin-bottom:20px; margin-top:50px;
                            padding-top:15px; padding-bottom:25px; width: 100%',
                   HTML("<font size=5><b>Protection Comparison</b></font>"),
                   

                   ),
       HTML("<font size=3>Frontline jobs put you at higher risk, but data show that
            vaccines are still highly effective. Dots in blue bands show scenarios with
            high chances of protection if you're vaccinated.</font>"),
       
       
      # buttons
      span( align = 'center', style = 'padding: 0px',
            h6( 
       bs_button(label = "why?", button_type = 'default', button_size = 'small') %>%
         bs_attach_collapse(id_collapse = 'el_explanation2'),
       bs_button(label = "if I'm not vaccinated?", button_type = 'default', button_size = 'small') %>%
         bs_attach_collapse(id_collapse = 'el_novax'),
            )), br(),
      
      # ui for inner buttons 
      bs_collapse(id = 'el_explanation2', 
                  wellPanel(align = 'left',
                            style = 'background: #FFF; padding: 3px; border-color:#373a3c; border-width: 1px',
                            HTML("<font size=2>
                                 
                                 Moderna's and Pfizer's clinical trial data concluded that the average unvaccinated 
                                 person tested positive for covid about <b>56</b> or <b>73 per 1000</b> person-years.
                                 However, the rate for frontline workers was nearly 10 times
                                 higher: <b>504 per 1000</b> person-years. In other words, in over the course of the
                                 time equivalent of one year, the risk of unvaccinated frontline workers getting Covid-19
                                 was measured at 50%, or a 1 in 2 chance. <br><br>
                                 
                                 However, The CDC data concluded that the Pfizer and Moderna vaccines maintain 
                                 very high levels of efficacy. An efficacy rate of (<font color=\"#2171B5\"><b>90%</b></font>)
                                 for frontline workers means that these vaccines reduce frontline workers' chances of getting covid
                                 by <font color=\"#2171B5\"><b>90%</b></font> on average. Graphically, we see
                                 that the purple or Frontline dot is still in the blue:
                                 it doesn't move far to the left even though the 
                                 non-vaccinated infection rate is higher than the Average Person 
                                 dots by an order of magnitude.
                                 
                                 </font>")
                            
                            )),
      bs_collapse(id = 'el_novax', 
                  wellPanel(align = 'left',
                            style = 'background: #FFF; padding: 3px; border-color:#373a3c; border-width: 1px',
                            
                            HTML("<font size=2>
                                 
                                 (new feature coming soon!)
                                 
                                 </font>")
                            
                            )),
      
      
      
       plotlyOutput("effplot", height = "100%"), ## rainbow curve plot ----
      
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
  observeEvent(event_data(interaction2), { 
    if (input$click == "Moves point")
    origin$src <- "plot"
    }, label = "origin plot3" )
  
   # if changes the efficacy rate, or poprate it goes back to the user
  observeEvent(input$effrate,    {origin$src <- "user" }, label = "origin effrate")
  observeEvent(input$poprate,    { origin$src <- "user"}, label = "origin poprate")
  
  # if the presets are equal to vaccines then it goes to user (this is the same as mode$preset == TRUE)
  observeEvent(mode$preset, {
    if (mode$preset) {
      origin$src <- "user"
    }
  }, label =  "origin presets")
  
  
  
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
    if (input$click == "Moves point") {
    click$x <- event_data(interaction2)$x
    click$y <- event_data(interaction2)$y
    click$z <- event_data(interaction2)$z
    
    # also resets presets to Explore to avoid confusion with clinical data
    updateRadioGroupButtons(session = session, inputId = 'presets',
                            selected = "Explore"  )
    }
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
  observeEvent(input$popratehigh, {updateSliderInput('poprate', session = session, value = 400)})
  
  
  
  ## map protection ----
  mapProtectVar <- reactive({
    paste("protection", input$mapProtect, sep = "_")
    })
  

  
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
      "<b><font color=\"#41AB5D\" size=5>",  
      effrate_B_pct(), "%",
      "</b></font>"
    )
  })
  
  output$center_protectrate <- renderText({
    paste0(
      "<b><font color=\"#2171B5\" size=6>",
      protectrate_pct(), "%",
      "</b></font>"
    )
  })
  math_eq <- reactive({
    paste0( "$$", 
   "\\begin{align}
   \\text{Protection} &= 1 - (\\frac{\\text{Infect Rate}}{1000}*(1 - \\text{Efficacy})) \\\\ ",
      round(protectrate(), 4), " &= 1 - (", "\\frac{", round(poprate_B()), "}{1000}","*(1 - ",
           round(effrate_B(),3),"))\\end{align}", '$$'
                 )
  })
  output$math <- renderUI({
    withMathJax(helpText(math_eq()))
  })
  
  output$mapupdate <- renderText({
    paste0('<font size=3>
           Data: 2-week period ending on ',
           "<b>",
           day(recent_date), " ", month(recent_date, label = T), " ", year(recent_date),
           '</b></font>')
  })
  
  output$mapPrefix <- renderText({
    paste0("<font size=5 color=\"#F16913\">
           Recent Infections</b></font> and<br>
           <font size=5 color=\"#2171B5\"><b>Protection Chances if Vaccinated</b><font><br>
           
           
           ")
  })
  
  
  ## summary + expl ----
  sum <- reactive({if (input$presets == "Explore") {
    paste0(
      "<font size=4>The <b><font color= \"#54278F\">", as.character(selected_vax_name()), "</font></b>",
      " vaccine would protect people from Covid-19 infections about ", "<b><font color=\"#2171B5\">", 
      protectrate_pct1(), "%", "</font></b> of the time given the selected infection rate and efficacy rate
      below.<br><br>"
    ) 
  } else if (input$presets == "mRNA") {
    paste0(
      "<font size=4>The CDC conducted a trial of only <b>frontline and essential workers</b>. Vaccinated 
      participants received either the Pfizer or the Moderna mRNA vaccine 
      and the results were reported together. <br><br>
      In the CDC's trial, the <b><font color= \"#54278F\"> mRNA vaccines </font></b>",
      " protected fully-vaccinated people from Covid-19 infections about ", "<b><font color=\"#2171B5\">", 
      protectrate_pct1(), "%", "</font></b> of the time, even in frontline situations.<br><br>"
    )
  }
    else {
      paste0(
        "<font size=4>During clinical trials, the <b><font color= \"#54278F\">", as.character(selected_vax_name()), "</font></b>",
        " vaccine protected people from Covid-19 infections about ", "<b><font color=\"#2171B5\">", 
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
      non-vaccinated people became infected with covid at the time of the clinical trial.
      In ", vax_data$trial_name[vax_data$short_name %in% selected_vax_name()], "'s trials, non-vaccinated people got covid at a rate equivalent
      to ", "<b><font color=\"#000000\">",
      as.character(poprate_B_per1k()), "</font>", " per ", "1,000", "</b>", " people years. However, participants with the ",
      as.character(selected_vax_name()), " vaccine got infected only ",
      "<b><font color=\"#000000\">",
      as.character(round(vax_data$treatment_covid_incidence[vax_data$short_name %in% selected_vax_name()],1)),
      "</font>", " per ", "1,000", "</b>", ", a rate that was ",
      "<font color=\"#41AB5D\"><b>", round(effrate_B_pct(),1), "%", "</b></font>",
      " lower. About ", "<b><font color=\"#2171B5\">", 
      protectrate_pct1(), "%", "</font></b>", " of the time, participants with the ",selected_vax_name(), 
      " vaccine did not test positive for covid."
    )
  })
  
  # variant text explanation --
  variant_text <- reactive({
    paste0(
      "According to the CDC, data suggest that the ", vax_data$short_name[vax_data$short_name %in% selected_vax_name()],
      " vaccine should remain effective against Covid-19 variants (CDC, 7 April). ",
      "The ", vax_data$trial_name[vax_data$short_name %in% selected_vax_name()],
      " trial ran from <b>", day(vax_data$start_date[vax_data$short_name %in% selected_vax_name()]), " ",
      month(vax_data$start_date[vax_data$short_name %in% selected_vax_name()], label = TRUE, abbr = FALSE), " ", 
      year(vax_data$start_date[vax_data$short_name %in% selected_vax_name()]),
      "</b> to <b>",
      day(vax_data$end_date[vax_data$short_name %in% selected_vax_name()]), " ",
      month(vax_data$end_date[vax_data$short_name %in% selected_vax_name()], label = TRUE, abbr = FALSE), " ", 
      year(vax_data$end_date[vax_data$short_name %in% selected_vax_name()]),
     "</b> in the United States
      and ", vax_data$n_countries[vax_data$short_name %in% selected_vax_name()]-1, " other countries."
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
  
  # map -------------------------------------------------------------------------------------
  
  # title panels 
  
  # make css
   tag.map.title <- tags$style(HTML("
          .leaflet-control.map-title {
          transform: translate(0%,-150%);
          left: 90px;
          text-align: center;
          position: relative;
          padding-left: 5px;
          padding-right: 5px;
          margin: 3px;
          background-color: #FFFFFF98;
          border-radius: 5px;
          font-weight: bold;
          font-size: 14px;
          z-index: 100;
        }"))
  
  title.infections <- tags$div(
    tag.map.title, HTML(paste0("Recent Infections:<br>",
                              day(ago2wk), " ", month(ago2wk, label = T), " - ",
                              day(recent_date), " ", month(recent_date, label = T)
                               ))
  )
  
  title.protection <- reactive({
    tags$div(
      tag.map.title, HTML(paste0("Protection Chance with<br>",
                                 case_when(input$mapProtect == 66 ~ "66%",
                                           input$mapProtect == 90 ~ "90%",
                                           input$mapProtect == 95 ~ "95%"),
                                 " Effective Vaccine"))
    )
  })
    
  
  ## infections ----
  mycols.top <- colour_values(us$incidence_2wk_100k, palette = "inferno", n_summaries = 5, digits = 0)
  mycols.bottom <- reactive({colour_values(us[[mapProtectVar()]], palette = "rdylbu", n_summaries = 5, digits = 2)})
  
    

  top <- reactive({
    leaflet(data = us, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
      addGlPolygons( data = us,
        stroke = T, color = "#969696", weight = 0.2, opacity = 0.4, smoothFactor = 0,
        fillColor = mycols.top$colours, fillOpacity = 0.9,
        label = ~labs.infections, 
        labelOptions = labelOptions(textsize = 20, sticky = F, 
                                    direction = "top",
                                    offset = c(0, -7),
                                    style = list(padding = "3px 3px")),
                                            fill = T, bringToFront = T
        ) %>%
       addLegend(
        na.label = NULL, title = "<font size=2>New Cases<br>per 100,000</font>",
        colors = mycols.top$summary_colours,
        labels = mycols.top$summary_values,
        opacity = 0.4) %>%
      addControl(title.infections, position = "topleft", className = 'map-title')
  })
    
  
  ## protection ----
  bottom <- reactive({
    leaflet(data = us, options = leafletOptions(minZoom = 2, maxZoom = 10), height = 300) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(cntr_crds[1], cntr_crds[2], zoom = 3) %>%
      addGlPolygons( data = us,
        stroke = T, color = "#969696", weight = 0.2, opacity = 0.4, smoothFactor = 0,
        fillColor = mycols.bottom()$colours, 
        fillOpacity = 0.9,
        label = ~case_when(input$mapProtect == 66 ~ labs.protection66,
                            input$mapProtect == 90 ~ labs.protection90,
                            input$mapProtect == 95 ~ labs.protection95),
        labelOptions = labelOptions(textsize = 20, sticky = F, 
                                    direction = "top",
                                    offset = c(0, -7),
                                    style = list(padding = "3px 3px")),
        highlightOptions = highlightOptions(stroke = TRUE, color = "black", weight = 2, opacity = 1, 
                                            fill = T, bringToFront = T
        )
      ) %>%
      addLegend(
        na.label = NULL,
        title = "<font size=2>1-Year<br>Protection<br>Chance",
        colors = mycols.bottom()$summary_colours,
        labels = mycols.bottom()$summary_values,
        opacity = 0.4,
        labFormat = labelFormat(suffix = "%", digits = 3, transform = function(x) 100*x)) %>%
      addControl(title.protection(), position = "topleft", className = 'map-title')
  })
  
  
  ## combine map ----
  map <- eventReactive(input$simulate, {
    sync(top(), bottom(), ncol = 1)},
    ignoreNULL=FALSE, ignoreInit = FALSE, label = 'double-map')
  
  
  ## render map 
  output$map <- renderUI({map()})
  output$see <- renderLeaflet({bottom()})

  
  
  
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
  
    
  output$effplot <- renderPlotly({effplot})

  
  
  
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
              content = "This is an early development version of the app. Please write me with 
                        feedback, new feature requests, or if something isn't working."
              )
  
  # dropdown UI ----
  output$dropdown <- renderUI({
    dropdownButton(
      inputId = 'drop1', label = "Graph Info",
      circle = FALSE, status = 'default', size = 'sm', tooltip = tooltipOptions(title = NULL),
      icon = icon("gear"), right = TRUE, up = F, inline = TRUE, width = '320px',
      
      radioGroupButtons(inputId = 'click', label = "Plot Click:", justified = TRUE, width = '300px',
                        choices = c("Shows hover info", "Moves point")),
      HTML("<font size=3>Info:</font><br>
                   <font size=2> Good quality vaccines maintain high efficacy rates in both high- and low-
                  risk situations, which means that all recipients can be assured that their chances of protection
                  from covid remain high.<br>
                  Points on the right side indicate better vacine efficacy rates and point that are lower show lower
                  average risk. Points in blue bands of color indicate good chances of protection.<br>
                  The clinical data show that mRNA vaccines provide users with very high chances of protection,
                  even in high risk settings. <font>")
      
    )
  })
  

} # end server ------------------------------------------------------------------------



shinyApp(ui = ui, server = server, options = list("launch.browswer" = TRUE))


