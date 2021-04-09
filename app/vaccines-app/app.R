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
         plotlyOutput("effplot", height = "100%"), ## rainbow curve plot ----
         ), 
        tags$source("Sources: Baden, Lindsey R et al. (2021) and Polack, Fernando P et al. (2020)"),
        
        verbatimTextOutput('see')
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
  observeEvent(event_data("plotly_click"), {
    
    click$x <- event_data("plotly_click")$x
    click$y <- event_data("plotly_click")$y
    
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
  

  ## step5: calc user-friendly values ----
  ## poprate per 1k 
  poprate_B_per1k <- reactive({ round(poprate_B())})
  effrate_B_pct   <- reactive({ round(effrate_B()*100, 2)})
  protectrate_pct <- reactive({ round(protectrate()*100,2) })
  
  ## prepare hover values 
  ### 1. determine near points 
  hover_points <- reactive({
    # find points
    pts <- nearPoints(df = eff_clinical_data, coordinfo = input$plot_hover, threshold = 5, maxpoints = 1)
    
    # return values, if no values, return null
    if (nrow(pts) == 0) {
      return()
    } 
    else {
      pts
      #round(pts$p_safe*100, 3) 
    }
    
      
  }, label = 'hover points')
  
  
  
  output$see <- renderPrint({str(click)})
  
  
  
  
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
              coloring = 'fill', showlabels = T, # fill or heatmap
              labelfont = list(size=12, color = 'black'),
              labelformat = '%'),
            line = list(color='black', width=1),
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
    ) %>%
      add_trace(data = eff_clinical_data, type = "scatter", mode = 'markers',
                uid = "clinical_data",
                x = ~eff, y = ~pop, color = ~name, opacity = 1,
                marker = list(
                  size = 8, color = c("#1F78B4", "#6A3D9A")
                ),
                text=paste0( 
                  "<span style='white'><b>", as.character(eff_clinical_data$name), "</span></b><br>",
                  "<b>Protection: ", as.character(round(eff_clinical_data$p_safe*100,1)),"%",
                  "</b>"),
                showlegend = FALSE, hoverinfo="text", 
                hoverlabel=list(bgcolor=~name),
                hovertemplate = NULL
      ) %>% 
      layout(
        font = list(family="Arial"),
        dragmode = FALSE, # disable click/drag
        title = list(
          text = "<b>Protection Chances</b>",
          font = list(size=20),
          pad = list(t=2,r=0,b=2,l=0)
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


