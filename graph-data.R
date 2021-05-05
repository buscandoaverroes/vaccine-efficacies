# graph-data.R
# generates the plots for app

library(RColorBrewer)
library(plotly)
library(scales)
library(gghighlight)
library(plotly)


# import data
vax_data <- readRDS(file.path(data, "vax_data.rda"))


# clinical trial outcome data ================================================================
## outcome data ------------------------------------------------------------------------------

ind <- c("treatment_n_participants", "placebo_n_participants", 
         "placebo_n_covid_pos", "treatment_n_covid_pos", 
         "placebo_n_severe", "treatment_n_severe")

# tilt long from vax_data
vax_data_long <-
  select(vax_data, short_name, ind
          ) %>%
  pivot_longer(
    cols = ind,
    names_to = "indicator",
    values_to = "value") %>%
  separate(indicator, into = c("arm", "indicator"), sep = '_', extra = 'merge') %>%
  mutate(arm = stringr::str_to_title(arm)) # convert to title case
  # filter(indicator == "treatment_n_participants" | indicator == "placebo_n_participants" |
  #          indicator == "placebo_n_covid_pos" | indicator == "treatment_n_covid_pos" |
  #          indicator == "placebo_n_severe" | indicator == "treatment_n_severe")

## Generate ggplot objects  --------------------------------------------------------------

ui_outcome_plot <- function(name, ymax, bgcolor) {

  p <-   
  vax_data_long %>%
    filter(short_name == as.character(name) & indicator != "n_participants") %>%
    ggplot(., aes(arm, value)) +
    geom_col(aes(fill = indicator), position = 'stack', width = 0.6) +
    scale_fill_viridis_d(
      aesthetics = "fill",
      begin = 0.42, end = 0.92,
      option = "plasma", direction = 1,
      labels = c("Covid", "Severe Covid")) +
    scale_x_discrete(labels=c("No \n Vaccine", paste(as.character(name), "\n Vaccine"))) + 
    scale_y_continuous(limits = c(0,ymax)) +
    labs(y = "Cases", x = NULL, fill = NULL,
         caption = paste0("No Vaccine n = ", prettyNum(
                          vax_data$placebo_n_participants[vax_data$short_name %in% as.character(name)], big.mark = ','),
                          "; Vaccine n = ", prettyNum(
                          vax_data$treatment_n_participants[vax_data$short_name %in% as.character(name)],
                          big.mark = ','))) +
    theme_minimal() + 
    theme(
      axis.title.x = NULL,
      axis.title.y = element_text(size=14, margin = margin(t=0,r=8,b=0,l=0),
                                  face = 'bold', color = 'white'),
      axis.text.x = element_text(size=16, face = 'bold', color = 'white'),
      axis.text.y = element_text(size=10, color = 'white'),
      axis.line = element_blank(),
      legend.title = NULL,
      legend.text = element_text(size=12, colour = 'white'),
      legend.key.size = unit(5,"mm"),
      legend.position = 'top',
      legend.direction = 'horizontal',
      legend.justification = "center",
      legend.spacing.x = unit(4, 'mm'),
      legend.box.margin = margin(t=0,r=0,b=0,l=0),
      legend.box.spacing = unit(0, 'mm'),
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "#2c3e50", color = NA),
      plot.background = element_rect(fill = "#2c3e50", color = NA),
      plot.margin = margin(t=0,r=0,b=0,l=0),
      plot.caption = element_text(color = 'white', size = 10, margin = margin(t=10,r=5,b=5,l=5)),
      plot.caption.position = 'panel',
      panel.border = element_rect(fill = NA, linetype = 'dashed', size = 0)
    ) +
    gghighlight(value > 0) + 
    geom_label(aes(label = value),
               position = position_dodge2(width = 0.25), 
               label.size = 0.25, 
               fill = "#525252", color = 'white', alpha = 0.7) 
  
  p
}


### ggplot function call ----
ui_plot_pfizer   <- ui_outcome_plot("Pfizer", 250)
ui_plot_moderna  <- ui_outcome_plot("Moderna", 250)
ui_plot_cdc      <- ui_outcome_plot("Pfizer or Moderna", 250)

# generate data for rainbow plot ----

eff_data <- expand_grid(
  pop    = seq(from = 0, to = 600, by = 5),
  eff    = seq(from = 0, to = 1, by = 0.01)
) %>% mutate(
  p_safe   = 1-((pop/1000)*(1-eff))
)

# for actual clinical data ----
eff_clinical_data <- tibble(
  name = c("Everyone (Pfizer)", "Everyone (Moderna)", "Frontline Workers<br>(Pfizer/Moderna)"),
  name_abb = c("Pfz", "Mod", "mRNA"),
  pop  = c(vax_data$placebo_covid_incidence[vax_data$short_name %in% "Pfizer"],
           vax_data$placebo_covid_incidence[vax_data$short_name %in% "Moderna"],
           round(vax_data$placebo_covid_incidence[vax_data$short_name %in% "Pfizer or Moderna"])),
  eff  = c(vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer"],
           vax_data$covid_efficacy[vax_data$short_name %in% "Moderna"],
           vax_data$covid_efficacy[vax_data$short_name %in% "Pfizer or Moderna"]),
  p_safe = 1-((pop/1000)*(1-eff))
)

# store key values
breaks     <- c(0, seq(from = 0.80, to = 1.00, by = 0.05))
break_labs <- c("0"="0", "0.9"="90", "0.92"="92", "0.94"="94", "0.96"="96", "0.98"="98", "1"="")
break_lvls <- unique(base::cut(eff_data$p_safe,
                               breaks = breaks,
                               include.lowest = TRUE,
                               ordered_result = TRUE, right = TRUE))


# colors
point_colors <- c(
  brewer.pal(9, "Oranges")[6],
  brewer.pal(5, "RdBu")[5],                
  brewer.pal(3, "RdPu")[3]
)



# make plotly object ----

effplot <- plot_ly(eff_data, type = 'contour', 
                   x = ~eff, y = ~pop, z = ~p_safe,
                   colorscale = "Blues", zauto = F, zmin = 0.8, zmax = 1, #scaling doesn't seem to work.
                   opacity = 0.8, reversescale = T,
                   colorbar = list(
                     thicknessmode = 'fraction', thickness = 0.04,
                     lenmode = 'fraction', len = 0.5, xpad = 0,
                     tickmode = 'array', tickvals = breaks,
                     tickformat = '%', tickfont = list(size=10),
                     title = list(text="", font=list(size=14)) # for now, no title.
                   ),
                   autocontour = F, contours = list(
                     type = "levels",
                     start = 0.8, end = 1, size = 0.05,
                     coloring = 'fill', showlabels = F, # fill or heatmap
                     labelfont = list(size=12, color = 'black'),
                     labelformat = '%'),
                   line = list(color='black', width=0.5),
                   hovertemplate = paste0(
                     "<span style='color:white'><b>Protection: %{z:.1%}</b></span><br>",
                     "<span style='color:lightgrey'>Infections: %{y} per 1000</span><br>",
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
            "<b>Protection: ", as.character(round(eff_clinical_data$p_safe*100,1)),"%</b><br>",
            "<span style='color:#F0F0F0'>Infections: ", as.character(round(eff_clinical_data$pop)), " per 1000</span><br>",
            "<span style='color:#F0F0F0'>Vaccine Efficacy: ",
            as.character(round(eff_clinical_data$eff*100,0)), "%</span>"),
          showlegend = FALSE, hoverinfo="text", 
          hoverlabel=list(bgcolor=~name),
          hovertemplate = NULL
) %>%
layout( 
  font = list(family="Arial"),
  dragmode = FALSE, # disable click/drag
  uniformtext = list(mode='hide', minsize=8),
  title = list(
    text = "Protection Chances in<br>Fronline vs General Situations",
    font = list(size=16),
    pad = list(t=1,r=0,b=2,l=0)
  ),
  height = 400,
  margin = list(t=60,r=2,b=20,l=10),
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
      text = "Population Infections per 1,000",
      font = list(size=15),
      standoff = 4),
    showline = FALSE, showgrid = FALSE
  )
) %>%
  config(displayModeBar = FALSE) %>%
  onRender("function(el, x)
           {Plotly.d3.select('.cursor-crosshair').style('cursor',
           'default')}")


effplot

# export ----
save(
  vax_data, eff_data, effplot,
  ui_plot_moderna, ui_plot_pfizer, ui_plot_cdc, breaks, break_labs, eff_clinical_data, break_lvls,
  file = file.path(data, "app-data.Rdata")
)

### save a copy to the app directory
save(
  vax_data, eff_data, effplot,
  ui_plot_moderna, ui_plot_pfizer, ui_plot_cdc, breaks, break_labs, eff_clinical_data, break_lvls, 
  file = file.path(app, "data/app-data.Rdata")
)

