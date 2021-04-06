# uiplot-sandbox.R
# testing different versions of the uiplot outside of shinyapp
# make sure to load vax_data rdata

library(tidyverse)
library(scales)
library(plotly)
library(gghighlight) # might be useful for individual dot labels


# misc ----
# define hovertemplate



# data ============================ 
# ensure the data are in a way that are easy to graph 

# tilt long
data <- select(vax_data, short_name,
               ends_with("n_covid_pos"),
               ends_with("covid_rate"),
               ends_with("covid_rate_pct"),
               ends_with("rate10k")
               ) %>%
  pivot_longer(
    cols = c(ends_with("n_covid_pos"),
               ends_with("covid_rate"),
               ends_with("covid_rate_pct"),
               ends_with("rate10k")),
   names_to = "indicator",
   values_to = "value")  %>%
  separate(indicator, into = c("arm", "indicator"), sep = '_', extra = 'merge') %>%
  mutate(arm = stringr::str_to_title(arm)) %>% # convert to title case
  #pivot_wider( id_cols = c("short_name", "arm"), names_from = "indicator", values_from = "value") %>%
  filter(short_name == "Pfizer", indicator == "covid_rate10k" | indicator == "severe_rate10k") #  


# ggplot ====================
## v1: dodged, grouped by indicator -----
gg <- data %>%
  ggplot(., aes(indicator, value), ) +
  geom_col(aes(fill = arm), position = 'dodge') +
  scale_fill_viridis_d(
    aesthetics = "fill",
    option = "viridis",
  ) +
  scale_x_discrete(labels=c("Covid", "Severe Covid")) +
  labs(y = "Rate per 10k", fill = NULL, x = NULL) +
  theme_minimal() + 
  theme(
    axis.title.x = NULL,
    axis.title.y = element_text(size=10, margin = margin(t=0,r=8,b=0,l=0), face = 'bold'),
    axis.text.x = element_text(size=10, face = 'bold'),
    axis.text.y = element_text(size=10),
    legend.title = NULL,
    legend.text = element_text(size=10),
    legend.key.size = unit(4,"mm"),
    panel.grid = element_blank(),
  ) 
gg  


gg_p <- ggplotly(gg) # for hover...
gg_p
# works for covid rate, how to incorporate severe and mortality?


## v2: stacked, grouped by arm
gg2 <- data %>%
  ggplot(., aes(arm, value)) +
  geom_col(aes(fill = indicator), position = 'stack', width = 0.6) +
  scale_fill_viridis_d(
    aesthetics = "fill",
    option = "viridis",
    labels = c("Covid",
               "Severe Covid")
  ) +
  scale_x_discrete(labels=c("Placebo", "Vaccine")) +
  labs(y = "Rate per 10k", x = NULL, fill = NULL) +
  theme_minimal() + 
  theme(
    axis.title.x = NULL,
    axis.title.y = element_text(size=10, margin = margin(t=0,r=8,b=0,l=0), face = 'bold'),
    axis.text.x = element_text(size=10, face = 'bold'),
    axis.text.y = element_text(size=10),
    legend.title = NULL,
    legend.text = element_text(size=10),
    legend.key.size = unit(4,"mm"),
    panel.grid = element_blank(),
  ) 
gg2   
gg_p2 <- ggplotly(gg2, tooltip = 'all') 
gg_p2
ht2 <- paste0(
  "<b>%{x}</b><br>",
  "%{marker.color}: %{y:0}
  <extra></extra>"
)
gg_p2 %>%
  style(hovertemplate = ht2)



# build using plotly ==============
# stacked bar
# 
ht1 = paste0(
  "<b>%{x}</b><br>",
  "%{marker.color}: %{y:0}
  <extra></extra>"
)
plot_ly(data = data, type = 'bar') %>%
  add_trace(type = "bar",
            x= ~arm, y= ~value,
            color = ~indicator,
            hovertemplate = paste0(
              "<b>%{x}</b><br>",
              "%{marker.color}: %{y:0}
                <extra></extra>"
            ),
            marker = list(color = c("severe_rate10k" , "covid_rate10k"))
            ) %>%
   layout(
    barmode = 'stack',
    bargap = 0.6
  )



# gg stacked, no hover, with labels ======
data %>%
  ggplot(., aes(arm, value, label=value)) +
  geom_col(aes(fill = indicator), position = 'stack', width = 0.6) +
  scale_fill_viridis_d(
    aesthetics = "fill",
    option = "viridis",
    labels = c("Covid",
               "Severe Covid")
  ) +
  scale_x_discrete(labels=c("Placebo", "Vaccine")) +
  labs(y = "Rate per 10k", x = NULL, fill = NULL) +
  theme_minimal() + 
  theme(
    axis.title.x = NULL,
    axis.title.y = element_text(size=10, margin = margin(t=0,r=8,b=0,l=0), face = 'bold'),
    axis.text.x = element_text(size=10, face = 'bold'),
    axis.text.y = element_text(size=10),
    legend.title = NULL,
    legend.text = element_text(size=10),
    legend.key.size = unit(4,"mm"),
    panel.grid = element_blank(),
  ) +
  geom_label(vjust = 0.5, position = position_nudge(0,0), 
             label.padding = unit(0.6, 'mm'), label.r = unit(0.15, 'mm')
             )
gg3   

## using gghighlight -----
data %>%
  ggplot(., aes(arm, value)) +
  geom_col(aes(fill = indicator), position = 'stack', width = 0.6) +
  scale_fill_viridis_d(
    aesthetics = "fill",
    option = "viridis",
    labels = c("Covid",
               "Severe Covid")
  ) +
  scale_x_discrete(labels=c("Placebo", "Vaccine")) +
  labs(y = "Rate per 10k", x = NULL, fill = NULL) +
  theme_minimal() + 
  theme(
    axis.title.x = NULL,
    axis.title.y = element_text(size=10, margin = margin(t=0,r=8,b=0,l=0), face = 'bold'),
    axis.text.x = element_text(size=10, face = 'bold'),
    axis.text.y = element_text(size=10),
    legend.title = NULL,
    legend.text = element_text(size=10),
    legend.key.size = unit(4,"mm"),
    panel.grid = element_blank(),
  ) +
  gghighlight(value > 0) + 
  geom_label(aes(label = value),
             position = position_stack(vjust = 0.0),
            # hjust = 3, #vjust = 1,
             label.size = 0.25,
             fill = "purple", color = 'white', alpha = 0.5)

### with double stack again ----
data %>%
  ggplot(., aes(arm, value)) +
  geom_col(aes(fill = indicator), position = 'dodge', width = 0.8) +
  scale_fill_viridis_d(
    aesthetics = "fill",
    option = "viridis",
    labels = c("Covid",
               "Severe Covid")
  ) +
  scale_x_discrete(labels=c("Placebo", "Vaccine")) +
  scale_y_continuous(limits = c(0,100)) +
  labs(y = "Rate per 10k", x = NULL, fill = NULL) +
  theme_minimal() + 
  theme(
    axis.title.x = NULL,
    axis.title.y = element_text(size=10, margin = margin(t=0,r=8,b=0,l=0), face = 'bold'),
    axis.text.x = element_text(size=10, face = 'bold'),
    axis.text.y = element_text(size=10),
    legend.title = NULL,
    legend.text = element_text(size=10),
    legend.key.size = unit(4,"mm"),
    panel.grid = element_blank(),
  ) +
  gghighlight(value >= 0) + 
  geom_label(aes(label = value),
             position = position_dodge2(0.8), # this width matces colwidth above
             vjust = -0.2,
             label.size = 0.25,
             fill = "#525252", color = 'white', alpha = 0.4)
