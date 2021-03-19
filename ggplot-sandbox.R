# sort of assumes you ran plotly sandbox for data work. 
library(ggplot2)

# ggplot(data = data, aes(x, y, color = outcome, alpha=outcome, size=outcome)) + 
#   geom_point() +
#   scale_color_manual(values = c(brewer.pal(9, "Blues")[2],
#                                 brewer.pal(9, "Purples")[8],
#                                 brewer.pal(9, "Set1")[1])) +
#   scale_alpha_manual(values = c(0.1, 1.0, 1.0)) +
#   scale_size_manual(values = c(0.5, 0.6, 1)) +
#   facet_grid(cols = vars(arm)) +
#   theme_void()
# 
# RColorBrewer::display.brewer.all(type = "div", n = 3, )
# brewer.pal(9, "Set1")[1] # negative
# brewer.pal(9, "Blues")[5] 


ggplot(data = data, aes(x, y)) + # use raster since high perferfmance and all same size
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
  
