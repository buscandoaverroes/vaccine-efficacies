# sort of assumes you ran plotly sandbox for data work. 
library(ggplot2)

ggplot(data = data, aes(x, y, color = outcome, alpha=outcome, size=outcome)) + 
  geom_point() +
  scale_color_manual(values = c(brewer.pal(9, "Blues")[2],
                                brewer.pal(9, "Purples")[8],
                                brewer.pal(9, "Set1")[1])) +
  scale_alpha_manual(values = c(0.1, 1.0, 1.0)) +
  scale_size_manual(values = c(0.5, 0.6, 1)) +
  facet_grid(cols = vars(arm)) +
  theme_void()

RColorBrewer::display.brewer.all(type = "qual")
brewer.pal(9, "Set1")[1] # negative
brewer.pal(9, "Blues")[5] 

RColorBrewer::brewer.pal.info
