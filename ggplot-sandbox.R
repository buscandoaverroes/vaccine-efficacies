# sort of assumes you ran plotly sandbox for data work. 
library(ggplot2)

ggplot(data = data, aes(x, y, color = outcome, alpha=outcome, size=outcome)) + 
  geom_point() +
  scale_color_manual(values = c("grey","orange","red")) +
  scale_alpha_manual(values = c(0.1, 0.9, 1.0)) +
  scale_size_manual(values = c(0.5, 0.5, 0.7)) +
  facet_grid(cols = vars(arm)) +
  theme_void()
