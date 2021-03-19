# plotly-sandbox.R

library(plotly)


data <- filter(sim_data, vax_name == "Pfizer") # arm=="covid_placebo"




  
p1 <-  plot_ly(data = data[data$arm == "Placebo",]) %>%
    add_trace(type= 'scatter3d', 
              marker=list(size=2),
              x = ~x, y=~y, z=~outcome,
              color=~outcome) 
p2 <- plot_ly(data = data[data$arm == "Treatment",]) %>%
    add_trace(type= 'scatter3d', 
              marker=list(size=2),
              x = ~x, y=~y, z=~outcome,
              color=~outcome)

data %>%
  group_by(arm) %>%
  do(p = plot_ly(., x = ~x, y=~y, #z=~outcome,
                 color=~outcome)) %>%
  subplot(nrows = 1, shareX = FALSE)

# can't do subplot of 3d graphs...
p1
schema()
