# plotly-sandbox.R

library(plotly)


data <- filter(sim_data, vax_name == "Pfizer") # arm=="covid_placebo"




  
p1 <- subplot(
  plot_ly(data = data[data$arm == "Placebo",]) %>%
    add_trace(type= 'scatter', mode='markers',
              marker=list(size=5),
              x = ~x, y=~y, #z=~outcome,
              color=~outcome) %>%
  plot_ly(data = data[data$arm == "Treatment",]) %>%
    add_trace(type= 'scatter', mode='markers',
              marker=list(size=5),
              x = ~x, y=~y, #z=~outcome,
              color=~outcome)

  ) 
p1
