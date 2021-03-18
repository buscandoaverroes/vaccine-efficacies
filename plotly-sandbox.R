# plotly-sandbox.R

library(plotly)


data <- filter(sim_data, vax_name == "Pfizer-BioNTech", arm=="covid_placebo") # arm=="covid_placebo"
plot_ly(data = data) %>%
  add_trace(type= '', x = ~x, y=~y, color=~outcome
            #frame=~arm
  )
