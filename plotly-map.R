# plotly-map.R

library(plotly)
library(geojsonsf) 

# convert sf object to geojson 
# add project id
us2 <- us %>% 
  mutate(
    id_proj = row_number()
  ) 

# check project id
assertthat::assert_that( n_distinct(us2$id_proj) == nrow(us2)  )
  
# conver to geojson 
us_json <- sf_geojson(us2, atomise = FALSE, factors_as_string = FALSE)

us3  <- select(us2, id_proj, fips, id, population, geometry)
#json <- sf_geojson(us3, atomise = FALSE, factors_as_string = FALSE, digits = NULL) # odd, won't convert.

# geometry
g <- list(
  scope = "usa",
  projection = list(type = "albers usa"),
  showlakes = TRUE
)

plot_ly() %>% # but map won't generate?
  add_trace(
    type = "choropleth",
    geojson = us_json,
    featureidkey = "id_proj",
    locations = us$fips,
    z = us$population
  ) %>%
  layout(
    geo = g
  )
