library(plotly)
library(dplyr)

data = read.csv('https://github.com/lc5415/COVID19/raw/master/data.csv')


l <- list(color = toRGB("grey"), width = 0.5)

g <- list(
  scope = 'world',
  countrycolor = toRGB('grey'),
  showframe = T,
  showcoastlines = TRUE,
  projection = list(type = 'natural earth')
)

map.time = data %>%
  plot_geo() %>% 
  add_trace(z = ~Confirmed, color = ~Confirmed, frame = ~Date, colors = 'Blues',
            text = ~Country, locations = ~Alpha.3.code, marker = list(line = l)) %>% 
  colorbar(title = 'Confirmed') %>%
  layout(
    title = 'Number of confirmed cases over time',
    geo = g
  ) %>% 
  animation_opts(redraw = F) %>%
  animation_slider(
    currentvalue = list(
      prefix = paste0("Days from ",
                      format(StartDate, "%B %dnd"),": "))) %>%
  plotly_build()

map.time
