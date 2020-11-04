suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(leaflet))
library(leaftime) ## for Leaflet.Timeline plugin
library(ezplot) ## for nice and easy to read big numbers (millions) etc)

## NEED TO DO IMPUTATION, 
## + FIGURE OUT HOW TO MAKE CHOROPLETH WITH TIMELINE 

## load data
cumulative = readRDS("ProcessedData/cumulative.rds")
countries = readRDS("ProcessedData/countries_map.rds")

## For every location pick only the data for the latest entry
cumulative = cumulative %>% 
  group_by(location, iso_code) %>%
  summarise(total_cases = total_cases[date==max(date)]) %>% 
  ungroup()

agg = countries
agg@data = right_join(agg@data, cumulative, by = c('ISO_A3' = 'iso_code'))
agg@polygons = agg@polygons[countries@data$ISO_A3 %in% cumulative$iso_code]


# Example map
pal <- colorNumeric("Blues", NULL)
leaflet(agg,
        width = 1040,
        height = 800,
        options = leafletOptions(center = c(30,0),
                                 zoom=2,
                                 maxBounds = list(c(-90, -180),
                                                  c(90,180)))
) %>%
  addTiles() %>%
  addPolygons(stroke = FALSE,
              smoothFactor = 0.3,
              fillOpacity = 1,
              fillColor = ~pal(log10(total_cases)),
              label = ~paste0(location, ": ",
                              ez_labels(total_cases, signif = 3))
  ) %>%
  addLegend(pal = pal,
            values = ~log10(total_cases),
            opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))
  )


