suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(leaflet))
library(ezplot) ## for nice and easy to read big numbers (millions) etc)
library(htmlwidgets)

## load data
cumulative = readRDS("ProcessedData/cumulative.rds")
countries = readRDS("ProcessedData/countries_map.rds")

## For every location pick only the data for the latest entry
latest_dates = cumulative %>% 
  group_by(location, iso_code) %>%
  summarise(latest_date_cases = max(date[!is.na(total_cases)]),
            latest_date_deaths = max(date[!is.na(total_deaths)]))

## imputation
# cumulative = cumulative %>% 
#   arrange(location, iso_code, date) %>%
#   group_by(location, iso_code) %>%
#   summarise(date = date,
#             total_cases = ifelse(all(is.na(total_cases)),
#                                  total_cases,
#                                  na_locf(total_cases, na_remaining = "keep")),
#             total_deaths = ifelse(all(is.na(total_deaths)),
#                                   total_deaths,
#                                   na_locf(total_deaths, na_remaining = "keep")))

cumulative = cumulative %>%
  group_by(location, iso_code) %>%
  summarise(total_cases = total_cases[date==max(date)],
            total_deaths = total_deaths[date==max(date)]) %>% 
  ungroup() %>%
  filter(!is.na(total_cases))

agg = countries
## perform right join to keep all countries available in the cumulative
## dataset
agg@data = right_join(agg@data, cumulative, by = c('ISO_A3' = 'iso_code'))

## the data is paired to the polygons in nominal order hence, we need to also
## remove the polygons for which no data is available, otherwise the country matching
## would not work
agg@polygons = agg@polygons[countries@data$ISO_A3 %in% cumulative$iso_code]


# Example map
pal <- colorNumeric("Blues", NULL)
leaf_map  = leaflet(agg,
                    width = 1040,
                    height = 800,
                    options = leafletOptions(center = c(30,0),zoom=2)
)  %>% addTiles() %>%
  addPolygons(stroke = FALSE, 
              ## ^^ whether to draw the borders of polygons
              smoothFactor = 0.3,
              ## ^^ how much to smooth the border line when zooming 
              fillOpacity = 1,
              fillColor = ~pal(log10(total_cases)),
              ## which values to fill each polygon with
              ## here, make it proportional to total_cases.
              label = ~paste0(location, ": ",ez_labels(total_cases, signif = 3))
              ## ^^ add on-hover label for each country, making use of the handy 
              ## ez_labels to more easily read large numbers
  )  %>% addLegend(pal = pal,
                   values = ~log10(total_cases),
                   opacity = 1.0,
                   labFormat = labelFormat(transform = function(x) round(10^x)),
                   title = 'Total Cases'
  )

saveRDS(leaf_map, file="Plots/mapcases.rds")

# --------- DEATHS

leaf_map_deaths = leaflet(agg,
                          width = 1040,
                          height = 800,
                          options = leafletOptions(center = c(30,0),zoom=2)
)  %>% addTiles() %>%
  addPolygons(stroke = FALSE, 
              ## ^^ whether to draw the borders of polygons
              smoothFactor = 0.3,
              ## ^^ how much to smooth the border line when zooming 
              fillOpacity = 1,
              fillColor = ~pal(log10(total_deaths)),
              ## which values to fill each polygon with
              ## here, make it proportional to total_deaths.
              label = ~paste0(location, ": ",ez_labels(total_deaths, signif = 3))
              ## ^^ add on-hover label for each country, making use of the handy 
              ## ez_labels to more easily read large numbers
  )  %>% addLegend(pal = pal,
                   values = ~log10(total_deaths),
                   opacity = 1.0,
                   labFormat = labelFormat(transform = function(x) round(10^x)),
                   title = 'Total Deaths'
  )

saveRDS(leaf_map_deaths, file="Plots/mapdeaths.rds")
