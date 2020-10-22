suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))

# Maps --------------------------------------------------------------------
all.time = readRDS('ProcessedData/cleanTime.rds')

# could have 2 maps, one by iso code the other by actual location (long,lat)
country.iso = read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/7255fdfbf292c592b75cf5f7a19c16ea59735f74/countries_codes_and_coordinates.csv")
country.iso$Alpha.3.code = trimws(country.iso$Alpha.3.code)
all.time = all.time %>% 
  mutate(
    Country = ifelse(
      Country=="Iran",
      "Iran, Islamic Republic of",
      ifelse(Country =="Korea, South",
             "South Korea",
             ifelse(Country=="Laos",
                    "Lao People's Democratic Republic",
                    ifelse(Country=="Moldova",
                           "Moldova, Republic of",
                           ifelse(Country == "Syria",
                                  "Syrian Arab Republic",
                                  ifelse(Country=="Tanzania",
                                         "Tanzania, United Republic of",
                                         ifelse(Country=="US","United States",
                                                Country)
                                  )
                           )
                    )
             )
      )
    )
  )

# all.time.geo = merge(all.time, country.iso[,c("Country","Alpha.3.code")], by = "Country", all.x = T)
all.time.geo = right_join(all.time, country.iso[,c("Country","Alpha.3.code")], by = "Country")

all.time.geo = all.time.geo %>% 
  complete(Country, Date, Metric) 

all.time.geo = merge(all.time.geo %>% select(-Alpha.3.code),
                     country.iso[,c("Country","Alpha.3.code")], by = "Country", all.x = T) %>%
  filter(!is.na(Metric))

StartDate = min(all.time$Date)

all.time.geo = all.time.geo %>% select(-Continent) %>% filter(!is.na(Metric)) %>% 
  pivot_wider(names_from = "Metric", values_from = "Amount") %>%
  mutate(Date=as.numeric(Date-min(Date, na.rm = T)))

write.csv(all.time.geo, 'data.csv')

l <- list(color = toRGB("grey"), width = 0.5)
g <- list(
  scope = 'world',
  countrycolor = toRGB('grey'),
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'natural earth')
)

map.time = 
  all.time.geo %>%
  plot_geo() %>% 
  add_trace(z = ~Confirmed,
            color = ~Confirmed,
            frame = ~Date,
            colors = 'Blues',
            text = ~Country,
            locations = ~Alpha.3.code,
            marker = list(line = l)
  ) %>% 
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

saveRDS(map.time, 'Plots/map_time.rds')

