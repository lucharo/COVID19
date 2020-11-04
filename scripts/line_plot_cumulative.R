suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(tidyr))

# Curves ------------------------------------------------------------------

print("Making curves over time...")

cumulative = readRDS('ProcessedData/cumulative.rds')

## deaths over time by country
ggplot_deaths_time = cumulative %>% 
  select(-c(total_cases)) %>% 
  ggplot(aes(x = date,
             y = total_deaths,
             color = continent,
             # the group aesthetic tells ggplot which
             ## variable connects the dots that build each line,
             ##in this case it's the location/country variable
             group = location))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Cumulative number of deaths")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Cumulative number of deaths over time")

plotly_deaths_time_country = ggplotly(ggplot_deaths_time) %>% plotly_build()
saveRDS(plotly_deaths_time_country, 'Plots/plotly_deaths_time_country.rds')


## cases over Time by country 
ggplot_cases_time = cumulative %>% 
  select(-c(total_deaths)) %>% 
  ggplot(aes(x = date,
             y = total_cases,
             color = continent,
             # the group aesthetic tells ggplot which
             ## variable connects the dots that build each line,
             ##in this case it's the location/country variable
             group = location))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Cumulative number of cases")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Cumulative number of cases over time")

plotly_cases_time_country = ggplotly(ggplot_cases_time) %>% plotly_build()
saveRDS(plotly_cases_time_country, 'Plots/plotly_cases_time_country.rds')

## deaths over time by continent
ggplot_deaths_time_cont = cumulative %>% 
  select(-c(total_cases)) %>% 
  ## bit to get events by continent
  group_by(continent, date) %>%
  summarise(Deaths = sum(total_deaths, na.rm = TRUE)) %>%
  ## na.rm = TRUE ensures all values are NA are taken as 0
  ggplot(aes(x = date,
             y = Deaths,
             color = continent))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Cumulative number of deaths")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Cumulative number of deaths over time  by Continent")

plotly_deaths_time_continent = ggplotly(ggplot_deaths_time_cont) %>% plotly_build()
saveRDS(plotly_deaths_time_continent, 'Plots/plotly_deaths_time_continent.rds')


## cases  over time by continent
ggplot_cases_time_cont = cumulative %>% 
  select(-c(total_deaths)) %>% 
  ## bit to get events by continent
  group_by(continent, date) %>%
  summarise(Cases = sum(total_cases, na.rm = TRUE)) %>%
  ## na.rm = TRUE ensures all values are NA are taken as 0
  ggplot(aes(x = date,
             y = Cases,
             color = continent))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Cumulative number of cases")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Cumulative number of cases over time  by Continent")

plotly_cases_time_continent = ggplotly(ggplot_cases_time_cont) %>% plotly_build()
saveRDS(plotly_cases_time_continent, 'Plots/plotly_cases_time_continent.rds')


# LOG10
## log 10 for countries makes little sense in the context of this tutorial
## because there are too many lines and they overlap too much in log scale
## deaths over time by continent
plotly_deaths_time_continent_log10 = ggplotly(
  ggplot_deaths_time_cont +
    scale_y_log10()+
    ylab("Cumulative number of deaths (log10)")) %>%
  plotly_build()
saveRDS(plotly_deaths_time_continent_log10, 'Plots/plotly_deaths_time_continent_log10.rds')

## cases  over time by continent
plotly_cases_time_continent_log10 = ggplotly(
  ggplot_cases_time_cont + 
    scale_y_log10()+
    ylab("Cumulative number of cases (log10)")) %>%
  plotly_build()
saveRDS(plotly_cases_time_continent_log10, 'Plots/plotly_cases_time_continent_log10.rds')


