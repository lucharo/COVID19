suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))

cat("Making plots per day over time...")

daily = readRDS('ProcessedData/daily.rds')

## deaths over time by country
ggplot_deaths_time = daily %>% 
  select(-c(new_cases)) %>% 
  ggplot(aes(x = date,
             y = new_deaths,
             color = continent,
             # the group aesthetic tells ggplot which
             ## variable connects the dots that build each line,
             ##in this case it's the location/country variable
             group = location))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Daily number of deaths")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Daily number of deaths over time")

plotly_deaths_time_country = ggplotly(ggplot_deaths_time) %>% plotly_build()
saveRDS(plotly_deaths_time_country, 'Plots/plotly_deaths_time_country_daily.rds')


## cases over Time by country 
ggplot_cases_time = daily %>% 
  select(-c(new_deaths)) %>% 
  ggplot(aes(x = date,
             y = new_cases,
             color = continent,
             # the group aesthetic tells ggplot which
             ## variable connects the dots that build each line,
             ##in this case it's the location/country variable
             group = location))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Daily number of cases")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Daily number of cases over time")

plotly_cases_time_country = ggplotly(ggplot_cases_time) %>% plotly_build()
saveRDS(plotly_cases_time_country, 'Plots/plotly_cases_time_country_daily.rds')

## deaths over time by continent
ggplot_deaths_time_cont = daily %>% 
  select(-c(new_cases)) %>% 
  ## bit to get events by continent
  group_by(continent, date) %>%
  summarise(Deaths = sum(new_deaths, na.rm = TRUE)) %>%
  ## na.rm = TRUE ensures all values are NA are taken as 0
  ggplot(aes(x = date,
             y = Deaths,
             color = continent))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Daily number of deaths")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Daily number of deaths over time  by Continent")

plotly_deaths_time_continent = ggplotly(ggplot_deaths_time_cont) %>% plotly_build()
saveRDS(plotly_deaths_time_continent, 'Plots/plotly_deaths_time_continent_daily.rds')


## cases  over time by continent
ggplot_cases_time_cont = daily %>% 
  select(-c(new_deaths)) %>% 
  ## bit to get events by continent
  group_by(continent, date) %>%
  summarise(Cases = sum(new_cases, na.rm = TRUE)) %>%
  ## na.rm = TRUE ensures all values are NA are taken as 0
  ggplot(aes(x = date,
             y = Cases,
             color = continent))+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette = "Paired")+
  ylab("Daily number of cases")+
  xlab('Date')+
  labs(color = "Continent")+
  ggtitle("Daily number of cases over time  by Continent")

plotly_cases_time_continent = ggplotly(ggplot_cases_time_cont) %>% plotly_build()
saveRDS(plotly_cases_time_continent, 'Plots/plotly_cases_time_continent_daily.rds')


# LOG10
## log 10 for countries makes little sense in the context of this tutorial
## because there are too many lines and they overlap too much in log scale
## deaths over time by continent
plotly_deaths_time_continent_log10 = ggplotly(
  ggplot_deaths_time_cont +
    scale_y_log10()+
    ylab("Daily number of deaths (log10)")) %>%
  plotly_build()
saveRDS(plotly_deaths_time_continent_log10, 'Plots/plotly_deaths_time_continent_log10_daily.rds')

## cases  over time by continent
plotly_cases_time_continent_log10 = ggplotly(
  ggplot_cases_time_cont + 
    scale_y_log10()+
    ylab("Daily number of cases (log10)")) %>%
  plotly_build()
saveRDS(plotly_cases_time_continent_log10, 'Plots/plotly_cases_time_continent_log10_daily.rds')



