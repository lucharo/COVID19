suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(tidyr))

# Curves ------------------------------------------------------------------

print("Making curves over time...")

all.time = readRDS('ProcessedData/cleanTime.rds')

## deathsOverTime
plt.deaths.time = ggplotly(
  all.time %>% 
    filter(Metric == "Deaths") %>% 
    ggplot(aes(x = Date, y = Amount,
               color = Continent, group = Country))+
    geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
    ylab("Cumulative number of deaths")+ggtitle("Cumulative number of deaths over time")
) %>% plotly_build()
saveRDS(plt.deaths.time, 'Plots/plt_deaths_time.rds')


## confirmed over Time}
plt.conf.time = ggplotly(
  all.time %>% 
    filter(Metric == "Confirmed") %>% 
    ggplot(aes(x = Date, y = Amount,
               color = Continent, group = Country))+
    geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
    ylab("Cumulative number of confirmed cases")+
    ggtitle("Cumulative number of confirmed cases over time")
) %>% plotly_build()
saveRDS(plt.conf.time, 'Plots/plt_conf_time.rds')


## Recovered over time
plt.rec.time = ggplotly(
  all.time %>% 
    filter(Metric == "Recovered") %>% 
    ggplot(aes(x = Date, y = Amount,
               color = Continent, group = Country))+
    geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
    ylab("Cumulative number of Recovered cases")+ggtitle("Cumulative number of recovered cases over time")
) %>% plotly_build()
saveRDS(plt.rec.time, 'Plots/plt_rec_time.rds')


## Cumulative number of active cases
plt.act.time = ggplotly(
  all.time %>% 
    pivot_wider(names_from = Metric, values_from = Amount) %>% 
    mutate(Active = Confirmed-Recovered-Deaths) %>%  
    ggplot(aes(x = Date, y = Active,
               color = Continent, group = Country))+
    geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
    ylab("Number of active cases")+ggtitle("Number of active cases over time")
) %>% plotly_build()
saveRDS(plt.act.time, 'Plots/plt_act_time.rds')

