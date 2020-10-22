suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plotly))

cat("Making plots per day over time...")

all.per.day = readRDS("ProcessedData/allPerDay.rds")

# deaths per day over time ------------------------------------------------
d.daytime = ggplotly(
  all.per.day %>% filter(Metric == "Deaths") %>% 
    ggplot(aes(x = Date, y = Amount,
               color = Continent, group = Country))+
    geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
    ylab("Number of deaths per day over time")+ggtitle("Number of deaths per day over time")
) %>% plotly_build()
saveRDS(d.daytime, 'Plots/d_daytime.rds')


# cases per day over time -------------------------------------------------
conf.daytime = ggplotly(
  all.per.day %>% filter(Metric == "Confirmed") %>% 
    ggplot(aes(x = Date, y = Amount,
               color = Continent, group = Country))+
    geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
    ylab("Cumulative number of confirmed cases")+
    ggtitle("Cumulative number of confirmed cases over time")
) %>% plotly_build()
saveRDS(conf.daytime, 'Plots/conf_daytime.rds')


# recoveries per day over time --------------------------------------------
rec.daytime = ggplotly(
  all.per.day %>% filter(Metric == "Recovered") %>% 
    ggplot(aes(x = Date, y = Amount,
               color = Continent, group = Country))+
    geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
    ylab("Cumulative number of Recovered cases")+ggtitle("Cumulative number of recovered cases over time")
) %>% plotly_build()
saveRDS(rec.daytime, 'Plots/rec_daytime.rds')

