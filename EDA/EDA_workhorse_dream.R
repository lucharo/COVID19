
library(ggplot2)
library(plotly)
library(tidyverse)
library(treemapify)
library(utils)
library(here)
library(kableExtra)
library(lubridate)
library(shiny)
library(gganimate)


if (Sys.getenv("RSTUDIO") == "1"){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))}

args = commandArgs(trailingOnly = T)
if (purrr::is_empty(args)){
  gif = F
}else{
  gif = as.logical(args[1])
}

### Initial data engineer and html tables #######


# up.to.date = read.csv(paste0(curr.dir,"NovelCOVID/covid_19_data.csv"))
up.to.date = read.csv("./NovelCOVID/covid_19_data.csv")
up.to.date$Active = up.to.date$Confirmed - up.to.date$Deaths-up.to.date$Recovered
up.to.date$ObservationDate = mdy(up.to.date$ObservationDate)
# up.to.date$Last.Update=mdy_h(up.to.date$Last.Update)

# countryToContinent = read.csv(paste0(curr.dir,"Countries-Continents.csv"))
countryToContinent = read.csv("Countries-Continents.csv")
countryToContinent = rbind(countryToContinent, 
                           cbind(Continent = c(rep("Asia",5),rep("Europe",2), "Africa"),
                                 Country = c("Hong Kong", "Mainland China", "Macau",
                                             "Taiwan","South Korea", "UK",
                                             "Czech Republic", "Burkina Faso")))
countryToContinent$Continent = as.character(countryToContinent$Continent)
countryToContinent$Country = as.character(countryToContinent$Country)

up.to.date = merge(up.to.date, countryToContinent, by.x = "Country.Region", by.y = "Country", all.x = T, sort = F)
up.to.date$Continent = ifelse(is.na(up.to.date$Continent), "Other", up.to.date$Continent)

# Up-to-date numbers by continent and by country

## Table view
print("Making tables...")
sum.table = up.to.date %>% 
  select(-c(SNo)) %>%
  filter(ObservationDate==max(ObservationDate)) %>% 
  group_by(Continent, Country.Region) %>%
  distinct() %>% # for some reason I was getting duplicated entries
  summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), #using max because data is cumulative
            Recovered = sum(Recovered), Active = sum(Active)) %>%
  arrange(-Confirmed)
# knitr::kable(sum.table %>% head(20))

continent.wise = up.to.date %>%
  select(-c(Last.Update, SNo)) %>%
  filter(ObservationDate==max(ObservationDate)) %>%
  group_by(Continent) %>%
  summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), #using max because data is cumulative
            Recovered = sum(Recovered), Active = sum(Active)) %>% arrange(-Active) %>% head(20)

saveRDS(sum.table, "ProcessedData/sumtable.rds")
saveRDS(continent.wise, "ProcessedData/continentWise.rds")


### TREEMAP CODE #####


print("Making treemaps...")
# sum.table = sum.table %>% mutate(Province.State = ifelse(Province.State == "",
#                                as.character(Country.Region), 
#                                as.character(Province.State))) 

df.plotly = sum.table
df.plotly$Continent = ifelse(is.na(df.plotly$Continent), "Other", df.plotly$Continent)

conts = df.plotly %>% group_by(Continent) %>% summarise(Deaths = sum(Deaths),
                                                        Confirmed = sum(Confirmed),
                                                        Recovered = sum(Recovered),
                                                        Active = sum(Active)) %>%
  mutate(parent = "World") %>% rename(labels = Continent)

countrs = df.plotly %>% group_by(Country.Region) %>% 
  summarise(Deaths = sum(Deaths),Confirmed = sum(Confirmed),
            Recovered = sum(Recovered),Active = sum(Active), 
            parent = unique(Continent)) %>%  rename(labels = Country.Region)

# regs = df.plotly %>% ungroup() %>% select(-Continent) %>% rename(labels = Province.State, parent = Country.Region)
# regs = regs[,c(2,3,4,5,6,1)]
world = data.frame(labels = "World", Deaths = sum(conts$Deaths),
                   Confirmed = sum(conts$Confirmed), Recovered = sum(df.plotly$Recovered),
                   Active = sum(conts$Active), parent = "")

df.plotly = rbind(conts, countrs, world)

# add filter to line below if you'd like to filter by anything
# e.g. df.plotly  %>% filter(Active > 1000)
toPlot = df.plotly
## Number of deaths
deads <- plot_ly(
  type="treemap",
  values=toPlot$Deaths,
  labels = toPlot$labels,
  parents= toPlot$parent,
  hoverinfo="label+value+percent parent+percent root"
  # textinfo="label+value+percent parent+percent entry+percent root",
  #domain=list(column=0)
)

## Number of confirmed
conf <- plot_ly(
  type="treemap",
  values=toPlot$Confirmed,
  labels = toPlot$labels,
  parents= toPlot$parent,
  hoverinfo="label+value+percent parent+percent root"
  # textinfo="label+value+percent parent+percent root"
  #domain=list(column=0)
)

## Number of recovered
rec <- plot_ly(
  type="treemap",
  values=toPlot$Recovered,
  labels = toPlot$labels,
  parents= toPlot$parent,
  hoverinfo="label+value+percent parent+percent root"
  #domain=list(column=0)
)

## Case-Fatality Rate by Country
deaths.conf <- plot_ly(
  type="treemap",
  values=round(toPlot$Deaths/toPlot$Confirmed,4)*100,
  labels = toPlot$labels,
  parents= toPlot$parent,
  hoverinfo="label+value+percent",
  hovertext = "Potato"
  #domain=list(column=0)
)

## Ratio of recovered-to-confirmed cases
rec.conf <- plot_ly(
  type="treemap",
  values=toPlot$Recovered/toPlot$Confirmed,
  labels = toPlot$labels,
  parents= toPlot$parent
  # textinfo="label+value+percent parent+percent entry+percent root",
  #domain=list(column=0)
)

htmlwidgets::saveWidget(deads,
                        file = file.path(normalizePath("ProcessedData/"),"treemapDeaths.html"),
                        selfcontained = T)
htmlwidgets::saveWidget(rec,
                        file = file.path(normalizePath("ProcessedData/"),"treemapRecovered.html"),
                        selfcontained = T)
htmlwidgets::saveWidget(conf,
                        file = file.path(normalizePath("ProcessedData/"),"treemapConfirmed.html"),
                        selfcontained = T)
htmlwidgets::saveWidget(rec.conf,
                        file = file.path(normalizePath("ProcessedData/"),"treemapRec2Conf.html"),
                        selfcontained = T)
htmlwidgets::saveWidget(deaths.conf,
                        file = file.path(normalizePath("ProcessedData/"),"treemapDeaths2Conf.html"),
                        selfcontained = T)
saveRDS(deads,"ProcessedData/treemapDeaths.rds")
saveRDS(rec,"ProcessedData/treemapRecovered.rds")
saveRDS(conf,"ProcessedData/treemapConfirmed.rds")
saveRDS(deaths.conf,"ProcessedData/treemapDeaths2Confirmed.rds")
saveRDS(rec.conf,"ProcessedData/treemapRecovered2Confirmed.rds")


### Timeline data data engineer #####

print("Making curves over time...")

# rm(list = ls()[ls() %in% "curr.dir"])
deaths.time = read.csv("NovelCOVID/time_series_covid_19_deaths.csv")
confirmed.time = read.csv("NovelCOVID/time_series_covid_19_confirmed.csv")
recovered.time = read.csv("NovelCOVID/time_series_covid_19_recovered.csv")

TimeByCountry = function(data) {
  
  location = data %>% group_by(Country.Region) %>%
    summarise(Lat = mean(Lat), Long = mean(Long)) %>% ungroup()
  
  over.country = data %>% 
    select(-c(Lat, Long, Province.State)) %>%
    group_by(Country.Region) %>% summarise_all(sum) %>% ungroup()
  
  return(merge(location,over.country, by = "Country.Region"))
}

# At this point data is by country (no continent data) and contains latitude and longitude info
deaths.time = TimeByCountry(deaths.time)
confirmed.time = TimeByCountry(confirmed.time)
recovered.time = TimeByCountry(recovered.time)

# countryToContinent = read.csv(paste0(curr.dir,"Countries-Continents.csv"))
countryToContinent = read.csv("Countries-Continents.csv")
countryToContinent = rbind(countryToContinent, 
                           cbind(Continent = c(rep("Asia",5),rep("Europe",2), "Africa"),
                                 Country = c("Hong Kong", "Mainland China", "Macau",
                                             "Taiwan","South Korea", "UK", "Czech Republic", "Burkina Faso")))
countryToContinent$Continent = as.character(countryToContinent$Continent)
countryToContinent$Country = as.character(countryToContinent$Country)

deaths.time = deaths.time %>% select(-c(Lat,Long))
confirmed.time = confirmed.time %>% select(-c(Lat,Long))
recovered.time = recovered.time %>% select(-c(Lat,Long))

deaths.time = merge(countryToContinent,deaths.time,  by.y = "Country.Region", by.x = "Country")
confirmed.time = merge(countryToContinent, confirmed.time, by.y = "Country.Region", by.x = "Country")
recovered.time = merge( countryToContinent,recovered.time, by.y = "Country.Region", by.x = "Country")

deaths.time = deaths.time %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Deaths")
confirmed.time = confirmed.time %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Confirmed")
recovered.time = recovered.time %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Recovered")

all.time = rbind(deaths.time, confirmed.time, recovered.time)

# from Sys.Date() I believe the preferred date format is YYYY-MM-DD
all.time$Date = as.Date(sapply(strsplit(all.time$Date, "X"), "[[",2), format = "%m.%d.%y")

saveRDS(all.time, "ProcessedData/cleanTime.rds")


## deathsOverTime
plt.deaths.time = ggplotly(all.time %>% filter(Metric == "Deaths") %>% 
                             ggplot(aes(x = Date, y = Amount,
                                        color = Continent, group = Country))+
                             geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
                             ylab("Cumulative number of deaths")+ggtitle("Cumulative number of deaths over time")
)

saveRDS(plt.deaths.time, "ProcessedData/DeathsOverTime.rds")

## confirmed over Time}
plt.conf.time = ggplotly(all.time %>% filter(Metric == "Confirmed") %>% 
                           ggplot(aes(x = Date, y = Amount,
                                      color = Continent, group = Country))+
                           geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
                           ylab("Cumulative number of confirmed cases")+
                           ggtitle("Cumulative number of confirmed cases over time")
)

saveRDS(plt.conf.time, "ProcessedData/ConfOverTime.rds")

## Recovered over time
plt.rec.time = ggplotly(all.time %>% filter(Metric == "Recovered") %>% 
                          ggplot(aes(x = Date, y = Amount,
                                     color = Continent, group = Country))+
                          geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
                          ylab("Cumulative number of Recovered cases")+ggtitle("Cumulative number of recovered cases over time")
)

saveRDS(plt.rec.time, "ProcessedData/RecoveredOverTime.rds")

## Cumulative number of active cases
plt.act.time = ggplotly(all.time %>% pivot_wider(names_from = Metric, values_from = Amount) %>% 
                          mutate(Active = Confirmed-Recovered-Deaths) %>%  
                          ggplot(aes(x = Date, y = Active,
                                     color = Continent, group = Country))+
                          geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
                          ylab("Number of active cases")+ggtitle("Number of active cases over time")
)

saveRDS(plt.act.time, "ProcessedData/ActiveOverTime.rds")


### Per day over time####

print("Making plots per day over time...")

# rm(list = ls()[ls() %in% "curr.dir"])
deaths.time = read.csv("NovelCOVID/time_series_covid_19_deaths.csv")
confirmed.time = read.csv("NovelCOVID/time_series_covid_19_confirmed.csv")
recovered.time = read.csv("NovelCOVID/time_series_covid_19_recovered.csv")

TimeByCountry = function(data) {
  
  location = data %>% group_by(Country.Region) %>%
    summarise(Lat = mean(Lat), Long = mean(Long)) %>% ungroup()
  
  over.country = data %>% 
    select(-c(Lat, Long, Province.State)) %>%
    group_by(Country.Region) %>% summarise_all(sum) %>% ungroup()
  
  return(merge(location,over.country, by = "Country.Region"))
}

# At this point data is by country (no continent data) and contains latitude and longitude info
deaths.time = TimeByCountry(deaths.time)
confirmed.time = TimeByCountry(confirmed.time)
recovered.time = TimeByCountry(recovered.time)

countryToContinent = read.csv("Countries-Continents.csv")
countryToContinent = rbind(countryToContinent, 
                           cbind(Continent = c(rep("Asia",5),rep("Europe",2), "Africa"),
                                 Country = c("Hong Kong", "Mainland China", "Macau",
                                             "Taiwan","South Korea", "UK", "Czech Republic", "Burkina Faso")))
countryToContinent$Continent = as.character(countryToContinent$Continent)
countryToContinent$Country = as.character(countryToContinent$Country)

deaths.time = deaths.time %>% select(-c(Lat,Long))
confirmed.time = confirmed.time %>% select(-c(Lat,Long))
recovered.time = recovered.time %>% select(-c(Lat,Long))

deaths.time = merge(countryToContinent,deaths.time,  by.y = "Country.Region", by.x = "Country")
confirmed.time = merge(countryToContinent, confirmed.time, by.y = "Country.Region", by.x = "Country")
recovered.time = merge( countryToContinent,recovered.time, by.y = "Country.Region", by.x = "Country")


## Number of cases per day
today = deaths.time %>% select(-c(Country, Continent))
yesterday = lag(deaths.time %>% select(-c(Country, Continent)))
deaths.per.day = deaths.time %>% select(Country, Continent)
deaths.per.day = cbind(deaths.per.day, (today-yesterday)[-1])

today = confirmed.time %>% select(-c(Country, Continent))
yesterday = lag(confirmed.time %>% select(-c(Country, Continent)))
confirmed.per.day = confirmed.time %>% select(Country, Continent)
confirmed.per.day = cbind(confirmed.per.day, (today-yesterday)[-1])

today = recovered.time %>% select(-c(Country, Continent))
yesterday = lag(recovered.time %>% select(-c(Country, Continent)))
recovered.per.day = recovered.time %>% select(Country, Continent)
recovered.per.day = cbind(recovered.per.day, (today-yesterday)[-1])

deaths.per.day = deaths.per.day %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Deaths")
confirmed.per.day = confirmed.per.day %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Confirmed")
recovered.per.day = recovered.per.day %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Recovered")

all.per.day = rbind(deaths.per.day, confirmed.per.day, recovered.per.day)

# from Sys.Date() I believe the preferred date format is YYYY-MM-DD
all.per.day$Date = as.Date(sapply(strsplit(all.per.day$Date, "X"), "[[",2), format = "%m.%d.%y")


##deathsPerDayOverTime}
d.daytime = ggplotly(all.per.day %>% filter(Metric == "Deaths") %>% 
                       ggplot(aes(x = Date, y = Amount,
                                  color = Continent, group = Country))+
                       geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
                       ylab("Number of deaths per day over time")+ggtitle("Number of deaths per day over time")
)
saveRDS(d.daytime, "ProcessedData/deathsPerDay.rds")

##casesPerDayOverTime}
conf.daytime = ggplotly(all.per.day %>% filter(Metric == "Confirmed") %>% 
                          ggplot(aes(x = Date, y = Amount,
                                     color = Continent, group = Country))+
                          geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
                          ylab("Cumulative number of confirmed cases")+
                          ggtitle("Cumulative number of confirmed cases over time")
)

saveRDS(conf.daytime, "ProcessedData/confPerDay.rds")


##recoveredPerDayOverTime}
rec.daytime = ggplotly(all.per.day %>% filter(Metric == "Recovered") %>% 
                         ggplot(aes(x = Date, y = Amount,
                                    color = Continent, group = Country))+
                         geom_line()+theme_minimal()+scale_color_brewer(palette = "Paired")+
                         ylab("Cumulative number of Recovered cases")+ggtitle("Cumulative number of recovered cases over time")
)
saveRDS(rec.daytime, "ProcessedData/recPerDay.rds")

### Barpltos over time ####

print("Making bar plots...")
## ideally in this bit I'd like to have the y axis being dynamics and changing 
# with the values
all.time = all.time %>% distinct()

all.timeLONG = all.time %>%
  pivot_wider(names_from = Metric, values_from = Amount) %>%
  filter(Date == max(Date)) %>% arrange(-Confirmed)

bar.over.time.gg =   all.time %>% 
  filter(Country %in% all.timeLONG$Country[1:20]) %>%
  mutate(Date=as.numeric(Date-min(Date))) %>%
  arrange(Metric) %>% #lucky confirms comes first
  ggplot(aes(x = Country,
             y = Amount,frame = Date))+
  geom_col(aes(fill = Metric),
           position = "identity", show.legend = F)+
  coord_flip()+theme_minimal()+xlab("Country")

bar.over.time = ggplotly(bar.over.time.gg) %>%
  animation_opts(redraw=F) %>%
  animation_slider(
    currentvalue = list(
      prefix = paste0("Days from ",
                      min(all.time$Date),": ")))

saveRDS(bar.over.time, "ProcessedData/StackedOverTime.rds")
# not going this way because of the tripled legend

# bar.over.time.facet = ggplotly(
#   all.time %>% 
#     filter(Country %in% all.timeLONG$Country[1:20]) %>%
#     mutate(Date=as.numeric(Date-min(Date))) %>%
#     arrange(Metric) %>% #lucky onfirms comes first
#   ggplot(aes(x = Country,
#              y = Amount,frame = Date,
#              fill = Continent))+
#   geom_col(position = "identity")+
#   coord_flip()+theme_minimal()+theme(axis.text.x = element_text(angle = 45))+
#     xlab("Country")+facet_wrap(~Metric, scales = "free_x")
# ) %>% animation_opts(redraw=F) %>%
#   animation_slider(
#     currentvalue = list(
#       prefix = paste0("Days from ",
#                       min(all.time$Date),": ")))

forSubplot = all.time %>% 
  filter(Country %in% all.timeLONG$Country[1:20]) %>%
  mutate(Date=as.numeric(Date-min(Date)))

facet1 =  forSubplot %>% filter(Metric=="Confirmed") %>% 
  plot_ly(y = ~Country, x = ~Amount, frame = ~Date,color = ~Continent,
          type = "bar",showlegend = T) %>%
  layout(yaxis = list(title = "Top 20 countries by confirmed cases"),
         xaxis = list(title = "Confirmed cases"))

facet2 =  forSubplot %>% filter(Metric=="Recovered") %>% 
  plot_ly(y = ~Country, x = ~Amount, frame = ~Date,color = ~Continent,
          type = "bar",showlegend = F) %>%
  layout(yaxis = list(title = "", showticklabels = F),
         xaxis = list(title = "Recoveries",
                      range = c(0,max(forSubplot$Amount[forSubplot$Metric == "Recovered"]))
         )) 

facet3 =  forSubplot %>% filter(Metric=="Deaths") %>% 
  plot_ly(y = ~Country, x = ~Amount, frame = ~Date,color = ~Continent,
          type = "bar",showlegend = F) %>%
  layout(yaxis = list(title = "", showticklabels = F),
         xaxis = list(title = "Deaths", range = c(0,max(forSubplot$Amount[forSubplot$Metric == "Deaths"]))))

byevent = suppressWarnings(
  subplot(facet1, facet3, facet2, 
          titleX = T, nrows = 1,
          titleY = T) %>%  animation_slider(
            currentvalue = list(
              prefix = paste0("Days from ",
                              min(all.time$Date),": ")))
)

saveRDS(byevent, "ProcessedData/BarFacet.rds")


## GIFs ########

if (gif){
  print("Making GIFS...")
  
  all.time = all.time %>% distinct()
  
  all.timeLONG = all.time %>%
    pivot_wider(names_from = Metric, values_from = Amount) %>%
    filter(Date == max(Date)) %>% arrange(-Confirmed)
  
  ## inspired by"
  # https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other/53163549
  # https://stackoverflow.com/questions/53092216/any-way-to-pause-at-specific-frames-time-points-with-transition-reveal-in-gganim
  anim_table =  all.time %>%
    filter(Country %in% all.timeLONG$Country[1:20]) %>%
    mutate(Date=as.numeric(Date-min(Date))) %>%
    arrange(Metric)
  
  anim_table_summary.conf = anim_table %>% filter(Metric == "Confirmed") %>%
    filter(Date %in% c(seq(0,20,10),seq(20,45,5),seq(40,max(Date),1))) %>%
    group_by(Date) %>% mutate(rank = min_rank(-Amount)*1) %>% ungroup()
  
  anim_table_summary.dead = anim_table %>% filter(Metric == "Deaths") %>%
    filter(Date %in% c(seq(0,20,10),seq(20,45,5),seq(40,max(Date),1))) %>%
    group_by(Date) %>% mutate(rank = min_rank(-Amount)*1) %>% ungroup()
  
  anim_table_summary.reco = anim_table %>% filter(Metric == "Recovered") %>%
    filter(Date %in% c(seq(0,20,10),seq(20,45,5),seq(40,max(Date),1))) %>%
    group_by(Date) %>% mutate(rank = min_rank(-Amount)*1) %>% ungroup()
  
  
  # anim_table_smooth = anim_table %>%
  #   filter(Metric == "Confirmed") %>%
  #   group_by(Country) %>%
  #   # Do somewhat rough interpolation for ranking
  #   # (Otherwise the ranking shifts unpleasantly fast.)
  #   complete(Date= full_seq(Date, 0.5)) %>%
  #   mutate(Amount = spline(x = Date, y = Amount, xout = Date)$y) %>%
  #   group_by(Date) %>%
  #   mutate(rank = min_rank(-Amount) * 1) %>%
  #   ungroup() %>%
  #   # Then interpolate further to quarter years for fast number ticking.
  #   # Interpolate the ranks calculated earlier.
  #   group_by(Country) %>%
  #   complete(Date = full_seq(Date, .25)) %>%
  #   mutate(Amount = spline(x = Date, y = Amount, xout = Date)$y) %>%
  #   # "approx" below for linear interpolation. "spline" has a bouncy effect.
  #   mutate(rank = approx(x = Date, y = rank, xout = Date)$y) %>%
  #   ungroup() %>%
  #   arrange(Country,Date)
  
  # anim_table_smooth$Amount = ifelse(anim_table_smooth$Amount <0,
  #                                   0, round(anim_table_smooth$Amount))
  # anim_table_smooth$Country = as.factor(anim_table_smooth$Country)
  
  StartDate = min(all.time$Date)
  
  toAnimate = function(anim_table){
    p <- ggplot(anim_table, aes(rank, group = as.factor(Country),
                                fill = as.factor(Continent),
                                color = as.factor(Continent))) +
      geom_tile(aes(y = Amount/2,
                    height = Amount,
                    width = 0.9), alpha = 0.8) +
      
      # text in x-axis (requires clip = "off" in coord_*)
      # paste(country, " ")  is a hack to make pretty spacing, since hjust > 1
      #   leads to weird artifacts in text spacing.
      geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y = Amount,
                    label = scales::comma(Amount)), hjust = 0, nudge_y = 300)  +
      
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous() +
      scale_x_reverse() +
      guides(color = FALSE) +
      labs(title='Date: {format(StartDate+closest_state %>% as.numeric %>% floor,"%B %dnd, %Y")}',
           x = "", y = "Confirmed cases", fill = "Continent", color ="") +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0, size = 22),
            axis.ticks.y = element_blank(),  # These relate to the axes post-flip
            axis.text.y  = element_blank(),  # These relate to the axes post-flip
            plot.margin = margin(t=1,r=2,b=1,l=3, "cm")) +
      transition_states(Date, transition_length = 1, state_length = 3) +
      enter_grow() +
      exit_shrink() +
      ease_aes('linear')
    
    return(p)
  }
  
  anim_save("ProcessedData/confirmed.gif",
            animate(toAnimate(anim_table_summary.conf), fps = 15,duration = 30, end_pause = 10))
  print("GIF 1 done.")
  anim_save("ProcessedData/deaths.gif",
            animate(toAnimate(anim_table_summary.dead), fps = 15,duration = 30, end_pause = 10))
  print("GIF 2 done.")
  anim_save("ProcessedData/recovered.gif",
            animate(toAnimate(anim_table_summary.reco), fps = 15,duration = 30, end_pause = 10))
  print("GIF 3 done.")
}
