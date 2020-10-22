suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tidyr))
# Up to date table ----------------------------------------------------------

up.to.date = read.csv("RawData/covid_19_data.csv")
up.to.date$Active = up.to.date$Confirmed - up.to.date$Deaths-up.to.date$Recovered
up.to.date$ObservationDate = mdy(up.to.date$ObservationDate)

# countryToContinent = read.csv(paste0(curr.dir,"Countries-Continents.csv"))
countryToContinent = read.csv("UtilsData/Countries-Continents.csv")
countryToContinent = rbind(countryToContinent, 
                           cbind(Continent = c(rep("Asia",5),rep("Europe",2), "Africa"),
                                 Country = c("Hong Kong", "Mainland China", "Macau",
                                             "Taiwan","South Korea", "UK",
                                             "Czech Republic", "Burkina Faso")))
countryToContinent$Continent = as.character(countryToContinent$Continent)
countryToContinent$Country = as.character(countryToContinent$Country)

up.to.date = merge(up.to.date, countryToContinent,
                   by.x = "Country.Region",
                   by.y = "Country",
                   all.x = T,
                   sort = F)

up.to.date$Continent = ifelse(is.na(up.to.date$Continent),
                              "Other",
                              up.to.date$Continent)

saveRDS(up.to.date, "ProcessedData/uptodate.rds")

# time-series data --------------------------------------------------------

deaths.time = read.csv("RawData/time_series_covid_19_deaths.csv")
confirmed.time = read.csv("RawData/time_series_covid_19_confirmed.csv")
recovered.time = read.csv("RawData/time_series_covid_19_recovered.csv")

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
countryToContinent = read.csv("UtilsData/Countries-Continents.csv")
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


# events per day ----------------------------------------------------------

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

saveRDS(all.per.day, "ProcessedData/allPerDay.rds")


# all across time ---------------------------------------------------------



deaths.time = deaths.time %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Deaths")
confirmed.time = confirmed.time %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Confirmed")
recovered.time = recovered.time %>% pivot_longer(-c(Country,Continent), names_to = "Date", values_to = "Amount") %>% mutate(Metric = "Recovered")

all.time = rbind(deaths.time, confirmed.time, recovered.time)

# from Sys.Date() I believe the preferred date format is YYYY-MM-DD
all.time$Date = as.Date(sapply(strsplit(all.time$Date, "X"), "[[",2), format = "%m.%d.%y")

saveRDS(all.time, "ProcessedData/cleanTime.rds")