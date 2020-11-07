suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(dplyr))

up.to.date = readRDS("ProcessedData/uptodate.rds")

# Table view --------------------------------------------------------------

print("Making tables...")
sum.table = up.to.date %>% 
  select(-c(SNo)) %>%
  filter(ObservationDate==max(ObservationDate)) %>% 
  group_by(Continent, Country.Region) %>%
  distinct() %>% # for some reason I was getting duplicated entries
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths), #using max because data is cumulative
            Recovered = sum(Recovered),
            Active = sum(Active)) %>%
  arrange(-Confirmed)
# knitr::kable(sum.table %>% head(20))

continent.wise = up.to.date %>%
  select(-c(Last.Update, SNo)) %>%
  filter(ObservationDate==max(ObservationDate)) %>%
  group_by(Continent) %>%
  summarise(Confirmed = sum(Confirmed),
            Deaths = sum(Deaths), #using max because data is cumulative
            Recovered = sum(Recovered),
            Active = sum(Active)) %>% 
  arrange(-Active) %>%
  head(20)

saveRDS(sum.table, "ProcessedData/sumtable.rds")
saveRDS(continent.wise, "ProcessedData/continentWise.rds")
