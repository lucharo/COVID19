## ---------------------------------------------------
library(dplyr)
## ---------------------------------------------------
covid = read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')
## ---------------------------------------------------
setwd(system('git rev-parse --show-toplevel', intern = TRUE))
print(getwd())

## save local copy
write.csv(covid, 'owid-data.csv')

## ---------------------------------------------------
covid = covid %>% 
  select(iso_code,
         continent,
         location,
         date,
         total_cases,
         new_cases,
         total_deaths,
         new_deaths,
         total_tests,
         new_tests
         )
## --------------------------------------------------
covid$date = as.Date(covid$date)

## ---------------------------------------------------
covid = covid %>% select(-contains('tests'))


## ---------------------------------------------------
## this line selects all columns from covid except those that contain
## the word new
cumulative = covid %>% select(-contains('new'))

daily = covid %>% select(-contains('total'))

## ---------------------------------------------------
daily = daily %>% filter(continent != '')
cumulative = cumulative %>% filter(continent != '')


## ---------------------------------------------------
saveRDS(daily, 'ProcessedData/daily.rds')
saveRDS(cumulative, 'ProcessedData/cumulative.rds')

