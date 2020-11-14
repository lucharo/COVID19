## ----setup, include=FALSE---------------------------
knitr::opts_chunk$set(echo = TRUE, message = F, warning = F, fig.align = 'center')


## ---------------------------------------------------
library(dplyr)


## ---------------------------------------------------
covid = read.csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv')


## ---------------------------------------------------
## save local copy
write.csv(covid, '../owid-data.csv')


## ---------------------------------------------------
str(covid)


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


## ---------------------------------------------------
covid$date = as.Date(covid$date)


## ---------------------------------------------------
library(naniar)
vis_miss(covid %>% arrange(date) %>% select(contains(c('total','new'))))


## ---------------------------------------------------
covid = covid %>% select(-contains('tests'))


## ---------------------------------------------------
## this line selects all columns from covid except those that contain
## the word new
cumulative = covid %>% select(-contains('new'))

daily = covid %>% select(-contains('total'))


## ---------------------------------------------------
any(daily$new_cases < 0)


## ---------------------------------------------------
any(daily$new_deaths < 0)


## ---------------------------------------------------
library(ggplot2)
daily %>% 
  filter(iso_code == 'ESP') %>%
  ggplot(aes(x = date,y = new_cases))+
  geom_point(color = 'red')+
  ggtitle('Spain\'s Daily COVID cases')


## ---------------------------------------------------
daily %>% 
  filter(iso_code == 'GBR') %>%
  ggplot(aes(x = date,y = new_cases))+
  geom_point(color = 'darkblue')+
  ggtitle('UK Daily Confirmed Cases')

## ---------------------------------------------------
cumulative %>% 
  filter(iso_code == 'GBR') %>%
  ggplot(aes(x = date,y = total_deaths))+
  geom_point(color = 'darkgreen')+
  ggtitle('Italy\'s Cumulative Confirmed Cases')+
  ylab('Total number of confirmed cases (log10 scale)')


## ---------------------------------------------------
daily = daily %>% filter(continent != '')
cumulative = cumulative %>% filter(continent != '')


## ---------------------------------------------------
saveRDS(daily, '../ProcessedData/daily.rds')
saveRDS(cumulative, '../ProcessedData/cumulative.rds')

