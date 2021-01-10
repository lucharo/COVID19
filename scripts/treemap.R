suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(tidyr))

# Treemaps ----------------------------------------------------------------

print("Making treemaps...")

## Load dataset
cumulative = readRDS('ProcessedData/cumulative.rds')

## Make countries tables by only selecting the rows
## for the latest date `max(date)`, rename columns to standard
## We also remove the `iso_code` and `date` column as the other
## two datasets won't have those

## Better to always take the day before the latest as most
# data will be available like that
latest = max(cumulative$date) - 1

## line below puts date in readable format: MonthName DayNumber, Year
formatted_date = format(latest, format="%B %d, %Y")

countries = cumulative %>%
  filter(date == latest) %>% 
  rename(Deaths = total_deaths,
         Cases = total_cases,
         labels = location,
         parent = continent) %>%
  select(-c(iso_code, date)) 

## Some small countries regions like Anguilla, Faroe Islands or
## Gibraltar often have missing data, with the next line of
## code we will change the missing data for 0.
countries = countries %>% replace_na(list(Deaths = 0,
                                          Cases = 0))

## Aggregate values by continent and assign each 
## continent its parent: "World"
continents = countries %>% 
  group_by(parent) %>%
  summarise(Deaths = sum(Deaths),
            Cases = sum(Cases)) %>%
  rename(labels = parent) %>%
  mutate(parent = "World") 

## Aggregate all continent values to get the World values
## World has no parent element
world = data.frame(Deaths = sum(continents$Deaths),
                   Cases = sum(continents$Cases),
                   labels = "World",
                   parent = "")

## Concatenate all tables in one
treemap_df = rbind(countries, continents, world)

# Number of deaths --------------------------------------------------------
treemap_deaths <- plot_ly(
  type="treemap",
  values=treemap_df$Deaths,
  labels = treemap_df$labels,
  parents= treemap_df$parent,
  hoverinfo="value+percent parent+percent root") %>%
  layout(title = paste("COVID19 Death Count on", formatted_date)) %>%
  plotly_build()
saveRDS(treemap_deaths, 'Plots/treemap_deaths.rds')


# Number of confirmed cases -----------------------------------------------
treemap_cases <- plot_ly(
  type="treemap",
  values=treemap_df$Cases,
  labels = treemap_df$labels,
  parents= treemap_df$parent,
  hoverinfo="value+percent parent+percent root") %>%
  layout(title = paste("COVID19 Cases on", formatted_date)) %>%
  plotly_build()
saveRDS(treemap_cases, 'Plots/treemap_cases.rds')

# Case-Fatality Rate by Country -------------------------------------------
treemap_deaths_by_cases = with(
  treemap_df,
  plot_ly(
    type="treemap",
    values=(Deaths/Cases)*100,
    labels = labels,
    parents= parent,
    hoverinfo="value",
    hovertemplate="%{value:.2f}%<extra></extra>")%>%
    ## <extra></extra> removes the trace0 label
    layout(title = paste("COVID19 Fatality-to-Case Ratio on", formatted_date)) %>%
    plotly_build()
)
saveRDS(treemap_deaths_by_cases, 'Plots/treemap_deaths_by_cases.rds')


