suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))

# Treemaps ----------------------------------------------------------------

print("Making treemaps...")

sum.table = readRDS('ProcessedData/sumtable.rds')

df.plotly = sum.table
df.plotly$Continent = ifelse(is.na(df.plotly$Continent), "Other", df.plotly$Continent)

conts = df.plotly %>% 
  group_by(Continent) %>%
  summarise(Deaths = sum(Deaths),
            Confirmed = sum(Confirmed),
            Recovered = sum(Recovered),
            Active = sum(Active)) %>%
  mutate(parent = "World") %>%
  rename(labels = Continent)

countrs = df.plotly %>% 
  group_by(Country.Region) %>% 
  summarise(Deaths = sum(Deaths),
            Confirmed = sum(Confirmed),
            Recovered = sum(Recovered),
            Active = sum(Active), 
            parent = unique(Continent)) %>% 
  rename(labels = Country.Region)

# regs = df.plotly %>% ungroup() %>% select(-Continent) %>% rename(labels = Province.State, parent = Country.Region)
# regs = regs[,c(2,3,4,5,6,1)]
world = data.frame(labels = "World",
                   Deaths = sum(conts$Deaths),
                   Confirmed = sum(conts$Confirmed),
                   Recovered = sum(df.plotly$Recovered),
                   Active = sum(conts$Active),
                   parent = "")

df.plotly = rbind(conts, countrs, world)
saveRDS(df.plotly, "ProcessedData/df_plotly.rds")


# Number of deaths --------------------------------------------------------
deads <- plot_ly(
  type="treemap",
  values=df.plotly$Deaths,
  labels = df.plotly$labels,
  parents= df.plotly$parent,
  hoverinfo="label+value+percent parent+percent root"
  # textinfo="label+value+percent parent+percent entry+percent root",
  #domain=list(column=0)
) %>% plotly_build()
saveRDS(deads, 'Plots/deads.rds')


# Number of confirmed cases -----------------------------------------------
conf <- plot_ly(
  type="treemap",
  values=df.plotly$Confirmed,
  labels = df.plotly$labels,
  parents= df.plotly$parent,
  hoverinfo="label+value+percent parent+percent root"
  # textinfo="label+value+percent parent+percent root"
  #domain=list(column=0)
) %>% plotly_build()
saveRDS(conf, 'Plots/conf.rds')


# Number of recovered -----------------------------------------------------
rec <- plot_ly(
  type="treemap",
  values=df.plotly$Recovered,
  labels = df.plotly$labels,
  parents= df.plotly$parent,
  hoverinfo="label+value+percent parent+percent root"
  #domain=list(column=0)
) %>% plotly_build()
saveRDS(rec, 'Plots/rec.rds')


# Case-Fatality Rate by Country -------------------------------------------
deaths_conf <- plot_ly(
  type="treemap",
  values=round(df.plotly$Deaths/df.plotly$Confirmed,4)*100,
  labels = df.plotly$labels,
  parents= df.plotly$parent,
  hoverinfo="label+value+percent",
  hovertext = "Potato"
  #domain=list(column=0)
) %>% plotly_build()
saveRDS(deaths_conf, 'Plots/deaths_conf.rds')


# Ratio of recovered-to-confirmed cases -----------------------------------
rec_conf <- plot_ly(
  type="treemap",
  values=df.plotly$Recovered/df.plotly$Confirmed,
  labels = df.plotly$labels,
  parents= df.plotly$parent
  # textinfo="label+value+percent parent+percent entry+percent root",
  #domain=list(column=0)
) %>% plotly_build()
saveRDS(rec_conf, 'Plots/rec_conf.rds')
