suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plotly))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(tidyr))

# Barplots over time ------------------------------------------------------
all.time = readRDS('ProcessedData/cleanTime.rds')
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
                      min(all.time$Date),": "))) %>% plotly_build()

saveRDS(bar.over.time, 'Plots/bar_over_time.rds')

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
  layout(
    # yaxis = list(title = "", showticklabels = F),
    xaxis = list(title = "Recoveries",
                 range = c(0,max(forSubplot$Amount[forSubplot$Metric == "Recovered"]))
    )) 

facet3 =  forSubplot %>% filter(Metric=="Deaths") %>% 
  plot_ly(y = ~Country, x = ~Amount, frame = ~Date,color = ~Continent,
          type = "bar",showlegend = F) %>%
  layout(
    # yaxis = list(title = "", showticklabels = F),
    xaxis = list(title = "Deaths", range = c(0,max(forSubplot$Amount[forSubplot$Metric == "Deaths"]))))

byevent = suppressWarnings(
  subplot(facet1, facet3, facet2, 
          titleX = T, nrows = 1,
          titleY = T, shareY = T) %>%  animation_slider(
            currentvalue = list(
              prefix = paste0("Days from ",
                              min(all.time$Date),": "))) %>% plotly_build()
)

saveRDS(byevent, 'Plots/byevent.rds')
