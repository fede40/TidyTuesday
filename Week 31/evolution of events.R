#Load libraries
library(tidytuesdayR)
library(tidyverse)
library(plotly)
library(lubridate)
library(chron)
library(leaflet)
library(tidygeocoder)
library(ggmap)
library(data.table)
library(janitor)
library(glue)

#Load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 31)

olympics <- tuesdata$olympics
olympics <- clean_names(olympics)

#create gold silver and bronze varialbes plus edition (i.e. city + year)
olympics <- olympics %>% 
  mutate(gold = if_else(medal == "Gold", 1, 0)) %>% 
  mutate(silver = if_else(medal == "Silver", 1, 0)) %>% 
  mutate(bronze = if_else(medal == "Bronze", 1, 0)) %>% 
  mutate(tot_med = ifelse(is.na(medal),0,1)) %>% 
  mutate(edition = paste(city,' ',year))

#Filter summer
summer_olympics <- olympics %>% 
  filter(season == "Summer")

#calculate total number of events by looking at gold medals assigned
no_of_events <- summer_olympics %>% 
  group_by(edition, year, sport) %>%  
  summarise(events = sum(!is.na(gold)))

no_of_events <- summer_olympics %>% 
  group_by(edition, year, sport) %>%  
  summarise(events = length(unique(event)))



no_of_events %>% 
  plot_ly(x = ~year) %>% 
  add_trace(y = ~events, color = ~sport, type = 'bar') %>% 
  layout(barmode = 'stack')
