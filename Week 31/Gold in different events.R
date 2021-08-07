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
#drop URSS and Eastern Germany
olympics <- olympics %>% 
  mutate(noc = ifelse(noc == 'URS', 'RUS', noc)) %>% 
  mutate(noc = ifelse(noc == 'GDR', 'GER', noc))

#create gold silver and bronze varialbes plus edition (i.e. city + year)
olympics <- olympics %>% 
  mutate(gold = if_else(medal == "Gold", 1, 0)) %>% 
  mutate(silver = if_else(medal == "Silver", 1, 0)) %>% 
  mutate(bronze = if_else(medal == "Bronze", 1, 0)) %>% 
  mutate(tot_med = ifelse(is.na(medal),0,1)) %>% 
  mutate(edition = paste(city,' ',year))

#set medal colours for charts
col_gold <- '#FFFF00'
silver <- 'RGB 192, 192, 192'
bronze <- 'RGB 205, 127, 50'




#use count instead of sum to avoid doublecounting team sports
medals_summer <- olympics %>% 
  filter( tot_med == 1, season == 'Summer')  %>% 
  group_by(edition, year,noc, sport, event) %>% 
  count(gold, silver, bronze) %>% 
  arrange((year))
tibble()

#how many disciplines have countries won gold in?
gold_by_event_by_nation <- medals_summer %>% 
  group_by(noc, sport) %>% 
  count(gold) %>% 
  filter(gold == 1)

gold_by_event_by_nation %>% 
  group_by(noc) %>% 
  summarise(gold = sum(gold)) %>%
  filter(quantile(gold, .75) < gold) %>% #filter top 25%
  plot_ly(x = ~noc) %>% 
  add_bars(y = ~gold, color = col_gold) %>% 
  layout(title = list(text = "Olympic spirit", yanchor = "top"),
         barmode = 'stack',
         showlegend = FALSE, 
         yaxis = list(title = 'Gold medal events'),
         xaxis = list(title = 'Nation'),
         plot_bgcolor = "#f7f1df",
         paper_bgcolor = '#f7f1df') %>% 
  add_annotations(xref = 'paper', yref = 'paper',
                  x = .99, y = -.15,
                  text = paste('Data Viz: @_fede40 | Data Source: Kaggle'),
                  font = list(family = 'Arial', size = 9.5, color = "#00000"),
                  showarrow = FALSE)
