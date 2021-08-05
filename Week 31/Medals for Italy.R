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
#Get Lat Long of olympic venues
lat_long <-  tidygeocoder::geo(city = unique(olympics$city), method = "osm")
#join the new lat long table to the original data
olympics <- left_join(olympics, lat_long)


#create gold silver and bronze varialbes plus edition (i.e. city + year)
olympics <- olympics %>% 
  mutate(gold = if_else(medal == "Gold", 1, 0)) %>% 
  mutate(silver = if_else(medal == "Silver", 1, 0)) %>% 
  mutate(bronze = if_else(medal == "Bronze", 1, 0)) %>% 
  mutate(tot_med = ifelse(is.na(medal),0,1)) %>% 
  mutate(edition = paste(city,' ',year))

#set medal colours for charts
gold <- '#FFFF00'
silver <- 'RGB 192, 192, 192'
bronze <- 'RGB 205, 127, 50'



  
#use count instead of sum to avoid doublecounting team sports
ita_medals_summer <- olympics %>% 
  filter(noc == "ITA", tot_med == 1, season == 'Summer')  %>% 
  group_by(edition, year, event) %>% 
  count(gold, silver, bronze) %>% 
  arrange((year))
tibble()

#fix x axis
ita_medals_summer$edition <- factor(ita_medals_summer$edition, 
                                    levels = unique(ita_medals_summer$edition))


#plot
ita_medals_summer %>% 
  plot_ly(x = ~edition) %>% 
  add_bars(y = ~bronze, marker = list(color = bronze), name = 'Bronze') %>% 
  add_bars(y = ~silver, marker = list(color = silver), name = 'Silver') %>% 
  add_bars(y = ~gold, marker = list(color = gold), name = 'Gold') %>% 
  layout(title = "Italy's Medals by Olympic Edition",barmode = 'stack', showlegend = FALSE, yaxis = list(title = 'Medals'),
         xaxis = list(title = 'Edition'),
         plot_bgcolor = "#f7f1df",
         paper_bgcolor = '#f7f1df') %>% 
  add_annotations(xref = 'paper', yref = 'paper',
                  x = .99, y = -.7,
                  text = paste('Data Viz: @_fede40 | Data Source: Kaggle'),
                  font = list(family = 'Arial', size = 9.5, color = "#00000"),
                  showarrow = FALSE)





