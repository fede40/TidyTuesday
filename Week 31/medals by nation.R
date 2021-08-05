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
library(reshape2)

#Load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 31)

#set colours for charts
gold <- '#FFFF00'
silver <- 'RGB 192, 192, 192'
bronze <- 'RGB 205, 127, 50'

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



#Get total medals by nations

medals_by_country <- olympics %>% 
  group_by(noc) %>% 
  summarise(Gold = sum(gold, na.rm = TRUE), Silver = sum(silver, na.rm = TRUE), 
                       Bronze = sum(bronze, na.rm = TRUE), Total = sum(tot_med))

medals_by_country <-  medals_by_country %>% 
  dplyr::arrange(desc(Total))


p <- medals_by_country %>% slice_head(n = 10) %>% 
  plot_ly(x = ~noc) %>% 
  add_bars(y = ~Gold, name = "Gold", marker = list(color = gold))%>% 
  add_bars(y = ~Silver, name = "Silver", marker = list(color = silver)) %>% 
  add_bars(y = ~Bronze, name = "Bronze", marker = list(color = bronze))

p

layout_func(p, "Country", "# of Medals", 'Number of medals by nations',
            TRUE)

medals_long <- medals_by_country %>% 
  melt(id = c("noc"))

medals_long %>%
  filter(variable != "Total") %>% 
  plot_ly(x = ~value) %>% 
  add_bars(y = ~noc, color = ~variable) 