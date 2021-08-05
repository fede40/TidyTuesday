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

  
  
#Get total medals by nations by edition
medals_by_edition <- olympics %>% 
  group_by(edition, noc, lat, long, season) %>% 
  summarise(medals = sum(tot_med))

#get markers to differentiate summer and winter olympics
icons <- iconList(
  Winter = makeIcon(awesomeIcons(icon = "snow-outline", library = "ion")),
  Summer = makeIcon(awesomeIcons(icon = "sunny-outline", library = "ion"))
)

Winter_icon <- awesomeIcons(icon = "snow-outline", library = "ion", 
                            iconColor = 'white')
Summer_icon <- awesomeIcons(icon = "sunny-outline", library = "ion",
                            iconColor = 'white')


#plot map, size of marker = no of medals for Italy in each edition
m <- medals_by_edition %>% 
  filter(noc == "ITA", season == "Summer") %>%
  mutate(med_ed = glue("Edition: {edition}; Medals: {medals}")) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addAwesomeMarkers(~long, ~lat, label = ~med_ed, 
                    icon = Summer_icon)
m



m <- medals_by_edition %>% 
  filter(noc == "ITA") %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~long, ~lat, label = ~medals)
m
#layout function
layout_func <- function(plot_name, xaxis, yaxis, header, legend) {
  plot_name %>% 
    layout(showlegend = legend,
           legend = list(bgcolor = '#fff', orientation = 'h'),
           xaxis = list(
             title = xaxis, color = "##000100"),
           yaxis = list(
             title = yaxis, color = "##000100"),
           title = list(text = header, font = list(color = "##000100")),
           paper_bgcolor = "#d0f2f5",
           plot_bgcolor = "#fffff"
    ) %>% 
    add_annotations(xref = 'paper', yref = 'paper',
                    x = .99, y = -.09,
                    text = paste('Data Viz: @_fede40 | Data Source: Kaggle'),
                    font = list(family = 'Arial', size = 9.5, color = "#c0c0c0"),
                    showarrow = FALSE)
}



plot_bgcolor = "#b5f1f7"

 
fig <- medals_by_edition %>%
  plot_ly(
    lat = ~lat,
    lon = ~long,
    marker = list(color = "fuchsia"),
    type = 'scattermapbox')
fig <- fig %>%
  layout(
    mapbox = list(
      style = 'open-street-map'))

fig
