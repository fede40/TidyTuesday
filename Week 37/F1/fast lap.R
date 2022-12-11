library(tidytuesdayR)
library(tidyverse)
library(plotly)
library(chron)
library(hms)

tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

lap_times <- tuesdata$lap_times
race_table <- tuesdata$races
drivers <- tuesdata$drivers
lap_times <- left_join(lap_times, select(race_table, raceId, name, year))
lap_times <- left_join(lap_times, select(drivers, driverId, surname))

ltime <- lap_times %>% 
  is_hms(time)

fast_lap <- lap_times %>% 
  group_by(name, year) %>% 
  summarise(fastest = (min(time))) %>% 
  mutate(fastest = as_hms(fastest)) %>% 
  ungroup()

fast_lap %>% 
  plot_ly(x = ~name) %>% 
  add_markers(y = ~fastest, color = ~year)

monza <- fast_lap %>% 
  filter(name == "Italian Grand Prix")

monza %>% 
  plot_ly(x = ~year) %>% 
  add_markers(y = ~fastest)

lap_times %>% 
  mutate(name = fct_reorder(name, time)) %>% 
  plot_ly(x = ~name) %>% 
  add_trace(y = ~time, type = "box")

qual <- tuesdata$qualifying

qual <- left_join(select(drivers, driverId, surname), qual)
