library(tidyverse)
library(plotly)
library(tidytuesdayR)
library(stringr)
library(forcats)
library(sysfonts)

tuesdata <- tidytuesdayR::tt_load(2021, week = 39)
nominees <- tuesdata$nominees
nominees %>% 
  select(type) %>% 
  unique()

nominees %>% 
  select(category) %>% 
  unique()

View(nominees %>% filter(title == "Friends"))

nominees %>% 
  select(title) %>% 
  unique()

#Create tibble for comedy series
comedy <- nominees %>% 
  filter(str_detect(category, "Comedy|COMEDY")) %>% 
  mutate(nominee = ifelse(type == 'Nominee', 1, 0)) %>% 
  mutate(winner = ifelse(type =='Winner', 1, 0))

#group by title to check win vs. nominations vs. Total
com_grp <- comedy %>% 
  group_by(title) %>% 
  summarise(nom = sum(nominee), win = sum(winner)) %>% 
  ungroup() %>% 
  mutate(total_nom = nom + win)

#create chart of win ratios
com_grp %>% 
  filter(quantile(total_nom,.75)< total_nom) %>% 
  mutate(win_ratio = win/total_nom) %>% 
  mutate(title = (fct_reorder(title, desc(win_ratio)))) %>% 
  filter(quantile(win_ratio, .75) < win_ratio) %>% 
  plot_ly(x = ~title) %>% 
  add_bars(y = ~win_ratio)
