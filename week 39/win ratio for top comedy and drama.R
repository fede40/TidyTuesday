library(tidyverse)
library(plotly)
library(tidytuesdayR)
library(stringr)
library(forcats)
library(sysfonts)

tuesdata <- tidytuesdayR::tt_load(2021, week = 39)
nominees <- tuesdata$nominees

#Add series for Nom, Win and Total
nominees <- nominees %>% 
  mutate(nominee = ifelse(type == 'Nominee', 1, 0)) %>% 
  mutate(winner = ifelse(type =='Winner', 1, 0)) %>% 
  mutate(total_nom = winner + nominee)

#group by title to check win vs. nominations vs. Total
nom_grp <- nominees %>% 
  group_by(title) %>% 
  summarise(nom = sum(nominee), win = sum(winner), tot = sum(total_nom)) %>% 
  ungroup()


#look at series only
series <- nominees %>% 
  filter(str_detect(category, "Comedy|Drama"))

#group
ser_grp <- series %>% 
  group_by(title) %>% 
  summarise(nom = sum(nominee), win = sum(winner), tot = sum(total_nom)) %>% 
  ungroup()


#create a chart representing the win ratio of the 25% most successful shows
p1 <- nom_grp %>%
  mutate(title = (fct_reorder(title, desc(tot)))) %>% 
  filter(quantile(tot,.90)< tot) %>% 
  mutate(win_ratio = win/tot) %>% 
  mutate(title = (fct_reorder(title, desc(win_ratio)))) %>% 
  filter(quantile(win_ratio, .75) < win_ratio) %>% 
  plot_ly(x = ~title) %>% 
  add_bars(y = ~win_ratio)


p2 <- ser_grp %>%
  mutate(title = (fct_reorder(title, desc(tot)))) %>% 
  filter(quantile(tot,.90)< tot) %>% 
  mutate(win_ratio = win/tot) %>% 
  mutate(title = (fct_reorder(title, desc(win_ratio)))) %>% 
  plot_ly(x = ~title) %>% 
  add_bars(y = ~win_ratio)

