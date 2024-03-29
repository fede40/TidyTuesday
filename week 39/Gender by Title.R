library(tidyverse)
library(plotly)
library(tidytuesdayR)
library(stringr)
library(forcats)
library(sysfonts)

tuesdata <- tidytuesdayR::tt_load(2021, week = 39)
nominees <- tuesdata$nominees

#Actors and Actresses who've been nominated
actrs <- nominees %>% 
  filter(str_detect(category, "Actor|Actress")) 
actrs <- actrs  %>% 
mutate(actress = ifelse(str_detect(category, "Actress"), 1, 0) ) %>% 
  mutate(actor = ifelse(str_detect(category, "Actor"), 1, 0) )


#Group by year and title
actrs_grp <- actrs %>% 
  group_by(year, title, distributor) %>% 
  summarise(actor = sum(actor), actress = sum(actress), total = actor+actress) %>% 
  ungroup()



#plot nominations
p3 <- actrs_grp %>% 
  filter(year > 2010 & title != "Saturday Night Live") %>% #chart series after 2010 and drop SNL
  group_by(title) %>% 
  summarise(actor = sum(actor), actress = sum(actress), total = sum(total)) %>% 
  filter(quantile(total,.9)< total) %>% #only pick the top 15%
  mutate(title = (fct_reorder(title, desc(total)))) %>% #reorder axis
  plot_ly(x = ~title) %>% 
  add_bars(y = ~actor, name = 'Actor', color = "#fcba03") %>% 
  add_bars(y = ~actress, name = "Actress", color = "#e87a3a") %>%   
  layout(title = list(text = "Nominations Actresses and Actors", yanchor = "top"),
        barmode = 'stack',
         showlegend = TRUE, 
         yaxis = list(title = 'Total Nominations'),
         xaxis = list(title = 'Series'),
         legend = list( x = .85, y = .95, size = 10),
         plot_bgcolor = "#f7f1df",
         paper_bgcolor = '#f7f1df')
            


#Create dataframe for Winning Actresses and Actors
actrs_w <- nominees %>% 
  filter(str_detect(category, "Actor|Actress") & type == "Winner") %>% 
  mutate(actress = ifelse(str_detect(category, "Actress"), 1, 0) ) %>% 
  mutate(actor = ifelse(str_detect(category, "Actor"), 1, 0) )

#Group by year and title
actrs_grp_w <- actrs_w %>% 
  group_by(year, title, distributor) %>% 
  summarise(actor = sum(actor), actress = sum(actress), total = actor+actress) %>% 
  ungroup()


p4 <- actrs_grp_w %>% 
  filter(year > 2010 & title != "Saturday Night Live") %>% #chart series after 2010 
  group_by(title) %>% 
  summarise(actor = sum(actor), actress = sum(actress), total = sum(total)) %>% 
  filter(quantile(total,.70)< total) %>% #only pick the top 15%
  mutate(title = (fct_reorder(title, desc(total)))) %>% #reorder axis
  plot_ly(x = ~title) %>% 
  add_bars(y = ~actor, name = 'Actor', color = "#fcba03") %>% 
  add_bars(y = ~actress, name = "Actress", color = "#e87a3a")  %>% 
  layout(title = list(text = "Winners Actresses and Actors", yanchor = "top"),
         barmode = 'stack',
         showlegend = TRUE, 
         yaxis = list(title = 'Total Awards'),
         xaxis = list(title = 'Series'),
         legend = list( x = .85, y = .95, size = 10),
         plot_bgcolor = "#f7f1df",
         paper_bgcolor = '#f7f1df')


#plot side by side
subplot(p3, p4) %>% 
  layout(title = list(text = "Nominees vs. Wins by show"),
         annotations = list(
           list( 
             x = 0.2,  
             y = .95,  
             text = "Nominees",  
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = FALSE 
           ),  
           list( 
             x = 0.8,  
             y = .95,  
             text = "Winners",  
             xref = "paper",  
             yref = "paper",  
             xanchor = "center",  
             yanchor = "bottom",  
             showarrow = FALSE 
           )
         )) %>% 
  add_annotations(xref = 'paper', yref = 'paper',
                  x = .55, y = -.70,
                  text = paste('Data Viz: @_fede40 <br>| Data Source: Emmys'),
                  font = list(family = 'Arial', size = 9.5, color = "#00000"),
                  showarrow = FALSE)  
      
 
  
