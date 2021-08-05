


italy_medals <- olympics %>% 
  filter(noc == "ITA") %>% 
  group_by(edition, year, season, event) %>% 
  summarise(gold = sum(gold, na.rm = TRUE), silver = sum(silver, na.rm = TRUE),
            bronze = sum(bronze, na.rm = TRUE), total = sum(tot_med, na.rm = TRUE))

italy_medals %>% 
  filter(season == 'Summer') %>% 
  plot_ly(x = ~year) %>% 
  add_bars(y = ~bronze, marker = list(color = bronze)) %>% 
  add_bars(y = ~silver, marker = list(color = silver)) %>% 
  add_bars(y = ~gold, marker = list(color = gold)) %>% 
  layout(barmode = 'stack', showlegend = FALSE, yaxis = list(title = 'Medals'))


italy_london <- olympics %>% 
  filter(noc == "ITA", edition == "London   2012") 
  filter(edition == "London   2012")
  
itaMed <- olympics %>% 
  filter(noc == "ITA", tot_med == 1)  %>% 
  group_by(edition, year, season, event) %>% 
  count(gold, silver, bronze) %>% 
  arrange(desc(year))
  tibble()

itaMed$edition <- factor(itaMed$edition, levels = unique(itaMed$edition))

ita_medals_summer <- olympics %>% 
  filter(noc == "ITA", tot_med == 1, season == 'Summer')  %>% 
  group_by(edition, year, event) %>% 
  count(gold, silver, bronze) %>% 
  arrange((year))
tibble()

ita_medals_summer$edition <- factor(ita_medals_summer$edition, 
                                    levels = unique(ita_medals_summer$edition))



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



itaMed %>% 
  filter(season == 'Summer') %>% 
  
  plot_ly(x = ~edition) %>% 
  add_bars(y = ~bronze, marker = list(color = bronze), name = 'Bronze') %>% 
  add_bars(y = ~silver, marker = list(color = silver), name = 'Silver') %>% 
  add_bars(y = ~gold, marker = list(color = gold), name = 'Gold') %>% 
  layout(barmode = 'stack', showlegend = FALSE, yaxis = list(title = 'Medals'))


