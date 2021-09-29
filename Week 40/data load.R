library(tidytuesdayR)
library(plotly)
library(tidyverse)
library(RColorBrewer)

tuesdata <- tidytuesdayR::tt_load('2021-09-28')

papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/authors.csv')
programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/programs.csv')
paper_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_authors.csv')
paper_programs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/paper_programs.csv')

#create dataset
papers <- left_join(papers, paper_authors) %>% 
  left_join(authors) %>% 
  left_join(paper_programs) %>% 
  left_join(programs)%>% 
  mutate(
    catalogue_group = str_sub(paper, 1, 1),
    catalogue_group = case_when(
      catalogue_group == "h" ~ "Historical",
      catalogue_group == "t" ~ "Technical",
      catalogue_group == "w" ~ "General"
    ),
    .after = paper
  ) 


#group papers by year and description
papers_grp <- papers %>% 
  group_by(year, program_desc) %>% 
  summarise(n = n()) %>% 
  ungroup()

clr <- brewer.pal(9, 'YlGnBu')
pal <- colorRampPalette(clr)(22)

#chart
p1 <- papers_grp %>% 
  plot_ly(x = ~year, y = ~n, type = 'scatter', 
          mode = 'none', stackgroup = 'one', color = ~program_desc, 
        colors= pal) %>% 
  layout(title = list(text = "Evolution of NBER papers<br> by topic, over time", yanchor = "top"),
         barmode = 'stack',
         legend = list(orientation = 'h'), 
         yaxis = list(title = 'Number of papers', showgrid = FALSE),
         xaxis = list(title = 'Year', showgrid = FALSE),
         legend = list( x = .85, y = .95, size = 10),
         plot_bgcolor = "#f7f1df",
         paper_bgcolor = '#f7f1df',
         images = list(
           list(source = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/nber-logo.png",
                xref = "paper",
                yref = "paper",
                x= 0.8,
                y= 1.05,
                sizex = 0.2,
                sizey = 0.2,
                opacity = 0.8
           ))
         )%>% 
  add_annotations(xref = 'paper', yref = 'paper',
                  x = .98, y = -.30,
                  text = paste('Data Viz: @_fede40 |<br>Data Source: NBER'),
                  font = list(family = 'Arial', size = 10, color = "#00000"),
                  showarrow = FALSE)  
p1


