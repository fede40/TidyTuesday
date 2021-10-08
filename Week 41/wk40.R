library(tidytuesdayR)
library(tidyverse)
library(plotly)
library(cluster)
library(factoextra)
library(reshape2)


tuesdata <- tidytuesdayR::tt_load('2021-10-05')
nurses <- tuesdata$nurses
#load df to get state codes
df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")

#look at differences between 2020 and 2010
nur20 <- nurses %>% 
  filter(Year %in% c(2010, 2020)) %>% 
  select(State, Year, 'Hourly Wage Avg') %>% 
  spread(Year, 'Hourly Wage Avg') %>% 
  mutate(diff = `2020`- `2010`) %>% 
  mutate(rel_diff = `2020`/`2010`-1)

#rename state to be able to join the data frames
df <- df %>% 
  mutate(State = state)
#join dataframes
nur_join <- left_join(nur20, df)





########################### Create Choropleth chart  ###################
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig <- plot_geo(nur_join, locationmode = 'USA-states')
fig <- fig %>% add_trace(
  z = ~rel_diff, locations = ~code, color = ~rel_diff)
fig <- fig %>% colorbar(title = "Relative change")
fig <- fig %>% layout(
  title = 'Change in hourly average wage for nurses by state 2020 vs. 2010',
  geo = g
) %>% 
  add_annotations(xref = 'paper', yref = 'paper', #footnote
                  x = .99, y = -.0,
                  text = paste('Data Viz: @_fede40 |<br>Data Source: Data World'),
                  font = list(family = 'Arial', size = 9, color = "#00000"),
                  showarrow = FALSE)  %>% 
  add_annotations(xref = 'paper', yref = 'paper', #commentary
                  x = 1.12, y = .25,
                  text = paste('Increase in California was nearly 40% <br> By contrast,<br> in Alabama and Delaware it was <5%'),
                  font = list(family = 'Arial', size = 13, color = "#00000"),
                  showarrow = FALSE)


fig

################## Comet Plot #####################

  comet_plot <- nur20 %>% 
  ggplot() + 
  geom_link(aes(x = `2010`, y = fct_reorder(`State`, `diff`), 
                xend = `2020`, yend = fct_reorder(`State`, `diff`), 
                color = `diff`, size = stat(index))) +
  scale_color_gradient(low = "#00FF00", high = "#85bb65")+ #gradient of colors
    geom_point(
    data = nur20,
    aes(`2020`, y =  fct_reorder(`State`, `diff`)),
    shape = 21,
    fill = "white",
    size = 5
  )  +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = "transparent", # Needed to add the border
                                                   color = 1,            # Color of the border
                                                   size = 2),
       panel.background = element_rect(fill = "#f7f1df"),
       plot.background = element_rect(fill = "#f7f1df")) +
    labs( x = "Average Hourly Wage", y = "State",
       title = "2020 vs 2010 change Hourly Salary",
       caption = "Data viz: @_fede40 | 
       Data Source: Data World")
 
comet_plot
  
  
 
