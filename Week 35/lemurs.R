library(tidyverse)
library(tidytuesdayR)
library(plotly)
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)


#load data
#tuesdata <- tidytuesdayR::tt_load('2021-08-24')
#tuesdata <- tidytuesdayR::tt_load(2021, week = 35)

#lemurs <- tuesdata$lemurs

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')


df <- lemurs %>% 
  group_by(taxon, age_at_death_y, litter_size, weight_g) %>% 
  summarise(weight_g = mean(weight_g)) %>%
  drop_na() %>% 
  tibble()


set.seed(12)
model <- train(taxon ~ .,
               data = df,
               method = "ranger")
print(model)
 

m <- rpart(taxon ~ ., data = df,
           method = "class")

rpart.plot(m, box.palette = "auto")

plot(m)
text(m, use.n = TRUE)



index = createDataPartition(y=df$taxon, p=0.7, list=FALSE)

train.set = df[index,]
test.set = df[-index,]

dim(train.set)

lemur.tree  = train(taxon ~ ., 
                    data=train.set, 
                    method="rpart", 
                    trControl = trainControl(method = "cv"))

plot(lemur.tree$finalModel, uniform=TRUE,
     main="Classification Tree")
text(lemur.tree$finalModel, use.n.=TRUE, all=TRUE, cex=.8)

df %>% 
  plot_ly(x = ~weight_g) %>% 
  add_markers(y = ~age_at_death_y, color = ~taxon)