### first Tidy Tuesday Assignment!
### Created by: Nicole Ayala
### Created on: 2023-02-14
### Updated on: 2023-02-15
#####################################################################
### Load libraries #######
library(tidyverse)
library(here)
library(devtools)
library(ggridges)
library(beyonce)
library(ggplot2)

### Loading in Data #####
movies<- read_csv(here("FirstTidyTuesday","Data","movies.csv.com"))
View(movies)
glimpse(movies)



movieplot<-movies %>%
  ggplot(aes(x = `Release Year`, # my x label
             y = `Age Difference`)) + # my y label
  geom_point(size=0.5, color = "maroon") +
  labs(x = "Age Difference",y = "Release Year", Title = "Hollywood Directors Age Differences over the Years")
  
ggsave(here("FirstTidyTuesday","Output","FirstTidyTuesdayAssignmentNicoleAyala.png"))

movieplot
 
