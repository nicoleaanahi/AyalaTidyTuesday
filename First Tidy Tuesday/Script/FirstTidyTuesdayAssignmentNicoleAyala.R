### first Tidy Tuesday Assignment!
### Created by: Nicole Ayala
### Created on: 2023-02-14
### Updated on: 2023-02-15
#####################################################################
### Load libraries #######
library(tidyverse)
library(here)
library(devtools)
library(ggrforce)
l
library(ggplot2)

### Loading in Data #####
movies<- read_csv(here("FirstTidyTuesday","Data","movies.csv.com"))
View(movies)
glimpse(movies)
movies_clean<-movies %>% # Always remember the pipe! :)
  filter(complete.cases(.)) #filters out everything that is not a complete row
  filter(.data = movies_clean, age < 50, gender == "woman")

movies %>%
  ggplot(aes(x = `Release Year`,
             y = `Actor 2 Age`)) +
  ggforce::geom_sina(
    maxwidth = .6, scale = "count", seed = 1,
    size = 7, alpha = .5
  ) + 
  ggforce::geom_sina(
    maxwidth = .6, scale = "count", seed = 1, 
    size = 7, shape = 1, color = "blue",
    stroke = .8
  )
 
 
