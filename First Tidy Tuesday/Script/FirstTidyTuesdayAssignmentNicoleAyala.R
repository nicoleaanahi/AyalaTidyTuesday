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

# thought I would start simple with my first tidy tuesday and I'll be more creative in my next one
# PIPES ARE IMPORTANT

movieplot<-movies %>%
  ggplot(aes(x = `Release Year`, # my x label
             y = `Age Difference`)) + # my y label
  geom_point(size=0.5, color = "maroon") +
  labs(x = "Age Difference",y = "Release Year", Title = "Hollywood Directors Age Differences over the Years") # CONFUSED AS TO WHY MY TITLE IS NOT PRESENT, BUT SMALL SIZE MAKES DATA SEEM LESS MERGED TOGETHER



ggsave(here("FirstTidyTuesday","Output","FirstTidyTuesdayAssignment(2023-02-17).png"))

movieplot
