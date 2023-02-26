### Second Tidy Tuesday Assignment!
### Created by: Nicole Ayala
### Created on: 2023-02-21
### Updated on: 2023-02-25
#####################################################################


### Load libraries #######
library(tidyverse)
library(tidytuesdayR)
library(here)
library(readr)
library(devtools)
library(widyr)
library(ggraph)



###### Loading Data #####
bobross<- read_csv(here("Second Tidy Tuesday","Data", "bob_ross.csv")) # time to laod in the data :)
glimpse(bobross) 
View(bobross_data) # see if data works
select(-contains("src")) %>% # pipe itself so it is easily readable and replicable, img_Src
  filter(season <= 31) %>% # filter out seasons through 31 and under
  mutate(season = paste("Season", season)) # combine two columns by row into a single cell, wanna change season into Season


# time to plot :D

bobross_data<-bobross %>%
  pivot_longer(cols = 8:25, names_to = "color") %>% # the cols you want to pivot. This says select the color cols form 8 to 25 and change it to colors
  filter(value) %>% #filter out all the values that do not correlate to color
  mutate(color = str_replace_all(color, "_", " ")) %>% # want to replace all the different variations of color names into a cohesive manner
  group_by(season) %>% # grouping 10 colors per season
  
  count(season, color) %>%
  slice_max(n, n = 3, with_ties = FALSE) %>% #select rows with highest or lowest values of a variable, n =population
  
  ggplot(aes(n, color, fill = season)) + #fill each season with 10 colors
  facet_wrap(~season, scales ="free") + #releases both x and y axis
  labs(x = "season", # my x label
       y = "color", # my y label
       title = "The 10 Most Common Colors Per Season") # title of my plot
  

ggsave(here("Second Tidy Tuesday","Output","SecondTidyTuesdayAssignment(2023-02-21).png"))

