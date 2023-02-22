### Second Tidy Tuesday Assignment!
### Created by: Nicole Ayala
### Created on: 2023-02-21
#####################################################################
### Load libraries #######
library(tidyverse)
library(tidytuesdayR)
library(here)
library(readr)
library(devtools)
library(ggdist)
library(ggplot2)
library(BobRossColors)
library(ggridges)
library(ggdist)




###### Loading Data #####
bobross_data<- read_csv(here("Second Tidy Tuesday","Data", "bob_ross.csv"))
glimpse(bob_ross) 
View(bob_ross)




    