### Second Tidy Tuesday Assignment!
### Created by: Nicole Ayala
### Created on: 2023-02-21
#####################################################################
### Load libraries #######
library(tidyverse)
library(tidytuesdayR)
library(here)
library(readr)


###### Loading Data #####
urlfile="https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv"

mydata<-read_csv(url(urlfile))


glimpse(bob_ross) # wanna see how the data looks prior to choosing the data I'd liek to plot

bob_ross <- select(bob_ross, -1) + # removing the first column that does not contain any necessary data

bob_ross <- bob_ross |> 
  mutate(across(Black_Gesso:Alizarin_Crimson, as.logical))
# saving the data
  bob_ross<- read_csv(here("Second Tidy Tuesday", "Data", "bob_ross.csv"))

    