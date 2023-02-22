### Second Tidy Tuesday Assignment!
### Created by: Nicole Ayala
### Created on: 2023-02-21
### Updated on: 2023-02-22
#####################################################################


### Load libraries #######
library(tidyverse)
library(tidytuesdayR)
library(here)
library(readr)
library(devtools)
library(ggdist) 
library(ggplot2) #dont think I need this for this plot




###### Loading Data #####
bobross_data<- read_csv(here("Second Tidy Tuesday","Data", "bob_ross.csv"))
glimpse(bob_ross) 
View(bob_ross)


bobross_colors <- bob_ross %>%
  mutate(color_hex = str_remove_all(color_hex, '\\['),
         color_hex = str_remove_all(color_hex, '\\]'),
         color_hex = str_remove_all(color_hex, "'")) %>%
  separate_rows(color_hex, sep = ',')

#hexes for the fill
hexcolors <- unique(bobross_colros$color_hex) %>% str_remove_all(' ')


bobrossplot <- bobross_colors %>%
  group_by(season) %>%
  count(color_hex) %>%
  ggplot(aes(x = season, y = colors, size = 5, color = color_hex))  +
  geom_linerange(aes(x=1,xmin=1,xmax=1,y=0, ymin=0,ymax=26),inherit.aes = F, alpha = .7, color = 'pink') +
  geom_count(alpha = .4) +
  annotate(geom = 'text', x = 1, y = 20, label = 'Season 3 , Episode 2', size = 10, color = 'pink') +
  geom_segment(aes(x = 1, y = 22, xend = 10, yend = 22),
              color = 'pink', size = .75) +
  scale_color_manual(values = hexes) +
  coord_polar() +
  scale_size(range = c(.1, 10)) +
  theme_light() +
  labs(title = 'Colors Within the Blue Moon Painting by Bob Ross') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = .5,vjust = -1.25, size = 60, color = 'pink'),
        plot.background = element_rect(fill = 'white', color = 'white'))

ggsave(here("SecondTidyTuesday","Output","SecondTidyTuesdayAssignment(2023-02-21).png"))

bobrossplot


