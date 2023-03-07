
### Third Tidy Tuesday Assignment!
### Created by: Nicole Ayala
### Created on: 2023-02-28
### Updated on: 2023-03-04
#####################################################################

### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(plyr)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(grid)

tuesdata <- tidytuesdayR::tt_load('2023-02-28') # literally loading the data

afrisenti <- tuesdata$afrisenti # access list tuesdat$afrisenti as solely afrisenti
languages <- tuesdata$languages # name tuesdata$languages list as languages
language_scripts <- tuesdata$language_scripts # name tuesdata$language_scripts as language_scripts
language_countries <- tuesdata$language_countries # name  tuesdata$language_countries as language_countries
country_regions <- tuesdata$country_regions # name  tuesdata$country_regions as country_regions


# Check NA
afrisenti %>% apply(MARGIN = 2,FUN = function(x)sum(is.na(x)))

# Creating count table
tmp <- aggregate(x = afrisenti$label,by = list(afrisenti$language_iso_code),FUN = function(x)table(x))
df0 <- tmp[,2] %>% data.frame() %>% cbind('language_iso_code' = tmp[,1],.)
df <- join(df0,languages)
df$total <- df$negative + df$positive + df$neutral
df

# > df
#    language_iso_code negative neutral positive               language total
# 1                amh     3273    4104     2103                Amharic  9480
# 2                arq     1590     582      851  Algerian Arabic/Darja  3023
# 3                ary     2652    3317     3069 Moroccan Arabic/Darija  9038
# 4                hau     7226    7597     7329                  Hausa 22152
# 5                ibo     4013    6940     4762                   Igbo 15715
# 6                kin     1788    1965     1402            Kinyarwanda  5155
# 7                orm      948    1023      523                  Oromo  2494
# 8                pcm     6380     524     3652        Nigerian Pidgin 10556
# 9              pt-MZ     1633    4379     1480  Mozambican Portuguese  7492
# 10               swa      319    1784      908                Swahili  3011
# 11               tir     1185     509      704               Tigrinya  2398
# 12               tso      446     214      601               Xitsonga  1261
# 13               twi     1815     726     2277                    Twi  4818
# 14               yor     3296    5487     6344                 Yorùbá 15127

# Level decreasing by total numbers
total <- df$total %>% order(decreasing = T) %>% df$language[.]

# Level decrasing by percentage of each sentiments
positive <- (df$positive/df$total) %>% order(decreasing = T) %>% df$language[.];positive
negative <- (df$negative/df$total) %>% order(decreasing = T) %>% df$language[.];negative
neutral <- (df$neutral/df$total) %>% order(decreasing = T) %>% df$language[.];neutral

# tidy data
afri <- df %>% select(c(language,positive,negative,neutral)) %>% gather(key = 'key',value = 'value',colnames(.)[-1])


# Function for bar plot
titlefontsize <- 18 # change font size
axistextsize <- 15 # change text size
legendtextsize <- 15 # change legend size
func_ggplot_bar <- function(afri,position,level,plottitle){
  sentiment <- c('positive','neutral','negative') # named it sentiment + i saw that this function for ggplot can have pos, level,and plottitle all for the same dataset in one row
  afri$language <-  afri$language %>% factor(levels = eval(parse(text = level)) %>% rev()) # $ means to extract or subset a specific part of a data object in R
  afri$key <-  afri$key %>% factor(levels = c(sentiment[!sentiment %in% level],sentiment[sentiment %in% level]))
  print( afri$key)
  
### TIME TO PLOT ###
  g <- 
    afri %>% # piping the dataset into ggplot
    ggplot(mapping = aes(x = language,y = value,fill = key)) + # ggplot w/ mapping, language is x and value is y
    geom_bar(position = position, stat="identity") + # gives me position of the value as a static
    coord_flip() + # inverting the x and y 
    theme_minimal() + # minimal theme
    theme(legend.title = element_blank(), # want a legend
          axis.title = element_blank(), # want a title
          axis.text = element_text(size = axistextsize), # text size is important
          plot.title = element_text(size = titlefontsize), # title plot must be same fit size
          legend.text = element_text(size = legendtextsize)) + # legend text size is important
    scale_fill_manual(values = c('positive' = 'pink','negative' = 'maroon','neutral' = 'purple')) + # colors for my values
    labs(title = plottitle) # my plot title is my main title for the plot
  return(g)
}

# Creating charts
gg <- list()

gg[[1]] <- 
  func_ggplot_bar(afri =  afri,position = 'stack',level = 'total', # stacking the position of my vector for my afri dataset
                  plottitle = 'Tweet Samples Shown in Descending Order') # OVERVIEW TITLE

  scale_y_continuous(labels = function(x)paste0(x*100,' %')) # making my y scaled continuous

layout_matrix <- rbind(c(1,1,1),c(2,3,4))
g <- arrangeGrob(grobs = gg,
                 ncol = 3, # 3 columns
                 nrow = 2, # 2 rows
                 layout_matrix = layout_matrix,
                 widths = c(1,1,1), 
                 heights = c(1,1),
                 top = textGrob('The Overview of African Language Sentiment\n',
                                gp=gpar(fontsize = 25))) %>% as_ggplot()
g


ggsave(here("Third Tidy Tuesday", "Output", "ThirdTidyTuesdayAssignment.png"))
            
