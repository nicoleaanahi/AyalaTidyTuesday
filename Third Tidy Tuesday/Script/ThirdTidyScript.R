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


### Load Data ###

tuesdata <- tidytuesdayR::tt_load('2023-02-28') # literally loading the data

afrisenti <- tuesdata$afrisenti # access list tuesdat$afrisenti as solely afrisenti
languages <- tuesdata$languages # name tuesdata$languages list as languages
language_scripts <- tuesdata$language_scripts # name tuesdata$language_scripts as language_scripts
language_countries <- tuesdata$language_countries # name  tuesdata$language_countries as language_countries
country_regions <- tuesdata$country_regions # name  tuesdata$country_regions as country_regions

### Glimpse ###
afrisenti %>% glimpse()
languages %>% glimpse()
language_scripts %>% glimpse()
language_countries %>% glimpse()
country_regions %>% glimpse()

afrisenti %>% apply(MARGIN = 2,FUN = function(x)sum(is.na(x))) # removing the NAs

### Make a count table ###
tmp <- aggregate(x = afrisenti$label,by = list(afrisenti$language_iso_code),FUN = function(x)table(x))
df0 <- tmp[,2] %>% data.frame() %>% cbind('language_iso_code' = tmp[,1],.) # changing the language to English
df1 <- join(df0,languages) # need to conjoin languages and recently translated data
df1$total <- df1$negative + df1$positive + df1$neutral # the total data will be composed on negative, positive and neutral variables


### Level decreasing by total numbers ###
total <- df1$total %>% order(decreasing = T) %>% df1$language[.]

### Level decrasing by percentage of each sentiments ###
positive <- (df1$positive/df1$total) %>% order(decreasing = T) %>% df1$language[.];positive
negative <- (df1$negative/df1$total) %>% order(decreasing = T) %>% df1$language[.];negative
neutral <- (df1$neutral/df1$total) %>% order(decreasing = T) %>% df1$language[.];neutral

### CLEAN THE DATA ###
cleanafridf <- df1 %>% select(c(language,positive,negative,neutral)) %>% gather(key = 'key',value = 'value',colnames(.)[-1]) # just cleaning up the data by grouping our values together and our keys together


### BAR PLOT... POTENTIALLY ###
### SETTING UP PARAMETERS FOR BARPLOT ###
# Function for bar plot
titlefontsize <- 20
axistextsize <- 10
legendtextsize <- 10
func_ggplot_bar <- function(cleanafridf,position,level,plottitle){
  sentiment <- c('positive','neutral','negative')
  cleanafridf$language <- cleanafridf$language %>% factor(levels = eval(parse(text = level)) %>% rev())
  cleanafridf$key <- cleanafridf$key %>% factor(levels = c(sentiment[!sentiment %in% level],sentiment[sentiment %in% level]))
  print(cleanafridf$key)
g <-
cleanafridf %>% 
           ggplot(mapping = aes(x = language,
                                y = value,fill = key)) + 
           geom_bar(position = position,stat="identity") + 
           coord_flip() + 
           theme_minimal() + 
           theme(legend.title = element_blank(),
                 axis.title = element_blank(),
                 axis.text = element_text(size = axistextsize),
                 plot.title = element_text(size = titlefontsize),
                 legend.text = element_text(size = legendtextsize)) +
           scale_fill_manual(values = c('positive' = 'pink','negative' = 'maroon','neutral' = 'purple')) + 
           labs(title = "plottitle")
return(g)
}

afrisentiplot <- list(g)
afrisentiplot[[1]] <- 
  func_ggplot_bar(cleanaridf = cleanafridf,position = 'fill',level = 'total',
                  plottitle = 'Descending order by total number of tweet samples') +
afrisentiplot[[2]] <- 
  func_ggplot_bar(cleanafridf = cleanafridf,position = 'fill',level = 'positive',
                  plottitle = 'Descending order\n by percentage of positive tweets') +
  scale_y_continuous(labels = function(x)paste0(x*100,' %'))
afrisentiplot[[3]] <- 
  func_ggplot_bar(cleanafridf = cleanafridf,position = 'fill',level = 'neutral',
                  plottitle = 'Descending order\n by percentage of neutral tweets') + 
  scale_y_continuous(labels = function(x)paste0(x*100,' %'))
afrisentiplot[[4]] <- 
  func_ggplot_bar(cleanafridf = cleanafridf,position = 'fill',level = 'negative',
                  plottitle = 'Descending order\n by percentage of negative tweets') + 
  scale_y_continuous(labels = function(x)paste0(x*100,' %'))
  layout_matrix <- rbind(c(1,1,1),c(2,3,4))

afriplot <- arrangeGrob(grobs = afrisentiplot,
                 ncol = 3,
                 nrow = 2,
                 layout_matrix = layout_matrix,
                 widths = c(1,1,1),
                 heights = c(1,1),
                 top = textGrob('African Language Sentiment\n',
                                gp=gpar(fontsize = 25))) %>% as_ggplot()
afriplot
# Chart output/チャートの出力
setwd('D:/tidytuesday/');getwd()
png(paste0("tidytuesday-2023-02-28-african-language-sentiment.png"),width = 1500,height = 1000)
g %>% print()
dev.off()

