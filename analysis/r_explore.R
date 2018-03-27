library(plyr)
library(dplyr)
library(tidytext)
library(highcharter)
library(zoo)

#Changing working directory
setwd("/Users/mammarrahmani/Projects/Fritid/musictoo")

lyrics_df <- read.csv("data/lyrics.csv", stringsAsFactors = FALSE)
s_words   <- read.csv("data/sexist_words.csv", sep=";", stringsAsFactors = FALSE)

lyrics_word_df    <- lyrics_df %>% unnest_tokens(word, Lyrics)
sexist_lyrics_df  <- lyrics_word_df %>% filter(word %in% tolower(unlist(s_words[,2])))

#Create trend data set with sexistic word count statistics
ytrend  <- sexist_lyrics_df %>% group_by(Song, Artist, Year) %>% filter(row_number()==1) %>% group_by(Year) %>% summarise(count=n())
missing <- unique(lyrics_df$Year[!(lyrics_df$Year %in% ytrend$Year)])
ytrend  <- rbind(ytrend, data.frame(Year=missing, count=0))
ytrend  <- ytrend %>% arrange(Year) %>% mutate(rolling = rollmean(count, 4, align="right", fill=0))

#Plot
highchart() %>% hc_xAxis(categories=as.character(ytrend$Year)) %>%
  hc_add_series(data=ytrend$count, type="line", name="Year by Year") %>% 
  hc_add_series(data=ytrend$rolling, type="line", name="Rolling Mean 4 Years")