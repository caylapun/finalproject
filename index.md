---
title: "Final Project"
author: "Cayla Pun"
output: html_notebook
date: December 14, 2020
---


```{r}
# loading libraries and "stop_words"
library(rvest)
library(tidyverse)
library(genius)
library(qdap)
library(tidytext)
library(mosaic)
library(lubridate)
library(esquisse)
data("stop_words")
```

```{r}
#read webpage for Grammy Awards
webpage <- read_html("https://en.wikipedia.org/wiki/Grammy_Award_for_Record_of_the_Year")
```


# **DATA PREP**

* 1980 data prep
```{r}
#regex to pull 1980 data
XPATH1980 <- '/html/body/div[3]/div[3]/div[5]/div[1]/table[5]' 
#creating table using regex
table_1980 <- 
  webpage %>%
  html_nodes(xpath = XPATH1980) %>%
  html_table(fill = TRUE)
#assigning table to data frame
D1980 <- table_1980[[1]]
#data wrangling: renaming columns, removing footnotes and na entries, creating a decade column
D1980 <- 
  D1980 %>% 
  drop_na() %>%
  rename(year = 'Year[I]', track = Record, artist = 'Artist(s)') %>%
  mutate(year = gsub(pattern = "(\\[\\d+\\])", replacement = "", x = year)) %>%
  mutate(decade = "1980s")
```


* 1990 data prep
```{r}
#regex to pull 1990 data
XPATH1990<-'/html/body/div[3]/div[3]/div[5]/div[1]/table[6]'
#creating table using regex
table_1990 <- 
  webpage %>%
  html_nodes(xpath = XPATH1990) %>%
  html_table(fill = TRUE)
#assigning table to data frame
D1990 <- table_1990[[1]]
#data wrangling: renaming columns, removing footnotes and na entries, creating a decade column
D1990 <- 
  D1990 %>% 
  drop_na() %>%
  rename(year = 'Year[I]', track = Record, artist = 'Artist(s)') %>%
  mutate(year = gsub(pattern = "(\\[\\d+\\])", replacement = "", x = year)) %>%
  mutate(decade = "1990s")
```


* 2000 data prep
```{r}
#regex to pull 2000 data
XPATH2000<- '/html/body/div[3]/div[3]/div[5]/div[1]/table[7]'
#creating table using regex
table_2000 <- 
  webpage %>%
  html_nodes(xpath = XPATH2000) %>%
  html_table(fill = TRUE)
#assigning table to data frame
D2000 <- table_2000[[1]]
#data wrangling: renaming columns, removing footnotes and na entries, creating a decade column
D2000 <- 
  D2000 %>% 
  drop_na() %>%
  rename(year = 'Year[I]', track = Record, artist = 'Artist(s)') %>%
  mutate(year = gsub(pattern = "(\\[\\d+\\])", replacement = "", x = year)) %>%
  mutate(decade = "2000s")
```


* 2010 data prep
```{r}
#regex to pull 2010 data
XPATH2010 <- '/html/body/div[3]/div[3]/div[5]/div[1]/table[8]'
#creating table using regex
table_2010 <- 
  webpage %>%
  html_nodes(xpath = XPATH2010) %>%
  html_table(fill = TRUE)
#assigning table to data frame
D2010 <- table_2010[[1]]
#data wrangling: renaming columns, removing footnotes and na entries, creating a decade column
D2010 <- 
  D2010 %>% 
  drop_na() %>%
  rename(year = 'Year[I]', track = Record, artist = 'Artist(s)') %>%
  mutate(year = gsub(pattern = "(\\[\\d+\\])", replacement = "", x = year)) %>%
  mutate(decade = "2010s")
```


* all decades
```{r}
#data frame containing all decades
All <-
  D1980 %>%
  full_join(D1990) %>%
  full_join(D2000) %>%
  full_join(D2010)
```


* lyrics for all decades
```{r}
#adding lyrics
Alllyrics <- All %>% 
  add_genius(artist, track, type = "lyrics")
#lyrics before filtering out stop words and additional stop words
AllWords <-
  Alllyrics %>%
  unnest_tokens(word,lyric)
#removing stop words from all words
AllWords_stopwords <-
  AllWords %>%
  anti_join(stop_words, by = "word")
#removing additional stop words
AllWords_Final <-
  AllWords_stopwords %>%
  filter(word != 'ba', word != 'du',word != 'yeah',word != 'da',
         word != 'ya',word != 'ooh',word != 'gonna',word != 'na',
         word != 'uh',word != 'la',word !='hol') 
```


# **GRAPHS**

#### 1. 

* data prep for graph 1
```{r}
#words per song dataframe ->idea adding every row and group by year
Wps <-
  AllWords %>%
  mutate(num = 1) %>% #assigning a 1 to every row
  mutate(decade = as.character(decade)) %>% #ensuring the decade is of type character
  group_by(track, decade, artist, year) %>% 
  summarise(num = sum(num)) %>% #adding up the number of words
  select(year, num, track, artist)
```
* graph: 
```{r}
ggplot(Wps) +
  aes(x = decade, y = num, fill = decade) +
  geom_boxplot() +
  scale_fill_brewer(palette = "BuPu") +
labs(x = "Words per Song", y = "Decade", title = "Boxplots of Words per Grammy Nominated Song by Decade ") +
  theme_minimal()
```

#### 2. 
* graph: 
```{r}
Top_all <- freq_terms(AllWords_Final$word, 10, at.least=3) #top 10 words overall
ggplot(Top_all) +
  aes(x = reorder(WORD, desc(FREQ)), weight = FREQ) +
  geom_bar(fill = "#FF5733") +
  labs(x = "Word", y = "Count", title = "Ten Most Popular Words of Grammy Nominated Songs from 1980 - 2019") +
  theme_minimal()
```

#### 3.

* data prep for graph 3
```{r}
#1980
Decade_1980 <-
  AllWords_Final %>%
  filter(decade == "1980s") 
Top10_1980 <- freq_terms(Decade_1980$word, 10, at.least=3)
g1 <- ggplot(Top10_1980) +
  aes(x = reorder(WORD, desc(FREQ)), weight = FREQ) +
  geom_bar(fill = "#ffe0ba") +
  labs(x = "Word", y = "Count", title = "1980s") +
  theme_minimal()
#1990
Decade_1990 <-
  AllWords_Final %>%
  filter(decade == "1990s")
Top10_1990 <- freq_terms(Decade_1990$word, 10, at.least=3)
g2 <- ggplot(Top10_1990) +
  aes(x = reorder(WORD, desc(FREQ)), weight = FREQ) +
  geom_bar(fill = "#d1f5b8") +
  labs(x = "Word", y = "Count", title = "1990s") +
  theme_minimal()
#2000
Decade_2000 <-
  AllWords_Final %>%
  filter(decade == "2000s")
Top10_2000 <- freq_terms(Decade_2000$word, 10, at.least=3)
g3 <- ggplot(Top10_2000) +
  aes(x = reorder(WORD, desc(FREQ)), weight = FREQ) +
  geom_bar(fill = "#b8d4f5") +
  labs(x = "Word", y = "Count", title = "2000s") +
  theme_minimal()
#2010
Decade_2010 <-
  AllWords_Final %>%
  filter(decade == "2010s")
Top10_2010 <- freq_terms(Decade_2010$word, 10, at.least=3)
g4 <- ggplot(Top10_2010) +
  aes(x = reorder(WORD, desc(FREQ)), weight = FREQ) +
  geom_bar(fill = "#c4c4ff") +
  labs(x = "Word", y = "Count", title = "2010s") +
  theme_minimal()
```
* graph: 
```{r}
#installing library needed to join graphs
library(gridExtra)
graph3 <- grid.arrange(g1, g2, g3,g4,
                             top="Top Ten Words by Decade",
                             ncol = 2, nrow = 2)
graph3
```


#### 4.
* data prep for graph 4
```{r}
#combining decades
WordSentiments <-
  Decade_1980 %>%
  full_join(Decade_1990) %>%
  full_join(Decade_2000) %>%
  full_join(Decade_2010) 
#joining sentiments 
WordSentiments <-   
  left_join(WordSentiments, sentiments) %>% 
  drop_na()
#casting positive and negatives to 1 and 0
WordSentiments$sentiment[WordSentiments$sentiment == "positive"] <- 1
WordSentiments$sentiment[WordSentiments$sentiment == "negative"] <- 0
#creating and calculating net sentiment 
WordSentiments <-
  WordSentiments %>%
  group_by(year) %>%
  mutate(sentiment = as.integer(sentiment)) %>% 
  mutate(count = sum(sentiment))
```

* graph 4
```{r}
graph4 <- ggplot(WordSentiments) +
  aes(x = year, fill = decade, weight = count) +
  geom_bar() +
  scale_fill_brewer(palette = "BrBG") +
  labs(x = "Year", y = "Net Sentiment", title = "Net Sentiment Score by Year") +
  theme_minimal() 
graph4 <- graph4 + scale_x_discrete(breaks = c(1980,1990,2000,2010)) #creating breaks in the x axis
graph4
```

#### 5.
* data prep for graph 5
```{r}
MeanSentiments <-
  WordSentiments %>%
  mutate(deacde = as.character(decade)) %>% 
  group_by(decade) %>%
  mutate(mean = mean(sentiment)) #calculating the mean 
```

* graph 5
```{r}
graph5 <- ggplot(MeanSentiments) +
  aes(x = decade, fill = decade, group = decade, weight = mean) +
  geom_bar() +
  scale_fill_brewer(palette = "BuPu") +
  labs(x = "Decade", y = "Mean Sentiment Score", title = "Mean Sentiment Score by Decade") +
  theme_minimal()
graph5
```

#### 6.
```{r}
WordSentiments <-
  WordSentiments %>%
  mutate(year = as.integer(year)) #changing the year variable to integer
  
graph6 <- ggplot(WordSentiments) +
  aes(x = year, y = count, colour = decade) +
  geom_point(size = 1L) +
  geom_smooth(method="lm",color = "black",se = FALSE, fullrange=TRUE) +
  scale_color_brewer(palette = "RdYlBu") +
  labs(x = "Year", y = "Net Sentiment", title = "Net Sentiment Score by Year of Grammy Nominated Records from 1980- 2019 with Linear Model Fit") +
  theme_minimal()
graph6
```
### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/caylapun/finalproject/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.
