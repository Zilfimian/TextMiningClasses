# Chapter 2
# Plotting word counts
#2.1
setwd("P:/GitHub Desktop/TextMiningClasses/Introduction To Text Analytics in R")
list.files()
library(readr)
library(dplyr)

mydata <- read_csv("Roomba reviews.csv")
mydata

library(tidyverse)
library(tidytext)

tidy_data <- mydata %>%
  mutate(id=row_number())%>%
  unnest_tokens(word, Review)%>%
  anti_join(stop_words)

word_count <- tidy_data %>%
  count(word)%>%
  arrange(desc(n))

#ggplot(word_count, aes(word, y = n))+
 # geom_col()

word_count2 <- tidy_data %>%
  count(word)%>%
  filter(n>300)%>%
  arrange(desc(n))
# coord_flip() - new function   
ggplot(word_count2, aes(word, y = n))+
 geom_col()

ggplot(word_count2, aes(word, y = n))+
  geom_col()+
  coord_flip()+
  ggtitle("Vertical to horizontal")


#2.2
# tribble()
#Improving word count plots
stop_words

#custom the stop_words
custom_stop_word <-tribble(
  ~word, ~lexicon, 
  "roomba", "CUSTOM", 
  "2", "CUSTOM")

stop_words2 <- stop_words%>%
  bind_rows(custom_stop_word)
  
# remove the new stopwords
tidy_data2 <- mydata %>%
  mutate(id = row_number())%>%
  select(id, Date, Product, Stars, Review)%>%
  unnest_tokens(word, Review)%>%
  anti_join(stop_words2)

tidy3 <- tidy_data2 %>%
  filter(word == "roomba")

tidy3 # the word "roomda" has been removed

# FACTORS
#fct_recorder()

word_coun3 <- tidy_data2 %>%
  count(word)%>%
  filter(n>300)%>%
  mutate(word2 = fct_reorder(word,n))

word_coun3

ggplot(data = word_coun3, aes(x=word, y=n))+
  geom_col()+
  coord_flip()+
  ggtitle("Review Word Counts")
# with reordering
ggplot(data = word_coun3, aes(x=word2, y=n))+
  geom_col()+
  coord_flip()+
  ggtitle("Review Word Counts")

# 2.3 Faceting word count plots


tidy_data %>%
  count(word, Product)%>%
  arrange(word, n)

#top_n()

tidy_data %>%
  count(word, Product)%>%
  group_by(Product)%>%
  top_n(10,n)

# ungroup()

tidy_data %>%
  count(word, Product)%>%
  group_by(Product)%>%
  top_n(10,n)%>%
  ungroup() # removes the effects of group_by

# for comments
word_counts <- tidy_twitter %>%
  # Count words by whether or not its a complaint
  count(word, complaint_label) %>%
  # Group by whether or not its a complaint
  group_by(complaint_label) %>%
  # Keep the top 20 words
  top_n(20, n) %>%
  # Ungroup before reordering word as a factor by the count
  ungroup() %>%
  mutate(word2 = fct_reorder(word, n))

word_count4 <-   tidy_data %>%
  count(word, Product)%>%
  group_by(Product)%>%
  top_n(10,n)%>%
  ungroup()%>% # removes the effects of group_by
 mutate(word5 = fct_reorder(word,n))

ggplot( word_count4, aes( word5, n, fill = Product))+
  geom_col(show.legend = FALSE)+
  facet_wrap(~Product, scales = "free_y")+ #y can be different for each plot
  coord_flip()+
  ggtitle("something")


# 2.4
# Plotting word clouds
# wordcloud() is not part of tidytext,verse
library(wordcloud)

word_count

wordcloud::wordcloud(
  words = word_count$word,
  # because it is not he part of tidyverse the $ is used for base R
  freq = word_count$n,
  max.words = 30, colors = "blue"
  
  )

# while reruning the size of words remains the same but the location is changing
# size is based on relative word count which is fixed
# The location is randomized
