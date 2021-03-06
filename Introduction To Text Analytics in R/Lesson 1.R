#1.1 Intro 
library(tidyverse)
setwd("P:/Text Mining")
mydata <- read_csv("Roomba Reviews.csv")
mydata
# average stars for one product progucts
mydata %>%
  filter(Product == "iRobot Roomba 650 for Pets")%>%
  summarize(stars_mean = mean(Stars))

# for all products
mydata %>%
  group_by(Product)%>%
  summarize(m=mean(Stars))

# The same just for more practice

# Start with the data frame
twitter_data %>% 
  # Group the data by whether or not the tweet is a complaint
  group_by(complaint_label) %>% 
  # Compute the mean, min, and max follower counts
  summarize(
    avg_followers = mean(usr_followers_count),
    min_followers = min(usr_followers_count),
    max_followers = max(usr_followers_count)
  )


#1.2 Counting the categorical data
mydata %>%
  summarise(num_row=n())

mydata %>%
  group_by(Product)%>%
  summarise(num_row=n())

# the function count has the identical result
# The one difference is that the column with th actual counts is named n by default, a reference to the fact that the m() function is being used by count() in the background
mydata %>%
  count(Product)

mydata %>%
  count(Product)%>%
  arrange(desc(n))
#Note that n does not require the argument while the count need to have and categorical argument


# 1.3 Tokenizing and cleaning
#Tidytext


# Some natural language processing (NLP) vocabulary:
# Bag of words: Words in a document are independent
# Every separate body of text is a document
# Each unique word is a term
# Every occurrence of a term is a token
# Creating a bag of words is called tokenizing

# unnest_tokens()
library(tidytext)

tidy_review <- mydata %>%
  unnest_tokens(word, Review)
tidy_review


tidy_review%>%
  count(word)%>%
  arrange(desc(n))
tidy_review2 <-tidy_review %>%
  unnest_tokens(word, Review)%>%
  anti_join(stop_words)


