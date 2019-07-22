library("tidytext")
library("dplyr")
library("ggplot2")
data("stop_words")
library(wordcloud)
library("rlist")

sad_tweets <- read.csv('tweets_h.csv', stringsAsFactors = FALSE)

sad_tweets$cleanText <- gsub("http.*","",  sad_tweets$text)
sad_tweets$cleanText <- gsub("https.*","", sad_tweets$cleanText)

sad_tweets_clean <- sad_tweets %>%
  dplyr::select(cleanText) %>%
  unnest_tokens(word, cleanText)

sad_tweet_words <- sad_tweets_clean %>%
  anti_join(stop_words)

sad_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count of the Words",
       x = "Unique words",
       title = "Count of unique words found in tweets")

sad_tweet_words %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150))


sad_tweets$c_tags <- gsub("http.*","",  sad_tweets$hashtags)
sad_tweets$c_tags <- gsub("https.*","", sad_tweets$c_tags)

sad_tweets_c_tags <- sad_tweets %>%
  dplyr::select(c_tags) %>%
  unnest_tokens(word, c_tags)

sad_tweet_tags <- sad_tweets_c_tags %>%
  anti_join(stop_words)

sad_tweet_tags %>%
  count(word, sort = TRUE) %>%
  top_n(25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count of the Words",
       x = "Unique words",
       title = "Count of unique words found in tweets")

sad_tweet_tags %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


