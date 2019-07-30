library("tidytext")
library("dplyr")
library("ggplot2")
library("wordcloud")
library("rlist")
data("stop_words")

sad_tweets <- read.csv('tweets_h.csv', stringsAsFactors = FALSE)

#### For the tweet ####

sad_tweets$cleanText <- gsub("http.*","",  sad_tweets$text)
sad_tweets$cleanText <- gsub("https.*","", sad_tweets$cleanText)

sad_tweets_clean <- sad_tweets %>%
  dplyr::select(cleanText) %>%
  unnest_tokens(word, cleanText)

sad_tweet_words <- sad_tweets_clean %>%
  anti_join(stop_words)

sad_tweet_words <- sad_tweet_words[-grep("amp|sad",
                              sad_tweet_words$word),]
sad_tweet_words <- data.frame(sad_tweet_words)

#### Histogram for tweets by max count ####

sad_tweet_words %>%
  count(sad_tweet_words, sort = TRUE) %>%
  top_n(25) %>%
  mutate(sad_tweet_words = reorder(sad_tweet_words, n)) %>%
  ggplot(aes(x = sad_tweet_words, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count of the Words",
       x = "Unique words",
       title = "BY MAX COUNT")

#### wordcloud for tweets by max count ####

sad_tweet_words %>%
  count(sad_tweet_words) %>%
  with(wordcloud(sad_tweet_words, n, max.words = 100, scale=c(1,0.5)))

#### For the hashtags ####

sad_tweets$c_tags <- gsub("http.*","",  sad_tweets$hashtags)
sad_tweets$c_tags <- gsub("https.*","", sad_tweets$c_tags)

sad_tweets_c_tags <- sad_tweets %>%
  dplyr::select(c_tags) %>%
  unnest_tokens(word, c_tags)

sad_tweet_tags <- sad_tweets_c_tags %>%
  anti_join(stop_words)

sad_tweet_tags <- sad_tweet_tags[-grep("amp|sad",
                                         sad_tweet_tags$word),]
sad_tweet_tags <- data.frame(sad_tweet_tags)

#### Plotting tthe histogram for hashtags####

sad_tweet_tags %>%
  count(sad_tweet_tags, sort = TRUE) %>%
  top_n(25) %>%
  mutate(sad_tweet_tags = reorder(sad_tweet_tags, n)) %>%
  ggplot(aes(x = sad_tweet_tags, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count of the Words",
       x = "Unique words",
       title = "By Hashtags")

#### Wordcloud for Hashtags ####

sad_tweet_tags %>%
  count(sad_tweet_tags) %>%
  with(wordcloud(sad_tweet_tags, n, max.words = 100, scale=c(1.7,0.5)))


