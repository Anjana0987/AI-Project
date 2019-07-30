library("dplyr")
library("tidytext")
library("ggplot2")
library("wordcloud")

sad_tweets <- read.csv('tweets_h.csv', stringsAsFactors = FALSE)

sad_tweets$cleanText <- gsub("http.*","",  sad_tweets$text)
sad_tweets$cleanText <- gsub("https.*","", sad_tweets$cleanText)

#### Calculating tf-idf for tweets ####

sad_tweets_tfidf <- sad_tweets %>%
  select(created_at,text) %>%
  unnest_tokens("word", text) %>%
  anti_join(stop_words) %>%
  count(word, created_at) %>%
  bind_tf_idf(word, created_at, n)

sad_tweets_tfidf <- sad_tweets_tfidf[-grep("amp|sad|co|https",
                                         sad_tweets_tfidf$word),]
sad_tweets_tfidf <- data.frame(sad_tweets_tfidf)
 

top_tfidf <- sad_tweets_tfidf %>%
  arrange(tf_idf)

#### wordcloud for tweets by tf-idf ####

top_tfidf %>%
  with(wordcloud(word, max.words = 100, scale=c(2,0.5)))

#### Histogram for tweets by tf-idf ####

ggplot(top_tfidf[1:25,], aes(word, tf_idf)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "tf-idf") + coord_flip()

