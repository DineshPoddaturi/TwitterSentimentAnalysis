librarian::shelf(tidyverse, reshape2, readxl, data.table, nleqslv, BB, Metrics, ggthemes, pracma,
                 twitteR, ROAuth, hms, lubridate, tidytext, tm, wordcloud, igraph, glue, networkD3,
                 rtweet, stringr, ggeasy, plotly, janeaustenr, widyr)
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

library(openssl)
library(httpuv)
library(rtweet)

#Note: Replace below with your credentials following above reference
api_key <- "oJ6CmPerh7jHxP5ZHazRuez1U"
api_secret_key <- "zVKRrtlXKdWBPsFKaBs4h6z9cS0nQrZwoRI6dmhfDcdV3qDS20"
access_token <- "3914872158-vYvnOj6VE5ZpXcexVAu3YcMPh54uVdtfu4LLGVk"
access_token_secret <- "jhC9VDAZNbFj4mpPoWbRU3eq1KCrghMqIeiMpkCylcG8i"
#Note: This will ask us permission for direct authentication, type '1' for yes:
# setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
library(rtweet)

## authenticate via web browser
token <- create_token(
  app = "RangoUnchained",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

token

search_term <- '#War'
by <- 'hour'


tweets <- search_tweets(search_term, n = 10000, retryonratelimit = TRUE)

tweets <- searchTwitter("#globalwarming", n=4000, lang="en")






