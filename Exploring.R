librarian::shelf(tidyverse, reshape2, readxl, data.table, nleqslv, BB, Metrics, ggthemes, pracma,
                 twitteR, ROAuth, hms, lubridate, tidytext, tm, wordcloud, igraph, glue, networkD3,
                 rtweet, stringr, ggeasy, plotly, janeaustenr, widyr, textdata)

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

## pre-processing text: This function is to clean the text of the tweet of 
## any symbols, and other unnecessary items that are non-readable 
cleanText <- function(x){
  # convert to lower case
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # some other cleaning text
  x = gsub('https://','',x)
  x = gsub('http://','',x)
  x = gsub('[^[:graph:]]', ' ',x)
  x = gsub('[[:punct:]]', '', x)
  x = gsub('[[:cntrl:]]', '', x)
  x = gsub('\\d+', '', x)
  x = str_replace_all(x,"[^[:graph:]]", " ")
  return(x)
}

search_term <- '#inflation'
by <- 'day'

tweets <- search_tweets(q = "#inflation" , 
                        n = 10000, retryonratelimit = TRUE, lang="en", include_rts = FALSE)

tweets <- tweets %>% as.data.frame()
nrow(tweets)
names(tweets)
tweets[1,]

tweetsSubSet <- tweets %>% select(created_at, text, hashtags)
nrow(tweetsSubSet)

# Ignore graphical Parameters to avoid input errors
tweetsSubSet$text <- str_replace_all(tweetsSubSet$text,"[^[:graph:]]", " ")

tweetsSubSet$text <- cleanText(tweetsSubSet$text)

tweetsSubSet %>% head() 

tweetsSubSet <- tweetsSubSet %>% 
  mutate(Created_At_Round = created_at %>% round(units = 'hours') %>% as.POSIXct())

tweetsSubSet %>% pull(created_at) %>% min()

tweetsSubSet %>% pull(created_at) %>% max()

plt <- tweetsSubSet %>% 
  count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_light() +
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = NULL) +
  ggtitle(label = 'Number of Tweets per Hour')

plt %>% ggplotly()

### Retrieving positive and negative words
positive <- scan('OpinionLexiconEnglish/positive-words.txt', what = 'character', comment.char = ';')
negative <- scan('OpinionLexiconEnglish/negative-words.txt', what = 'character', comment.char = ';')

# add our list of words below as you wish if missing in above read lists
positiveWords <- c(positive,'upgrade','Congrats','prizes','prize','thanks','thnx',
              'Grt','gr8','plz','trending','recovering','brainstorm','leader')

negativeWords <- c(negative,'wtf','wait','waiting','epicfail','Fight','fighting',
              'arrest','no','not')

sentimentScore <- function(sentences, pos.words, neg.words){
  # require(plyr)
  # require(stringr)
  
  # we are giving vector of sentences as input. 
  # plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  # sentences = tweetsSubSet$text
  # pos.words = positiveWords
  # neg.words = negativeWords
  
  scores <- lapply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub() function:
    sentence <- gsub('https://','',sentence)
    sentence <- gsub('http://','',sentence)
    sentence <- gsub('[^[:graph:]]', ' ',sentence)
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- str_replace_all(sentence,"[^[:graph:]]", " ")
    # and convert to lower case:
    sentence <- tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list <- str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words <- unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    
    # TRUE/FALSE will be treated as 1/0 by sum():
    score <- sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words)
  
  scores <- unlist(scores)
  sentences <- unlist(sentences)
  
  scores.df <- data.frame(score=scores, text=sentences)
  
  return(scores.df)
}


analysisSentiment <- sentimentScore(sentences = tweetsSubSet$text, pos.words = positiveWords, 
                                    neg.words = negativeWords)

analysisSentiment %>%
  ggplot(aes(x=score)) + 
  geom_histogram(binwidth = 1, fill = "lightblue")+ 
  ylab("Frequency") + 
  xlab("sentiment score") +
  ggtitle("Distribution of Sentiment scores of the tweets") +
  ggeasy::easy_center_title()

neutral <- length(which(analysisSentiment$score == 0))
positive <- length(which(analysisSentiment$score > 0))
negative <- length(which(analysisSentiment$score < 0))
Sentiment <- c("Positive","Neutral","Negative")
Count <- c(positive,neutral,negative)
output <- data.frame(Sentiment,Count)
output$Sentiment<-factor(output$Sentiment,levels=Sentiment)
ggplot(output, aes(x=Sentiment,y=Count))+
  geom_bar(stat = "identity", aes(fill = Sentiment))+
  ggtitle("Barplot of Sentiment for inflation")

text_corpus <- Corpus(VectorSource(tweetsSubSet$text))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, function(x)removeWords(x,stopwords("english")))
text_corpus <- tm_map(text_corpus, removeWords, c("inflation"))
tdm <- TermDocumentMatrix(text_corpus)
tdm <- as.matrix(tdm)
tdm <- sort(rowSums(tdm), decreasing = TRUE)
tdm <- data.frame(word = names(tdm), freq = tdm)
set.seed(123)
wordcloud(text_corpus, min.freq = 1, max.words = 100, scale = c(2.2,1),
          colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)









###### A different type of analysis
sentimentData <- get_sentiments("afinn") %>% as.data.frame() %>% mutate(score=value) %>% 
  select(word,score) %>% arrange(score)

### Here we split the column into tokens
sentiments <- tweets[,3:5] %>% unnest_tokens(output = 'word', input = 'text')

mergedSentiments <- merge(sentiments, sentimentData, by = 'word')

mergedSentiments$hour <- format(round(mergedSentiments$created_at, units="hours"), format="%H:%M")
str(mergedSentiments)

mergedSentiments$day <- format(round(mergedSentiments$created_at, units="days"))

pivotMergedSentiments <- mergedSentiments %>% group_by(hour) %>% summarise(sentiment = mean(score)) %>% as.data.frame()

pivotMergedSentiments1 <- mergedSentiments %>% group_by(day) %>% summarise(sentiment = mean(score)) %>% as.data.frame()


pivotMergedSentiments %>% ggplot(aes(x=hour,y=sentiment)) + geom_line(group = 1) + geom_point() + theme_minimal()+ 
  labs(title = paste0('Average sentiment of tweetings mentioning "',search_term,'"'))

pivotMergedSentiments1 %>% ggplot(aes(x=day,y=sentiment)) + geom_line(group = 1) + geom_point() + theme_minimal()+ 
  labs(title = paste0('Average sentiment of tweetings mentioning "',search_term,'"'))
       
       

ggplot(pivotMergedSentiments[-1,], aes(x = hour, y = sentiment)) + 
  geom_line(group = 1) + geom_point() + theme_minimal() + 
  labs(title = paste0('Average sentiment of tweetings mentioning "',search_term,'"'),
       subtitle = paste0(pivotMergedSentiments$hour[2],' - ',pivotMergedSentiments$hour[nrow(pivotMergedSentiments)],' on ', 
                         format(mergedSentiments$created_at[1], '%d %B %Y')),
                                                                                                                x = 'Date', y = 'Sentiment', caption = 'Source: Twitter API')




##### Getting the sentiment score




#### Another way of getting tweets
# setup_twitter_oauth(api_key,
#                     api_secret_key,
#                     access_token,
#                     access_token_secret)
# search_term <- '#inflation'
# tweets1 <- searchTwitter(search_term, n = 1000)







# There are many resources describing methods to estimate sentiment. For the purpose of this tutorial, we will use a very simple algorithm which assigns sentiment score of the text by simply counting the number of occurrences of “positive” and “negative” words in a tweet.
# 
# Hu & Liu have published an “Opinion Lexicon” that categorizes approximately 6,800 words as positive or negative, which can be downloaded from this link:
#   
#   http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html

# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107


