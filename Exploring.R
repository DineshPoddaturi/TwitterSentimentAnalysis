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
api_key <- "yourAPIKey"
api_secret_key <- "yourAPISecretKey"
access_token <- "yourAccessToken"
access_token_secret <- "yourAccessTokenSecret"

### The following code setup direct authentication to access twitter
setup_twitter_oauth(api_key, api_secret_key, access_token, access_token_secret)

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

search_term <- "#recession"
by <- 'hour'

# tweets <- search_tweets(q = search_term , 
#                         n = 100000, retryonratelimit = TRUE, lang="en", include_rts = FALSE)

tweets <- searchTwitter(searchString = search_term, n=100000, lang="en")

tweetsDF <- twListToDF(tweets)

names(tweetsDF)

tweetsDF <- tweetsDF %>% filter(isRetweet == "FALSE")

tweetsSubSet <- tweetsDF %>% select(created, text)
nrow(tweetsSubSet)

# Ignore graphical Parameters to avoid input errors
tweetsSubSet$text <- str_replace_all(tweetsSubSet$text,"[^[:graph:]]", " ")

tweetsSubSet$text <- cleanText(tweetsSubSet$text)

tweetsSubSet %>% head() 

tweetsSubSet <- tweetsSubSet %>% 
  mutate(Created_At_Round = created %>% round(units = 'hours') %>% as.POSIXct())

tweetsSubSet %>% pull(created) %>% min()

tweetsSubSet %>% pull(created) %>% max()

tweetHourPlot <- tweetsSubSet %>% 
  count(Created_At_Round) %>% 
  ggplot(mapping = aes(x = Created_At_Round, y = n)) +
  theme_economist() +
  geom_point() + 
  geom_line() +
  xlab(label = 'Date') +
  ylab(label = 'Count') +
  labs(title = paste0('Tweets mentioning "',
                         search_term,'" by hour') , caption = 'Source: Twitter API')

# tweetHourPlot %>% ggplotly()

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
  geom_histogram(binwidth = 1, fill = "olivedrab")+ 
  ylab("Frequency") + 
  xlab("Sentiment score") +
  ggtitle("Distribution of Sentiment scores of the tweets") +
  theme_classic() + 
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
  ggtitle("Barplot of Sentiment for recession")+
  theme_classic() + 
  ggeasy::easy_center_title()



textCorpus <- Corpus(VectorSource(tweetsSubSet$text))
textCorpus <- tm_map(textCorpus, content_transformer(tolower))
textCorpus <- tm_map(textCorpus, function(x)removeWords(x,stopwords("english")))
textCorpus <- tm_map(textCorpus, removeWords, c("recession", "will", "can"))
termDocMat <- TermDocumentMatrix(textCorpus)
termDocMat <- as.matrix(termDocMat)
termDocMat <- sort(rowSums(termDocMat), decreasing = TRUE)
termDocMat <- data.frame(word = names(termDocMat), freq = tdm)
set.seed(123)

wordcloud(textCorpus, min.freq = 5, max.words = 200, 
          colors=brewer.pal(8, "Dark2"), random.color = T, random.order = F)


ggplot(tdm[1:20,], aes(x=reorder(word, freq), y=freq, fill = word)) + 
  geom_bar(stat="identity") +
  xlab("Terms") + 
  ylab("Count") + 
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  ggtitle('Most common word frequency plot') +
  ggeasy::easy_center_title() +
  theme(legend.position="none")





