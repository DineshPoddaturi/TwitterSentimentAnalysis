
librarian::shelf(tm, SnowballC, wordcloud, readtext, pdftools)


wordbase <- readtext("DineshPoddaturi-ProposalPacket.pdf")

print(wordbase)

wordbase[1, 2]

corp <- Corpus(VectorSource(wordbase))

corp <- tm_map(corp, PlainTextDocument)

corp <- tm_map(corp, removePunctuation)

corp <- tm_map(corp, removeNumbers)

corp <- tm_map(corp, tolower)

corp <- tm_map(corp, removeWords, stopwords(kind = "en"))


wordcloud(corp, max.words = 100, random.order = FALSE)



txt <- pdf_text("DineshPoddaturi-ProposalPacket.pdf")
docs <- Corpus(VectorSource(txt))

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

d %>% filter(freq>=50)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


wordsDiss <- c(replicate(315, "economic"), replicate(300, "expectations"), 
               replicate(290, "dynamic"), replicate(280, "structural"),
               replicate(270, "model"), replicate(260, "price"), 
               replicate(250, "consumer"), replicate(240, "dynamics"),
               replicate(230, "production"), replicate(220, "supply"),
               replicate(220, "demand"), replicate(200, "data"),
               replicate(190, "impacts"), replicate(190, "expected"),
               replicate(200, "equilibrium"), replicate(150, "numerical"),
               replicate(180, "rational"), replicate(100, "industry"),
               replicate(140, "solve"), replicate(130, "algorithm"), 
               replicate(120, "distribution"), replicate(110, "equations"),
               replicate(90, "parameters"), replicate(92, "solution"),
               replicate(105, "behavior"), replicate(95, "estimation"),
               replicate(50, "interpolation"),replicate(94, "literature"),
                replicate(30, "inventories"),replicate(100, "non-linear"),
               replicate(160, "decisions"), replicate(125, "numerical"), 
               replicate(90, "quantities"), replicate(91, "future"),
               replicate(89, "observed"), replicate(89, "projected"), 
               replicate(85, "confidence"), replicate(83, "process"),
               replicate(80, "variables"),replicate(78, "fitted"),
               replicate(75, "developed"), replicate(265, "markets"), 
               replicate(74, "constructed"), replicate(70, "value"),
               replicate(65, "year"), replicate(60, "available"), 
               replicate(62, "annual"), replicate(160, "naïve"),
               replicate(60, "changes"), replicate(78, "assume"),
               replicate(60, "framework"), replicate(165, "exogenous"),
               replicate(160, "endogenous"), replicate(100, "competitive"),
               replicate(58, "response"), replicate(83, "system")) %>% as.data.frame()

names(wordsDiss) <- "words"

wordsDiss <- wordsDiss %>% group_by(words) %>% tally() %>% as.data.frame()


wordcloud(words = wordsDiss$words, freq = wordsDiss$n, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))








