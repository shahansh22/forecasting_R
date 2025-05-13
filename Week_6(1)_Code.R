library("tm")
library("tokenizers")
library("SnowballC")
library("textstem")
library("gutenbergr")
text <- c("Now, I truly understand that because and Because it's an election campaigning in the middle of the year",
          "expectations for what we will achieve this year are low.",
          "But, Mister Speaker, I appreciate the constructive approach",
          "that you and other leaders took at the end of last year",
          "to pass a budget and make tax cuts permanent for working",
          "families. So I hope we can work together this year on some",
          "bipartisan priorities like criminal justice reform and",
          "helping people who are battling prescription drug abuse",
          "and heroin abuse. So, who knows, we might surprise the",
          "cynics again")

#Corpus : collection of text docs
docs <- Corpus(VectorSource(text))
print(docs[[1]]$content)

#Toeknization
words_token <- tokenize_words(docs$content)
print(words_token[1])

#to_lowercase
docs2 <- tm_map(docs, content_transformer(tolower))
print(docs[[1]]$content)
print(docs2[[1]]$content)

#Remove stop words
docs2 <- tm_map(docs2, removeWords, stopwords("english"))
print(docs[[1]]$content)
print(docs2[[1]]$content)

#Stemming
text_stem <- tm_map(docs2, stemDocument)
print(text_stem [[1]]$content)

#Lemmatization
text_lemma <- tm_map(docs2, lemmatize_strings)
print(text_lemma[[1]]$content)

#grep,grepl,gsub,sub
sentences = c("I like statistics", "I like bananas", "Estates and statues are expensive")
grep("stat", sentences)
grepl("stat", sentences)
sentences = c("I like statistics and I study a lot", "I like bananas", "Estates and statues are expensive")
sub(pattern = "I", replacement = "You", sentences)
gsub(pattern = "I", replacement = "You", sentences)

#Doc term matrix
Plato <- gutenberg_download(150)
colnames(Plato) <- c('doc_id','text')
platocorpus <- SimpleCorpus(VectorSource(Plato$text))
dtm <- DocumentTermMatrix(platocorpus, control = list(
  removePunctuation = TRUE, removeNumbers = TRUE, 
  stopwords =  TRUE, tolower = TRUE, 
  wordLengths=c(1,Inf)))
dtm

#Most common
findFreqTerms(dtm,200)

#Most associated
findAssocs(dtm, "philosophy", corlimit = 0.1) 

#Remove rare terms
dtms <- removeSparseTerms(dtm, 0.99)
dtms
findFreqTerms(dtms)

library("wordcloud")
freq = data.frame(sort(colSums(as.matrix(dtms)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

#DTM - Inverse doc freq
dtmtfidf <- DocumentTermMatrix(platocorpus,
                               control = list( weighting =  weightTfIdf, removePunctuation = TRUE, removeNumbers = TRUE, stopwords =  TRUE, 
                                               tolower = TRUE, wordLengths=c(1,Inf)))

freq = data.frame(sort(colSums(as.matrix(dtmtfidf)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))
