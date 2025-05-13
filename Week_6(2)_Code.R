library('stringr')
library('RColorBrewer')
library('topicmodels')
library('ggplot2')
library('LDAvis')
library('servr')
library("tm")
library("tokenizers")
library("SnowballC")
library("textstem")
library("gutenbergr")
library("dplyr")

data <- read.csv(file = "abcnews-date-text-sample.csv", header = TRUE)
news <- stringr::str_conv(data$headline_text, "UTF-8")

# Create Corpus
docs <- Corpus(VectorSource(news))

#DTM with term freq
dtmdocs <- DocumentTermMatrix(docs,
                              control = list(lemma=TRUE,removePunctuation = TRUE,
                                             removeNumbers = TRUE, stopwords = TRUE,
                                             tolower = TRUE))
#As a result of this cleaning, it is possible that some documents would have all tokens removed. 
#Therefore, we also remove them from the dtm in the code below.
raw.sum=apply(dtmdocs,1,FUN=sum)
dtmdocs=dtmdocs[raw.sum!=0,]

#Most common
dtm.new <- as.matrix(dtmdocs)
frequency <- colSums(dtm.new)
frequency <- sort(frequency, decreasing=TRUE)
doc_length <- rowSums(dtm.new)

frequency[1:10]

words <- names(frequency)# get back the word

library("wordcloud")
wordcloud(words[1:100], frequency[1:100], rot.per=0.15, 
          random.order = FALSE, scale=c(4,0.5),
          random.color = FALSE, colors=brewer.pal(8,"Dark2"))

#LDA
library("ldatuning")
result <- FindTopicsNumber(
  dtm.new,
  topics = seq(from = 5, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

#Topic Modelling: Latent Dirichlet Allocation
ldaOut <-LDA(dtmdocs,13, method="Gibbs", 
             control=list(iter=1000,seed=1000))
phi <- posterior(ldaOut)$terms %>% as.matrix 
#matrix, with each row containing the distribution over terms for a topic,
theta <- posterior(ldaOut)$topics %>% as.matrix 
#matrix, with each row containing the probability distribution over topics for a document,

# Which highest alpha 'term' is part of which topics
ldaOut.terms <- as.matrix(terms(ldaOut, 10))
ldaOut.terms
# Which 'topic' is the review in (highest probability)
ldaOut.topics <- data.frame(topics(ldaOut))
ldaOut.topics$index <- as.numeric(row.names(ldaOut.topics))
data$index <- as.numeric(row.names(data))
datawithtopic <- merge(data, ldaOut.topics, by='index',all.x=TRUE)
datawithtopic <- datawithtopic[order(datawithtopic$index), ]
datawithtopic[0:10,]

topicProbabilities <- as.data.frame(ldaOut@gamma)
topicProbabilities[0:10,1:5]

#Visualisation
vocab <- colnames(phi) #vocab list in DTM

# create the JSON object to feed the visualization in LDAvis:
json_lda <- createJSON(phi = phi, theta = theta, 
                       vocab = vocab, doc.length = doc_length, 
                       term.frequency = frequency)


serVis(json_lda, out.dir = 'vis', open.browser = TRUE)
