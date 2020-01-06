## Data for this project can be found at: https://www.dropbox.com/s/snd1af2dwdgwl1g/NCAsheville1.txt?dl=0

library(topicmodels)
library(readtext)
library(Matrix)
library(tm)
library(quanteda)

## Sets your R Studio instance to use greater resources on the machine, and thereby run faster:
if(.Platform$OS.type == "windows") withAutoprint({
  memory.size()
  memory.size(TRUE)
  memory.limit()
})


ash1 = read.delim("NCAsheville1.txt", header=TRUE)   # Reading in the data
asheville_df <- DataframeSource(ash1)   # Putting the data into a data frame
ac <- VCorpus(asheville_df)   # Putting the data into a corpus, per the TM library
#2#2 If you want to clean the data, do so now with the below code, before converting to a Quanteda Corpus...
corp_tm <- tm::VCorpus(tm::VectorSource(ac)) # Prepping for the VCorpus (TM) to Corpus (Quanteda) translation
ac <- corpus(corp_tm) # Putting the data into a corpus, per the quanteda library
ac

#2#2 Cleaning up the Data - ONLY works if using the TM VCorpus object, doesn't work if you've already translated to a Quanteda Corpus:

#ac<-tm_map(ac, content_transformer(tolower)) # Using the Lower case cleaning in the stopword removal sequence
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern = "\n", replacement =  " ")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern = "\f", replacement =  " ")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern = "rt ", replacement = "")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern = "@\\w+", replacement = "")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern = "[[:punct:]]", replacement =  " ")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern ="[[:digit:]]", replacement =  " ")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern = "^ ", replacement = "")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern = " $", replacement =  " ")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern ="[^[:alnum:][:space:]'-]", replacement =  " ")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern ="  ", replacement =  " ")))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern ="[^0-9A-Za-z///' ]", replacement ="'",ignore.case = TRUE)))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern ="''", replacement =" ",ignore.case = TRUE)))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern ="?", replacement ="",ignore.case = TRUE)))
ac<-tm_map(ac, content_transformer(function(x) gsub(x, pattern="\\b\\w{1,2}\\s",replacement =" ")))

library(stm)
library(wordcloud)

## Setting up custom stopwords with host names ##
#host_names_stopwords <- read.delim("host_names_stopwords.txt", header=TRUE)
#hns_df <- DataframeSource(host_names_stopwords)
#hnsc <- VCorpus(hns_df)   # Putting the data into a corpus, per the TM library
#hns_corpus <- corpus(hnsc)
hns_lines <- readLines("hns.txt", encoding = "UTF-8")
asheville_dfm <- dfm(ac, remove = c(stopwords('english'),hns_lines), remove_punct = T, tolower=T, stem=T) # Includes host names as stopwords
## Note, the dfm() function's argument "bigram = T" doesn't work. :(
#asheville_dfm <- dfm(ac, tolower=T, stem=T, remove = stopwords('english'), remove_punct = T)
asheville_dfm

## Establishing n-grams
library(tidytext)
library(dplyr)
ngram <- 2
tidy_ac <- tidy(ac) # Tidying the corpus into tidy format
tokens_ac <- tidy_ac %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) #@ Error Explanation
tokens_ac
#@ Unnesting the tokens, and thereby splitting into bigrams, isn't working due to the fact that the Document-Feature Matrix (Quanteda dfm)
#@ is already tokenizing the data. We need to find a way to bigram it before processing with the dfm() function!

## Below Lines not working. Grr...
    # turn the tidy df into a quatenda::dfm
    asheville_dfm <- tokens_ac %>% 
      cast_dfm(id, bigram, ngram)
    
    # convert to stm ... Shouldn't need this as the 
    gsr_stm <- quanteda::convert(gsr_dfm, to = c("stm"), docvars = gsr_final)
## Above Lines not working.

asheville_stm <- convert(asheville_dfm, to='stm') # Converts DFM (document frequency matrix) to STM (structural topic model)
plotRemoved(asheville_stm$documents, lower.thresh = seq(1, 100, by = 1)) # View the different thresholds for sparse vocab removal
asheville_stm <- prepDocuments(asheville_stm$documents, asheville_stm$vocab, lower.thresh = 60) # Remove the sparse vocab

# Fit the structural topic model; K = number of topics, max.em.its = max number of iterations
fit_stm <- stm(documents = asheville_stm$documents, vocab = asheville_stm$vocab, K =42, seed = 1, data=asheville_stm$meta, max.em.its = 40)

labelTopics(fit_stm, n = 10) # View the topics
par(mfrow = c(1,2))
plot(fit_stm, type = 'labels', topics = c(4,6), labeltype = 'frex', main = 'FREX')
plot(fit_stm, type = 'labels', topics = c(4,6), labeltype = 'score', main = 'score')
par(mfrow = c(1,1))
cloud(fit_stm, topic = 6) # Prints the wordclouds of an individual topic

## Prints wordclouds for all topics to the session location
for (i in 1:42){
  pdf(file=paste("42wordcloud_topic", i, ".pdf"))
  cloud(fit_stm, topic = i, colors = "blue4")
  dev.off()
} # Prints i number of wordclouds all to pdf in the session location

plot.STM(fit_stm,type="summary",xlim=c(0,0.30), main="How Common is Each Topic?")

## Identifying the ideal K, number of topics ##
sk<-searchK(asheville_stm$documents,asheville_stm$vocab,K=c(10,20,30,40,50))
knitr::kable(sk$results)
plot(sk)

## Plot Exclusivity vs Semantic Coherence Plot ##
exclusivity <- function(mod.asheville_stm, M=10, frexw=.7){
  w <- frexw
  if(length(mod.asheville_stm$beta$logbeta)!=1) stop("Exclusivity calculation only designed for models without content covariates")
  tbeta <- t(exp(mod.asheville_stm$beta$logbeta[[1]]))
  s <- rowSums(tbeta)
  mat <- tbeta/s #normed by columns of beta now.
  
  ex <- apply(mat,2,rank)/nrow(mat)
  fr <- apply(tbeta,2,rank)/nrow(mat)
  frex<- 1/(w/ex + (1-w)/fr)
  index <- apply(tbeta, 2, order, decreasing=TRUE)[1:M,]
  asheville_stm <- vector(length=ncol(tbeta)) 
  for(i in 1:ncol(frex)) {
    asheville_stm[i] <- sum(frex[index[,i],i])
  }
  asheville_stm
}
