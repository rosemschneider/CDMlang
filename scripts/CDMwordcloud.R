#R Wordcloud 

#preliminaries 
library(ctv)
library(wordcloud)
library(tm)
library(RColorBrewer)
#Set path 
path <- file.path(getwd(), "Documents/target") #input where your directory with text file is
path
DirSource(path)

#Create corpus 
docs <- Corpus(DirSource(path))

#Data processing, create a dtm 
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(1,Inf)))

m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing=TRUE)
head(v,14)
words <- names(v)
d <- data.frame(word=words, freq=v)

#Wordcloud time 
colors <- brewer.pal(6, "Set1")
wordcloud(d$word, d$freq, scal=c(5,0.5), rot.per=.35, 
          use.r.layout=FALSE, random.color=TRUE,
          colors=colors, min.freq=1)

