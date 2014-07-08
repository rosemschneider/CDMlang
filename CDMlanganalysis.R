library(plyr)
library(ggplot2)
#Before you start - ggplot2, reshape2, plyr! 

df = read.csv("data/CDMlangsurvey_analysis.csv") #read in data
head(df)
summary(df)

#Word Frequencies
x <- as.matrix(df$Word) #set Words as matrix
counts <- table(x) #establish frequencies
par(ps = 8, cex = 1, cex.main = 1) #text size
barplot(counts[which(counts>5)], las = 2,  
        main = "Frequency of Words", xlab = "Words") #draw barplot, exclude words with fewer than 5 occurences

## an alternate way of doing this
freqs <- ddply(df, .(Word), summarise, count=length(Word))
freqs$Word <- factor(freqs$Word, 
                     levels=with(freqs, 
                                 Word[order(count, Word, decreasing = TRUE)]))
freqs$prop <- freqs$count / sum(freqs$count)
qplot(Word, prop, geom="bar",
      data=subset(freqs, count>10)) + theme_bw()


## age histogram
df$Age <- df$Age_utterance
qplot(Age_utterance,data=df) + theme_bw()

## genders

freqs <- ddply(df, .(Word,gender), summarise, count=length(Word))
freqs <- ddply(freqs, .(Word), mutate, total.count = sum(count))
freqs$Word <- factor(freqs$Word, 
                     levels=unique(with(freqs, 
                                 Word[order(total.count, 
                                            Word, decreasing = TRUE)])))
qplot(Word, count, fill=gender, 
      position="dodge",
      geom="bar", stat="identity", 
      data=subset(freqs,total.count>5 &
                  gender!="" & 
                  (Word=="Ball" | Word == "Hi"))) + theme_bw()


## wordle
library(wordcloud)
wordcloud(words)
