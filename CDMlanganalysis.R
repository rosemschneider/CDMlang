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
qplot(Word, count, geom="bar",
      data=subset(freqs, count>5))


## here's why you'd do it that way


#Age_utterance Frequencies
y <- as.matrix(df$Age_utterance) #set age of utterance as matrix 
counts2 <- table(y) #frequencies 
barplot(counts2, las = 2, ylim = c(0, 250), main = "Ages of Utterances") #draw barplot
text(2, 240, "236") #These are adding the counts above the bars 
text(3.2, 85, "81") 
text(4.3, 61, "57") 
text(5.5, 34, "30") 
text(6.75, 58, "54")
text(8, 45, "41")

#Subject frequencies 
z <- as.matrix(df$Subject)
counts3 <- table(z)
barplot(counts3, las = 2, ylim = c(0, 300), main = "Frequency of Subjects")

#Breakdown by gender 
#Females
df2 <- read.csv("CDMlangsurvey_females.csv")
head(df2)
summary(df2)
#Words
a <- as.matrix(df2$Word) #set Words as matrix
counts4 <- table(a) #establish frequencies
par(ps = 8, cex = 1, cex.main = 1) #text size
barplot(counts4[which(counts4>5)], las = 2, ylim = c(0, 45), main = "Frequency of Words - Females", xlab = "Words") #draw barplot, exclude words with fewer than 5 occurences
#Age
b <- as.matrix(df2$Age_utterance) #set age of utterance as matrix 
counts5 <- table(b) #frequencies 
barplot(counts5, las = 2, ylim = c(0, 150), main = "Ages of Utterances - Females") #draw barplot
#Subject 
c <- as.matrix(df2$Subject)
counts6 <- table(c)
barplot(counts6, las = 2, ylim = c(0, 200), main = "Frequency of Subjects - Females")

#Males 
df3 <- read.csv("CDMlangsurvey_males.csv")
l <- as.matrix(df3$Word) #set Words as matrix
counts7 <- table(l) #establish frequencies
par(ps = 8, cex = 1, cex.main = 1) #text size
barplot(counts7[which(counts7>5)], las = 2, ylim = c(0, 45), main = "Frequency of Words - Males", xlab = "Words") #draw barplot, exclude words with fewer than 5 occurences
#Age 
m <- as.matrix(df3$Age_utterance) #set age of utterance as matrix 
counts8 <- table(m) #frequencies 
barplot(counts8, las = 2, ylim = c(0, 150), main = "Ages of Utterances - Males") #draw barplot
#Subject 
n <- as.matrix(df3$Subject)
counts9 <- table(n)
barplot(counts9, las = 2, ylim = c(0,200), main = "Frequency of Subjects - Males")

#Test - is Ball different from Hi for females?
F_word_counts <- count(df2, "Word")
F_ball_count <- F_word_counts[13,2]
F_hi_count <- F_word_counts[52, 2]
#I don't know if this will be useful and I'm sure that there is an easier way to do it…work on this later


