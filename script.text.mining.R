#SolStat AGL script Word-Cloud && text mining
rm(list=ls())
# Install
# install.packages("tm")  # for text mining
# install.packages("SnowballC") # for text stemming
# install.packages("wordcloud") # word-cloud generator 
# install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


# Read the text file
filePath <- "/home/dell/Documents/1_WORKING/DATA/TEXT_MINING/cartaabierta.txt"
text <- readLines(filePath)
head(text, 40)

# Load the data as a corpus
docs <- Corpus(VectorSource(text))#Corpus(DirSource(filePath))#Corpus(VectorSource(text))#Corpus(DirSource(filePath))
summary(docs)
inspect(docs[1])

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("spanish"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("través", "cada", "forma", "según", "sólo",
                                    "hacia", "así")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
docs <- tm_map(docs, stemDocument)

inspect(docs[1])

# Build a term-document matrix
dtm <- TermDocumentMatrix(docs)
dim(dtm)
inspect(dtm[1:5,1:5])

m <- as.matrix(dtm)
head(m)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(term = names(v),freq=v)
head(d, 10)


#Generate the Word cloud
set.seed(1234)
wordcloud(words = d$term, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#Explore frequent terms and their associations

freq_200 <-findFreqTerms(dtm, lowfreq = 200)


findAssocs(dtm, terms = freq_200, corlimit = .999)


#Plot word frequencies


tf <- d
head(tf,20)
# 
# # descending sort of teh tf by freq
# tf$term <- factor(tf$term, levels = tf$term[order(-tf$freq)])
# library(ggplot2)
# 
# p <- ggplot(subset(tf, freq>300), aes(term, freq))    
# p <- p + geom_bar(stat="identity")   
# p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
# p

library(ggstance)
library(ggthemes)
# plot_austen <- book_words %>%
#   arrange(desc(tf_idf)) %>%
#   mutate(word = factor(word, levels = rev(unique(word))))
ggplotggplot(tf, aes(term, freq,)) fill = book, alpha = tf_idf) +
  geom_barh(stat = "identity") +
  labs(title = "Highest frequency",
       y = NULL, x = "count") +
  theme_tufte(base_family = "Arial", base_size = 13, ticks = FALSE) +
  scale_alpha_continuous(range = c(0.6, 1), guide = FALSE) +
  scale_x_continuous(expand=c(0,0)) +
  scale_fill_viridis(end = 0.85, discrete=TRUE) +
  theme(legend.title=element_blank()) +
  theme(legend.justification=c(1,0), legend.position=c(1,0))


#further heat map & correlogram
dtm
class(dtm)
inspect(dtm)
tdm.low.sparce <- removeSparseTerms(dtm, sparse = 0.1)
class
dtm

tdm.low.sparce <- as.matrix(tdm.low.sparce)

m <- tdm.low.sparce 
head(tdm.low.sparce)
str(tdm.low.sparce)
dim(tdm.low.sparce)
class(tdm.low.sparce)

sub2 <- 
  tdm.low.sparce[tdm.low.sparce[,c(1,2,3,4)] >=100, ]
filter(tdm.low.sparce,tdm.low.sparce$Docs >= 11)
m[ m[,1] >= 100 & m[,2] >= 100 & m[,3] >= 100 & m[,4] >= 100 , ]

library(reshape2)
library(plyr)
tdm.low.sparce.m <- melt(tdm.low.sparce, value.name = "count")
arrange(tdm.low.sparce.m, count )
head(tdm.low.sparce.m)
dim(tdm.low.sparce.m)
summary(tdm.low.sparce.m)

sub1 <- subset(tdm.low.sparce.m, count >= 100, drop=TRUE)

summary(sub1)
head(sub2, 20)
dim(sub1)
summary(sub1)
#heat map
ggplot(sub1, aes(x = Docs, y = Terms, fill = log10(count))) +
  geom_tile(colour = "white") +
  scale_fill_gradient(high="steelblue" , low="white")+
  ylab("") +
  theme(panel.background = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

library(corrplot)

corr <- cor(tdm.low.sparce)
corrplot(corr, method = "circle", type = "upper", tl.col="black", tl.cex=0.7)

#terms heat map
tdm.low.sparce.t <- t(tdm.low.sparce)
corr.t <- cor(tdm.low.sparce.t)
corrplot(corr.t,method = "circle", type = "upper", tl.col="black", tl.cex=0.7)


