## EDA: 1-gram, 2-gram, 3-gram
options(mc.cores=1)
options(java.parameters = "-Xmx4g")

library(tm)
library(RWeka)
library(ggplot2)


dirname <- "./train/100k"

#profanityfile <- "swearWords.txt"
#inCon <- file(filename, open="r")
#proCon <- file(profanityfile, open="r")
#text <- readLines(inCon, n = -1, warn = FALSE)
#profan <- readLines(proCon, n = -1, warn = FALSE)

tokenize1 <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!"))
tokenize2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!"))
tokenize3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!"))

#clean_text <- removeWords(text, profan)

corpus <- Corpus(DirSource(dirname), readerControl = list(language="en_US"))

##corpus <- Corpus(VectorSource(text))

#corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, PlainTextDocument)
#corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, stripWhitespace)
termfreq_control <- list(stemming=FALSE, stopwords=FALSE, wordLengths=c(2,100),
                         tokenize = tokenize2)

dtm <- DocumentTermMatrix(corpus, control=termfreq_control)

highfreq <- findFreqTerms(dtm, 2000)
highBi <- inspect(dtm[,highfreq])
rownames(highBi) <- "freq"
highBi <- t(highBi)
highBi <- as.data.frame(highBi)
highBi["tokens"] <- rownames(highBi)
FreqBiHist <- ggplot(highBi, aes (x = tokens, y = freq))
FreqBiHist <- FreqBiHist + geom_bar (stat = "identity", fill = "black")
FreqBiHist <- FreqBiHist + theme (axis.text.x = element_text (angle = 45, hjust = 1))
FreqBiHist
#inspect(dtm)[1:10, 1:10]
#terms <- colSums(inspect(dtm))
#termfreq <- sort(terms, decreasing = TRUE)

#close(inCon)
#close(proCon)




