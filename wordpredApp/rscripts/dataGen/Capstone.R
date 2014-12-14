## writing: 1-gram, 2-gram, 3-gram, 4-gram
options(mc.cores=1)
options(java.parameters = "-Xmx4g")

#detach("package:tm", unload=TRUE)
#detach("package:ggplot2", unload=TRUE)
#detach("package:openNLP", unload=TRUE)

#library(ggplot2)
library(tm)
library(RWeka)
library(plyr)
library(openNLP)
require("NLP")
#library(slam)



# #dirname <- "./train/sent/"
# dirname <- "./train/sent/200k/"
# #dirname <- "./train/trial/"
# 
# # Read in training data generated from News and Blogs, and annotate sentences
# #filename <- "./train/trial/trainingRandTrial.txt"
# #filename <- "./train/100k/trainRand100k.txt"
# 
# ##inCon <- file(filename, open="r")
# 
# ##text <- readLines(inCon, n = -1, warn = FALSE)
# 
# ## Make tokenizers
# tokenize1 <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t,;:\"()?!"))
# tokenize2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t,;:\"()?!"))
# tokenize3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t,;:\"()?!"))
# tokenize4 <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t,;:\"()?!"))
# tokenize5 <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5, delimiters = " \\r\\n\\t,;:\"()?!"))
# 
# corpus <- Corpus(DirSource(dirname), readerControl = list(language="en_US"))
# 
# ##corpus <- Corpus(VectorSource(text), readerControl = list(language="en_US"))
# 
# corpus <- tm_map(corpus, PlainTextDocument)
# 
# ## not good, we need 's
# ##corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
# 
# ## substitute curly ' as '
# corpus[[1]]$content <- gsub("\u2019", "\'", corpus[[1]]$content)
# corpus[[1]]$content <- gsub("\u0092", "\'", corpus[[1]]$content)
# 
# ## remove punctuations other than ' and -
# corpus[[1]]$content <- gsub("[^[:alnum:]['-]", " ", corpus[[1]]$content)
# corpus <- tm_map(corpus, stripWhitespace)
# 
# text <- corpus[[1]]$content
# rm(corpus)


## Tokenizing
nGrams <- tokenize3(text)

## Get last 5grams in sentences
#nGrams <- unlist(lapply(text, last5))

## <UNK> (too slow)
#nGrams <- unlist(lapply(nGrams, in.vocab))

nFreq <- table(nGrams)
#rm(text)
rm(nGrams)
nFreq <- as.data.frame(nFreq)
allNum <- nrow(nFreq)

## clean the nGrams, remove numbers and foreign languages
num_index <- grep("[^a-zA-Z| ]",nFreq$nGrams)
nFreq_clean <- nFreq[-num_index, ]
#rm(num_index)

nFreq_clean <- nFreq_clean[order(nFreq_clean$Freq, decreasing = TRUE),]
rm(nFreq)

## pruning
threshold <- 2
highN <- subset(nFreq_clean, Freq >= threshold)
highNum_clean <- nrow(nFreq_clean)
rm(nFreq_clean)

highNum <- nrow(highN)
covered <- highNum/highNum_clean

rownames(highN) <- NULL

## get the vocabulary
# words <- highN$nGrams
# wordcorpus <- Corpus(VectorSource(words), readerControl = list(language="en_US"))
# wordcorpus <- tm_map(wordcorpus, tolower)
# vocab <- wordcorpus$content
# vocab <- unique(vocab)
# vocab <- unlist(vocab)

outCon <- file("triGrams.csv")
print("Start Writing...")
write.csv(highN, outCon)
print("Done Writing.")
rm(outCon)
##################################################################################
#termfreq_control <- list(stemming=FALSE, stopwords=FALSE, wordLengths=c(2,100),
#                        tokenize = tokenize2)

#dtm <- DocumentTermMatrix(corpus, control=termfreq_control)

## colSums causes overflow and NAs, use rollup in "slam" package
# allN <- rollup(dtm, 1, na.rm=TRUE, FUN = sum)
# allNum <- sum(allN)
# 
# highfreq <- findFreqTerms(dtm, 2)
# highN <- inspect(dtm[,highfreq])
# highN <- colSums(highN)
# highNum <- sum(highN)
# highN <- as.data.frame(highN)
# colnames(highN) <- "freq"
# ## highN <- t(highN)
# ## highN <- as.data.frame(highN)
# highN["tokens"] <- rownames(highN)
# highN <- highN[order(highN$freq, decreasing = TRUE),]
# 
# covered <- highNum/allNum

# FreqNHist <- ggplot(highN, aes (x = reorder(tokens, desc(freq)), y = as.numeric(freq)))
# FreqNHist <- FreqNHist + geom_bar (stat = "identity", fill = "black")
# FreqNHist <- FreqNHist + theme (axis.text.x = element_text (angle = 45, hjust = 1))
# FreqNHist

#inspect(dtm)[1:10, 1:10]
#terms <- colSums(inspect(dtm))
#termfreq <- sort(terms, decreasing = TRUE)

#close(inCon)





