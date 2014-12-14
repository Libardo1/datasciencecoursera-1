## vocabtext: transform corpus into one with only words in vocab

#options(mc.cores=1)
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



#dirname <- "./train/sent/"
dirname <- "./train/sent/200k/"
#dirname <- "./train/trial/"

# Read in training data generated from News and Blogs, and annotate sentences
#filename <- "./train/trial/trainingRandTrial.txt"
#filename <- "./train/100k/trainRand100k.txt"

##inCon <- file(filename, open="r")

##text <- readLines(inCon, n = -1, warn = FALSE)

## Make tokenizers
tokenize1 <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t,;:\"()?!"))
tokenize2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t,;:\"()?!"))
tokenize3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t,;:\"()?!"))
tokenize4 <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t,;:\"()?!"))

corpus <- Corpus(DirSource(dirname), readerControl = list(language="en_US"))

##corpus <- Corpus(VectorSource(text), readerControl = list(language="en_US"))

corpus <- tm_map(corpus, PlainTextDocument)

## not good, we need 's
##corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)

## substitute curly ' as '
corpus[[1]]$content <- gsub("\u2019", "\'", corpus[[1]]$content)
corpus[[1]]$content <- gsub("\u0092", "\'", corpus[[1]]$content)

## remove punctuations other than ' and -
corpus[[1]]$content <- gsub("[^[:alnum:]['-]", " ", corpus[[1]]$content)
corpus <- tm_map(corpus, stripWhitespace)

text <- corpus[[1]]$content
rm(corpus)

system.time(
        pure_text <- unlist(lapply(text, in.vocab))
        )

outCon <- file("pure_text.txt")
print("Start Writing...")
writeLines(pure_text, outCon)
print("Done Writing.")
rm(outCon)


