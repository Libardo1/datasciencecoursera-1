## Read in the data and tokenize
library(tm)
library(RWeka)

filename <- "asop.txt"
#filename <- "en_US.news.txt"
#filename <- "en_US.blogs.txt"
#filename <- "output.txt"
profanityfile <- "swearWords.txt"
#filename <- "en_US.twitter.txt"
inCon <- file(filename, open="r")
proCon <- file(profanityfile, open="r")
text <- readLines(inCon, n = -1, warn = FALSE)
profan <- readLines(proCon, n = -1, warn = FALSE)

tokenizedTxt <- NGramTokenizer(text, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!"))
#skipWords <- function(x) removeWords(x, profan)
#funs <- list(stripWhitespace, skipWords, removePunctuation, tolower)

clean_text <- removeWords(text, profan)

#outCon<-file("output.txt")
#writeLines(text, outCon)

# build corpus, doc-term matrix from text
corpus <- Corpus(VectorSource(text))
# corpus to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
termfreq_control <- list(removePunctuation = TRUE, stemming=FALSE, stopwords=T, wordLengths=c(2,100))
# termDist <- termFreq(corpus[[1]], control=termfreq_control)
dtm <-DocumentTermMatrix(corpus, control=termfreq_control) #
terms <- colSums(inspect(dtm))
termfreq <- sort(terms, decreasing = TRUE)

close(inCon)
close(proCon)
#close(outCon)


## Profanity filtering








