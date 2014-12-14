## resolve an error
options(mc.cores=1)
## trial codes

library("RWeka")
library("tm")
library("openNLP")

#data("crude")

filename <- "trainTrialSent.txt"
#freq <- read.table("./freq/news_unifreq.txt")
inCon <- file(filename, open="r")

text <- readLines(inCon, n = -1, warn = FALSE)
# text <- paste(text, collapse = ' ')
# 
# text <- as.String(text)
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# sent_token_annotator
# a1 <- annotate(text, sent_token_annotator)
# a1
# text[a1]
#text <- c("The Gangmember retorted, \"Old and  time for you to die.\"",
#          "BUT,\" exclaimed the Old Lady")
corpus <- Corpus(VectorSource(text))

BigramTokenizer <- function(x) 
        NGramTokenizer(x, Weka_control(min = 2, max = 2,delimiters = " \\r\\n\\t,;:\"()?!"))
tdm <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

inspect(tdm[20000:20010, 30:40])
