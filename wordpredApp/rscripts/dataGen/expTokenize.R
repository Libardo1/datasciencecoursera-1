## Experiment tokenizer
options(mc.cores=1)
options(java.parameters = "-Xmx4g")

library(tm)
library(RWeka)

#text <- c("I'm the best-selling book author: Logan, Yang. What is the right way to tokenize n-grams? Is 
#there a best way to do it?! -", " 'bout what length of text can we process; how is the :) treated?")

tokenize1 <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!"))
tokenize2 <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t,;:\"()?!"))
tokenize3 <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t,;:\"()?!"))

unigram <- tokenize1(text)
bigram <- tokenize2(text)
trigram <- tokenize3(text)