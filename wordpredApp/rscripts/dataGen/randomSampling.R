options(mc.cores=1)
options(java.parameters = "-Xmx4g")

library(tm)
library(openNLP)
require("NLP")

## Sampling a subset of the data as the training set

file_news <- "en_US.news.txt"
file_blogs <- "en_US.blogs.txt"
file_tweets <- "en_US.twitter.txt"

## since random sampling from lines breaks the line continuity
## too much information loss, so instead, sample continuous chunks of N lines directly
## sample NT lines from each of News and Blogs, NT/2 lines from Twitter
N <- 100
NT <- 200000
set.seed(2441)
sent_token_annotator <- Maxent_Sent_Token_Annotator()

# text_nb <- c(text_news, text_blogs)
# text_nb <- paste(text_nb, collapse = ' ')
# text_nb <- as.String(text_nb)
# a1 <- annotate(text_nb, sent_token_annotator)
# sent_nb <- text_nb[a1]

## sample from news
inNews <- file(file_news, open="r")
text_news <- c()
print("Reading News...")
cat("0%")
count <- 0
while(count < (NT-1)){
        news_in <- readLines(inNews, n = N, warn = FALSE)
        news_sent <- paste(news_in, collapse = ' ')
        news_sent <- as.String(news_sent)
        news_a1 <- annotate(news_sent, sent_token_annotator)
        news_anno <- news_sent[news_a1]
        
        if(runif(1) < 0.4){
               text_news <- c(text_news, news_anno)
               count <- count + N
               if(count%%(NT/10) == 0){
                       percentage1 <- count/NT * 100
                       cat(paste("..", percentage1, "%", sep = ""))
               }
        }
}
print("")
count <- 0
close(inNews)
rm(news_a1)
rm(news_anno)
rm(news_sent)
rm(news_in)

## sample from blogs
inBlogs <- file(file_blogs, open="r")
text_blogs <- c()
print("Reading Blogs...")
cat("0%")
while(count < (NT-1)){
        blogs_in <- readLines(inBlogs, n = N, warn = FALSE)
        
        blogs_sent <- paste(blogs_in, collapse = ' ')
        blogs_sent <- as.String(blogs_sent)
        blogs_a1 <- annotate(blogs_sent, sent_token_annotator)
        blogs_anno <- blogs_sent[blogs_a1]
        
        if(runif(1) < 0.4){
                text_blogs <- c(text_blogs, blogs_anno) 
                count <- count + N
                if(count%%(NT/10) == 0){
                        percentage2 <- count/NT * 100
                        cat(paste("..", percentage2, "%", sep = ""))
                }
        }
}
print("")
count <- 0
close(inBlogs)
rm(blogs_a1)
rm(blogs_anno)
rm(blogs_sent)
rm(blogs_in)

## sample from tweets
inTweets <- file(file_tweets, open="r")
text_tweets <- c()
print("Reading Tweets...")
cat("0%")
while(length(text_tweets) < (NT/2 - 1)){
        tweets_in <- readLines(inTweets, n = N, warn = FALSE)
        if(runif(1) < 0.4){
                text_tweets <- c(text_tweets, tweets_in) 
                if(length(text_tweets)%%(NT/20) == 0){
                        percentage3 <- as.integer(length(text_tweets)/(NT/2-1) * 100)
                        cat(paste("..", percentage3, "%", sep = ""))
                }
        }
        
}
print("Done Reading.")
close(inTweets)
rm(tweets_in)

## write to training.txt


text <- c(text_news, text_blogs, text_tweets)
rm(text_news)
rm(text_blogs)
rm(text_tweets)

outCon <- file("trainSent200k.txt")
print("Start Writing...")
writeLines(text, outCon)
print("Done Writing.")
close(outCon)



