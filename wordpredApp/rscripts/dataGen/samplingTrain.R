## Sampling a subset of the data as the training set

file_news <- "en_US.news.txt"
file_blogs <- "en_US.blogs.txt"
file_tweets <- "en_US.twitter.txt"

## since random sampling from lines breaks the line continuity
## too many information loss, so instead, sample continuous chunks directly

## sample from news
inNews <- file(file_news, open="r")
text_news <- readLines(inNews, n = 200000, warn = FALSE)
close(inNews)

## sample from blogs
inBlogs <- file(file_blogs, open="r")
text_blogs <- readLines(inBlogs, n = 200000, warn = FALSE)
close(inBlogs)

## sample from tweets
inTweets <- file(file_tweets, open="r")
text_tweets <- readLines(inTweets, n = 100000, warn = FALSE)
close(inTweets)

## write to training.txt
text <- c(text_news, text_blogs, text_tweets)
outCon <- file("training.txt")
writeLines(text, outCon)
close(outCon)







