words <- c("a","b","c","d","e")
n1 <- c(rep("a",50),rep("b",50))
set.seed(3535)
n2 <- sample(words, 100, replace = TRUE)
n3 <- sample(words, 100, replace = TRUE)
n4 <- sample(words, 100, replace = TRUE)
ngrams <- paste(n1,n2,n3,n4)
print("100 total entries")
print(paste(length(unique(ngrams)),"unique total entries"))
print(paste("Of those, ", length(tapply(ngrams,ngrams,length)[tapply(ngrams,ngrams,length) >1]),
            " account for ", sum(tapply(ngrams,ngrams,length)[tapply(ngrams,ngrams,length) >1]), 
            " of the total"))
#see
tapply(ngrams,ngrams,length)[tapply(ngrams,ngrams,length) >1]
#however, if we were to use a weaker predictor, we can cut the amount of information we might 
#need to remember
ngrams <- paste(n2,n3,n4)
print("100 total entries")
print(paste(length(unique(ngrams)),"unique total entries"))
print(paste("Of those, ", length(tapply(ngrams,ngrams,length)[tapply(ngrams,ngrams,length) >1]), 
            " account for ", sum(tapply(ngrams,ngrams,length)[tapply(ngrams,ngrams,length) >1]), 
            " of the total"))
#see
tapply(ngrams,ngrams,length)[tapply(ngrams,ngrams,length) >1]

#but other things we can look at is, for variaous combinations, how much variation is there in
#the predicted entry

ngrams <- paste(n2,n3)
print("100 total entries")
table(ngrams, n4)

# From this we find "a a" always leads to d, regardless of what n1 is so can compact our data 
# from "b a a" and "a a a" with no loss of accuracy.
# We might also see that "b c" leads to a 50% of the time, so might consider if we want to 
# compact "a b c" and "b b c" knowing we will have a bit of a loss of accuracy
# We note "c b" leads to pretty much anything so that might need a full range of options, and 
# even then is not going to be a very accurate predictor.

ngrams <- paste(n1[n2=="c" & n3=="b"], n2[n2=="c" & n3=="b"],n3[n2=="c" & n3=="b"])
print("100 total entries")
table(ngrams, n4[n2=="c" & n3=="b"])

# If there had been a bias towards particular options at a finer level of detail, it might 
# have been worth keeping the detail, but we are likely to get as good results from predicting
# "c b" leads to "a" as having separate options for "a c b" and "b c b" so from a data compaction
# perspective we are no worse off.


