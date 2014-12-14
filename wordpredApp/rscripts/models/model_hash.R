## N-gram next word prediction algorithm
## 4-gram -> 3-gram -> 2-gram -> 1-gram
## P(wn | ...) = P(wn, ...) / P(...)

#options(mc.cores=1)
#options(java.parameters = "-Xmx4g")

library(hash)

# ## THE FOLLOWING ARE SOME DATA AND UTILITY FUNCTION, IN SERVER.R, OUTSIDE shinyServer() 
# uniGrams <- read.csv("./nGrams/uniGrams.csv")
# biGrams <- read.csv("./nGrams/biGrams.csv")
# triGrams <- read.csv("./nGrams/triGrams.csv")
# fGrams <- read.csv("./nGrams/fGrams.csv")
# fiveGrams <- read.csv("./nGrams/fiveGrams.csv")
# 
## utility function: to_hash
# to_hash <- function(ngram){
#         list <- as.list(ngram$Freq)
#         names(list) <- as.character(ngram$nGrams)
#         h <- hash()
#         .set(h, list)
#         return(h)
# }
# 
# uni.list <- as.list(uniGrams$Freq)
# names(uni.list) <- as.character(uniGrams$nGrams)
# 
# bi.hash <- to_hash(biGrams)
# bi.keys <- keys(bi.hash)
# 
# tri.hash <- to_hash(triGrams)
# tri.keys <- keys(tri.hash)
# 
# f.hash <- to_hash(fGrams)
# f.keys <- keys(f.hash)
# 
# five.hash <- to_hash(fiveGrams)
# five.keys <- keys(five.hash)

# rm(uniGrams)
# rm(biGrams)
# rm(triGrams)
# rm(fGrams)
# rm(fiveGrams)

## THE FOLLOWING FUNCTIONS INVOLVE INPUT, OUTPUT, HENCE INSIDE shinyServer()
## give a ngram (n-1) and the corresponding data list (n), predict the list of probable next words
predict_ngram <- function(lastn_1, n.hash, n.keys){
        ## grep the starting n-1 words from n-grams, find the pool of possible next word
        ## here n is 4, 3, 2
        pool_n <- grep(paste("^", lastn_1, " ", sep = ""), n.keys, value = TRUE)
        if(length(pool_n) == 0) return(NULL)
        ## compute the p(wn, w ...) joint probabilities
        ## to access the probability, pjoint4$<ngram>
        pjoint_ns <- n.hash[pool_n]
        
        ## compute the largest 6 probabilities for the "next word" n-gram model
        #p_last <- n_1.hash[lastn_1][[1]]
        #if(is.null(p_last)) return()
        p_pred <- values(pjoint_ns)        #/p_last
        p_pred <- sort(p_pred, decreasing = TRUE)
        predicted <- head(names(p_pred))
        return(predicted)
}

## backoff model: input a list of list, each element list is a list of prediction for a specific n
backoff <- function(prediction_lst, input_list){
        pred_list <- c()
        i <- 1
        while(i < length(prediction_lst)){
                if(!is.null(prediction_lst[i][[1]])){
                        #pred_list <- c(pred_list, prediction_lst[i][[1]])
                        predsplit <- strsplit(prediction_lst[i][[1]], split = " ")
                        for(j in 1:length(predsplit)){
                                if(length(input_list) < 4){
                                        word_pos <- length(input_list) + 1
                                        pred_word <- predsplit[[j]][word_pos]
                                } else {
                                        pred_word <- tail(predsplit[[j]],1)       
                                }
                                pred_list <- c(pred_list, rep(pred_word, 2^(6-i)))
                        }
                }
                i <- i + 1
        }
        sorted_pred <- names(sort(table(pred_list), decreasing = TRUE))
        return(sorted_pred)
}

predict_next <- function(sentence){
        if(!is.character(sentence)) return("Invalid input. Pls try again.")
        input_list <- strsplit(sentence, " ")
        #if(length(input_list[[1]]) < 3) return("Invalid input. Pls try again.")
        ## get the previous n-1 words
        last_4 <- paste(tail(input_list[[1]], 4), collapse = " ")
        last_3 <- paste(tail(input_list[[1]], 3), collapse = " ")
        last_2 <- paste(tail(input_list[[1]], 2), collapse = " ")
        last_1 <- tail(input_list[[1]], 1)
        
        pool_1 <- head(names(uni.list))
        prediction_5 <- predict_ngram(last_4, five.hash, five.keys)
        prediction_4 <- predict_ngram(last_3, f.hash, f.keys)
        prediction_3 <- predict_ngram(last_2, tri.hash, tri.keys)
        prediction_2 <- predict_ngram(last_1, bi.hash, bi.keys)
        
        predictions <- list(pred_5 = prediction_5, 
                            pred_4 = prediction_4,
                            pred_3 = prediction_3,
                            pred_2 = prediction_2,
                            pred_1 = pool_1)
        return(predictions)
        ## backoff
        pred_list <- backoff(predictions, input_list[[1]])
        pred_list <- unique(pred_list)
        return(pred_list[1:6])
        
}


