## model using data table instead of hash
library(data.table)

## THE FOLLOWING ARE SOME DATA AND UTILITY FUNCTION, IN SERVER.R, OUTSIDE shinyServer() 
# uni.table <- fread("./tables/unitable.csv", header = T, sep = ",")
# uni.table[, V1:= NULL]
# 
# bi.table <- fread("./tables/bitable.csv", header = T, sep = ",")
# bi.table[, V1:= NULL]
# 
# tri.table <- fread("./tables/tritable.csv", header = T, sep = ",")
# tri.table[, V1:= NULL]
# 
# f.table <- fread("./tables/ftable.csv", header = T, sep = ",")
# f.table[, V1:= NULL]
# 
# five.table <- fread("./tables/fivetable.csv", header = T, sep = ",")
# five.table[, V1:= NULL]

## utility function: strip extra spaces of the input
trim <- function (x) gsub("\\s+", " ", x)

## utility function: input a list of strings, get the last word of each element and form a new list 
last_words <- function(string_lst){
        out_lst <- c()
        for(i in 1:length(string_lst)){
                listsplit <- strsplit(string_lst[i], split = " ")
                out_lst <- c(out_lst, tail(listsplit[[1]], 1))
        }
        return(out_lst)
}

## THE FOLLOWING FUNCTIONS INVOLVE INPUT, OUTPUT, HENCE INSIDE shinyServer()
## give a ngram (n-1) and the corresponding data list (n), predict the list of probable next words
predict_ngram <- function(lastn_1, n.table){
        ## grep the starting n-1 words from n-grams, find the pool of possible next word
        ## here n is 4, 3, 2
        pool_n <- grep(paste("^", lastn_1, " ", sep = ""), n.table$nGrams, value = TRUE)
        if(length(pool_n) == 0) return(NULL)
        ## compute the p(wn, w ...) joint probabilities
        ## to access the probability, pjoint4$<ngram>
        pjoint_ns <- subset(n.table, nGrams %in% pool_n)
        ## named lists for predicted word and its probability
        ## compute the largest 6 probabilities for the "next word" n-gram mode
        p_pred <- pjoint_ns[order(Freq, decreasing = T),]
        predicted <- head(p_pred$nGrams)
        return(predicted)
}

## backoff model: input a list of list, each element list is a list of prediction for a specific n
backoff <- function(prediction_lst, input_list){
        pred_list <- c()
        i <- 1
        while(i <= length(prediction_lst)){
                if(!is.null(prediction_lst[i][[1]])){
                        predsplit <- strsplit(prediction_lst[i][[1]], split = " ")
                        l <- length(input_list)
                        if(l == 1 && i < 4){
                                i <- i+1
                                next}
                        else if(l == 2 && i < 3){
                                i <- i+1
                                next}
                        else if(l == 3 && i < 2){
                                i <- i+1
                                next}
                        else{
                                for(j in 1:length(predsplit)){
                                        pred_word <- tail(predsplit[[j]],1)       
                                        pred_list <- c(pred_list, rep(pred_word, (6-i)))
                                }
                                
                        }
                }
                i <- i+1
        }
        sorted_pred <- names(sort(table(pred_list), decreasing = TRUE))
        return(sorted_pred)
}

predict_next <- function(sentence){
        ## remove punctuations
        if(sentence == "") return("")
        sentence <- gsub("[^[:alnum:]['-]", " ", sentence)
        sentence <- trim(sentence)
        if(!is.character(sentence)) return("Invalid input. Pls try again.")
        input_list <- strsplit(sentence, " ")
        
        #if(length(input_list[[1]]) < 3) return("Invalid input. Pls try again.")
        ## get the previous n-1 words
        last_4 <- paste(tail(input_list[[1]], 4), collapse = " ")
        last_3 <- paste(tail(input_list[[1]], 3), collapse = " ")
        last_2 <- paste(tail(input_list[[1]], 2), collapse = " ")
        last_1 <- tail(input_list[[1]], 1)
        
        pool_1 <- head(uni.table$nGrams)
        prediction_2 <- predict_ngram(last_1, bi.table)
        prediction_3 <- predict_ngram(last_2, tri.table)
        prediction_4 <- predict_ngram(last_3, f.table)
        prediction_5 <- predict_ngram(last_4, five.table)
        
        predictions <- list(pred_5 = prediction_5, 
                            pred_4 = prediction_4,
                            pred_3 = prediction_3,
                            pred_2 = prediction_2,
                            pred_1 = pool_1)
        #return(predictions)
        
        ## backoff
        pred_list <- backoff(predictions, input_list[[1]])
        pred_list <- unique(pred_list)
        return(pred_list[1:6])
}






