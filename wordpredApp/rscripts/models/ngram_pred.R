## write predictions in file: ngram_pred.csv

library(data.table)

# uniGrams <- read.csv("./nGrams/uniGrams.csv")
# biGrams <- read.csv("./nGrams/biGrams.csv")
# triGrams <- read.csv("./nGrams/triGrams.csv")
# fGrams <- read.csv("./nGrams/fGrams.csv")
# fiveGrams <- read.csv("./nGrams/fiveGrams.csv")

## utility function: to_list
to_list <- function(ngram){
        list <- as.list(ngram$Freq)
        names(list) <- as.character(ngram$nGrams)
        return(list)
}

## utility function: input a list of strings, get the last word of each element and form a new list 
last_words <- function(string_lst){
        out_lst <- c()
        for(i in 1:length(string_lst)){
                listsplit <- strsplit(string_lst[i], split = " ")
                out_lst <- c(out_lst, tail(listsplit[[1]], 1))
        }
        return(out_lst)
}

## pass in a list of ngrams, get the list of list out with prediction vectors as one column
get_pred <- function(n_1.list, n.list){
        out_list <- list()
        for(i in 1:length(n_1.list)){
                pred_str <- predict_ngram(names(n_1.list)[i], n.list)
                pred <- last_words(pred_str)
                out_list[[length(out_list)+1]] <- pred
        }
        names(out_list) <- names(n_1.list)
        dt <- data.table(nGrams = names(out_list), pred = out_list)
        return(dt)
}

#uni.list <- to_list(uniGrams)
#bi.list <- to_list(biGrams)
# tri.list <- to_list(triGrams)
# f.list <- to_list(fGrams)
# five.list <- to_list(fiveGrams)

uni_pred <- get_pred(uni.list, bi.list)
#bi_pred <- get_pred(bi.list, tri.list)
#tri_pred <- get_pred(tri.list, f.list)
#f_pred <- get_pred(f.list, five.list)



outCon <- file("uniGram_pred.csv")
print("Start Writing...")
write.table(uni_pred, outCon)
print("Done Writing.")
rm(outCon)
