## get the last 5 words from a sentence(string)

last5 <- function(string){
        str_lst <- strsplit(string, split = " ")[[1]]
        last_lst <- tail(str_lst, 5)
        if(length(last_lst) != 5) return(0)
        last <- paste(last_lst, collapse = " ")
        return(last)
}