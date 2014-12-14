## test whether input string is in vocab, if not, replace with <UNK>

#vocab <- read.csv("./nGrams/vocab.csv")
#vocab <- vocab$x

in.vocab <- function(string){
        if(!is.character(string)) return("Invalid input. Pls try again.")
        input_list <- strsplit(string, " ")
        for(i in 1:length(input_list[[1]])){
                tmp_word <- tolower(input_list[[1]][i])
                if(!(tmp_word %in% vocab)) input_list[[1]][i] <- "<UNK>"
        }
        out_string <- paste(input_list[[1]], collapse = " ")
        return(out_string)
}