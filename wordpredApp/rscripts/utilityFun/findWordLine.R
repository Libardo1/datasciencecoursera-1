## find the number of lines that contain a certain word

findWordLine <- function(filename, word){
        con <- file(filename,open="r")
        text <- readLines(con, n = -1, warn = FALSE)
        #text <- paste(text, collapse = ' ')
        ## grep(value = FALSE) returns the indices of elements that contains the match in a list
        lines <- grep(word, text, value = T)
        numWordLines <- length(grep(word, text, value = F))
        close(con)
        return(c(numWordLines, lines))
}

## grep -i "biostat" en_US.twitter.txt
## grep "love" en_US.twitter | wc -l
## grep -x "A computer once beat me at chess, but it was no match for me at kickboxing" en_US.twitter.txt | wc -l