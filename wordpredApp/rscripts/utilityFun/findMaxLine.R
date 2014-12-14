## find the length of the longest line in a file

findMaxLine <- function(filename){
        con <- file(filename,open="r")
        maxlen <- 0
        text <- readLines(con, n = -1, warn = FALSE)
        for(i in 1:length(text)){
                ## nchar() gets the length of a character, string
                currentlen <- nchar(text[i])
                
                if(currentlen > maxlen){
                        maxlen <- currentlen
                        
                }               
        }
        close(con)
        return(maxlen)
}

## wc command:
## wc -L *.txt