## show the first 40 lines of the txt file

showLines <- function(filename){
        con <- file(filename,open="r")
        text <- readLines(con, n = 40, warn = FALSE)
        
        close(con)
        return(text)
}