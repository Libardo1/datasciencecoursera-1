## Find the number of lines in a large file

findNumLines <- function(filename){
        testcon <- file(filename,open="r")
        readsizeof <- 200
        nooflines <- 0
        ## the loop continues to read thru the text, readLines knows where it got
        while((linesread <- length(readLines(testcon,readsizeof, warn = FALSE))) > 0 ){
                nooflines <- nooflines+linesread
                #print(linesread)
        }
               
        close(testcon)
        return(nooflines)
}
