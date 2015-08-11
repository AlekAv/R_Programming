complete <- function(directory, id = 1:332) {

        allFiles <- list.files(path = directory, full.names = TRUE)
        data <- data.frame()

        for (i in id) {
                r1 <- read.csv(allFiles[i]) # read into file i
                nobs <- sum(complete.cases(r1)) # get complete cases in file i
                
                calc <- data.frame(i, nobs) # 2x2 data.frame with i and sum values
                data <- rbind(data, calc) # row bind calc into data
        }
        
        colnames(data) <- c("id", "nobs")
        return(data)
}