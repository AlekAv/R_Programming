corr <- function(directory, threshold = 0) {
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!
        allFiles <- list.files(path = directory, full.names = TRUE)
        
        data <- vector(mode = "numeric", length = 0)
        
        for (i in 1:length(allFiles)) {
                
                r1 <- read.csv(allFiles[i])
                
                # compare complete cases in r1 to threshold
                if (sum(complete.cases(r1)) > threshold) { 
                        
                        # obtain correlation parameters
                        c1 <- (cor(r1$sulfate, r1$nitrate, use = "complete.obs"))
                        data <- c(data, c1)
                }
        }
        return(data)
}