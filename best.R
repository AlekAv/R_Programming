## Finding the best hospital in a state

best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character", na.strings = "Not Available")
        
        ## Select required columns and recode as numeric
        splitdata <- data[, c(2, 7, 11, 17, 23)]
        splitdata[, 3:5] <- sapply(splitdata[, 3:5], as.numeric)
        
        ## Assign column names for matching
        colnames(splitdata) <- c("name", "states", "heart attack", 
                                 "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if (!(state %in% splitdata$states)) {
                stop("invalid state")
        }
        else if (!(outcome %in% colnames(splitdata))) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death rate
        z <- subset(splitdata, states == state)
        y <- min(z[, outcome], na.rm = TRUE)
        
        x <- subset(z, z[, outcome] == y)        
        return(x$name)
}
