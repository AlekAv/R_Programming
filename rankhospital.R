# Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
        

        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character", na.strings = "Not Available")
        
        ## Select required columns and recode values as numeric
        splitdata <- data[, c(2, 7, 11, 17, 23)]
        splitdata[, 3:5] <- sapply(splitdata[, 3:5], as.numeric)
        
        ## Assign column names for matching
        colnames(splitdata) <- c("name", "states", "heart attack", 
                                 "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if (!(state %in% splitdata$states)) {
                stop("invalid state")
        }
        else if (!(outcome %in% colnames(splitdata[3:5]))) {
                stop("invalid outcome")
        }        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        ## Extract data for a single state
        z <- subset(splitdata, states == state)
        
        ## Order the hospitals by outcome and then the name and remove NAs
        z <- na.omit(z[order(z[outcome],z$name, na.last = NA),])
        
        ## Translate num into vector
        if (num == "best") num = 1
        else if (num == "worst") num = nrow(z)

        ## Return hospital name at num relative position
        return(z[num,"name"])
}