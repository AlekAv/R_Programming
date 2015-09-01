# Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character", na.strings = "Not Available")
        
        ## Select required columns and recode values as numeric
        splitdata <- data[, c(2, 7, 11, 17, 23)]
        splitdata[, 3:5] <- sapply(splitdata[, 3:5], as.numeric)
  
        ## Assign column names for matching
        colnames(splitdata) <- c("hospital", "states", "heart attack", 
                                 "heart failure", "pneumonia")
        
        ## Check that outcome is valid
        if (!(outcome %in% colnames(splitdata[3:5]))) {
                stop("invalid outcome")
        }
        
        ## Get list of unique states and initialise data frame to store results
        uniqueState <- sort(unique(splitdata$states))
        hospitals <- data.frame()
        
        ## Go through each state and return hospital and state at location n
        for(i in uniqueState) {
                z <- subset(splitdata, states == i)
                z <- z[order(z[outcome],z$hospital, na.last=NA),]
                
                ## Translate num into vector n
                if (num == "best") n = 1
                else if (num == "worst") n = nrow(z)
                else n = num
                
                hospitals <- rbind(hospitals, cbind((z[n,"hospital"]), i))
                
        }
        names(hospitals) <- c("hospital", "state")
        hospitals
}