pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        allFiles <- list.files(path = directory, full.names = TRUE)
        data <- data.frame()
        
        for (i in id) {
                data <- rbind(data, read.csv(allFiles[i]))
                ##print(allFiles[i])
        }
        ##print(tail(data))
        return(mean(data[, pollutant], na.rm = TRUE))
}