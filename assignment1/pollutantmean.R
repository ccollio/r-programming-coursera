pollutantmean <- function(directory, pollutant, ids = 1:332) {

        # initialize the data frame
        all_data <- data.frame(Date = NA, sulfate = NA, nitrate = NA, ID = NA)
        
        for (i in ids) {
                
                # extract data from the new file
                if (i < 10) {
                   
                        new_data <- read.csv(paste(directory,"/","00",i,".csv", sep = ""), header=TRUE, sep=",")                         
                }
                else if (i < 100) {
                        
                        new_data <- read.csv(paste(directory,"/","0",i,".csv", sep = ""), header=TRUE, sep=",")   
                }
                else {
                        
                        new_data <- read.csv(paste(directory,"/",i,".csv", sep = ""), header=TRUE, sep=",")          
                }
                
                # combine new data with the total data being collected
                all_data <- rbind(all_data, new_data)
        }
        
        
        # extract the pollutant data
        pollutant_data <- all_data[,pollutant]
        
        # filter out NAs 
        clean_data <- pollutant_data[!is.na(pollutant_data)]
        
        # return the mean, rounded to three decimal places
        round(mean(clean_data), 3) 
}