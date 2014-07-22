complete <- function(directory, ids = 1:332) {
        
        # initialize the data frame
        complete_data <- data.frame()
        
        for (i in ids) {
                # extract data from the new file
                if (i < 10) {
                        new_data <- read.csv(paste(directory,"/","00",i,".csv", sep = ""), 
                                             header=TRUE, 
                                             sep=",")                         
                }
                else if (i < 100) { 
                        new_data <- read.csv(paste(directory,"/","0",i,".csv", sep = ""), 
                                             header=TRUE, 
                                             sep=",")   
                }
                else {  
                        new_data <- read.csv(paste(directory,"/",i,".csv", sep = ""), 
                                             header=TRUE, 
                                             sep=",")          
                }  
                
                # if it's the first entry to add
                if (dim(complete_data)[1] == 0) {
                        complete_data <- data.frame(id = i, nobs = sum(complete.cases(new_data)))
                }
                else {
                        # append new row containing the id and number of complete cases
                        complete_data <- rbind(complete_data, c(i, sum(complete.cases(new_data))))
                }
        }
        
        # return the list
        complete_data

}