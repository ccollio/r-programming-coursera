corr <- function(directory, threshold = 0) {

        # put all of the csv files into a list
        csv_file_list <- list.files(directory)
        
        # initialize vector for storing correlations
        correlations <- numeric()
        
        for (filename in csv_file_list) {
                
                # find the file number from the filename
                filename_list <- strsplit(filename, "\\.")
                filename_matrix <- matrix(unlist(filename_list), nrow=2)
                file_num <- as.integer(filename_matrix[1,1])
                
                # get the number of complete observations within the monitor
                num_complete = complete(directory, file_num)["nobs"]
 
                if (num_complete > threshold) {
       
                        data <- read.csv(paste(directory,"/",filename,sep=""),
                                         header=TRUE, 
                                         sep=",")
                        
                        # run a correlation only on complete observations
                        # append to the collection of correlations
                        correlations <- c(correlations, cor(data[,"sulfate"],
                                                                  data[,"nitrate"],
                                                                  use = "complete.obs"))                                      
                }
        }
        
        correlations
}