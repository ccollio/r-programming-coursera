rankhospital <- function(state, outcome, num = "best") {

        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state is valid
        valid_data <- outcome_data[outcome_data$State == state, ]
        if (nrow(valid_data) == 0) {
                stop("invalid state")
        }
        
        #####################################################
        ## Check that outcome is valid. Must be one of the following:
        ##      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        ##      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        ##      Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        ## Extract the hospital data so that we can rank them
        #####################################################
        ranked_hospital <- ""
        if (outcome == "heart failure") {
                
                ## Change characters to numerics so we can evaluate
                valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- 
                        as.numeric(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                
                ## Clean out NAs
                valid_data <- valid_data[!is.na(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                
                ## Reduce to just hospital name and mortality rate
                valid_data <- subset(valid_data, select = c(Hospital.Name,
                                                            Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        }
        else if (outcome == "heart attack") {

                ## Change characters to numerics so we can evaluate
                valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- 
                        as.numeric(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)  
                
                ## Clean out NAs
                valid_data <- valid_data[!is.na(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                
                ## Reduce to just hospital name and mortality rate
                valid_data <- subset(valid_data, select = c(Hospital.Name,
                                                            Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))          
        }
        else if (outcome == "pneumonia") {
                ## Change characters to numerics so we can evaluate
                valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- 
                        as.numeric(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
            
                ## Clean out NAs
                valid_data <- valid_data[!is.na(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                
                ## Reduce to just hospital name and mortality rate
                valid_data <- subset(valid_data, select = c(Hospital.Name,
                                                            Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        }
        else {
                stop("invalid outcome")  
        }
        
        
        #####################################################
        ## Find the mortality rate at the specified rank
        #####################################################
        
        ## Order by Rate then by Hospital Name
        colnames(valid_data)[2] <- "Rate"
        valid_data <- valid_data[order(valid_data$Rate, valid_data$Hospital.Name),]  
        
        if (num == "best") {
                if (length(valid_data) > 0) {
                        ranked_hospital <- valid_data$Hospital.Name[1]
                }
                else {
                        ranked_hospital <- NA
                }
        }
        else if (num == "worst") {
                if (nrow(valid_data) > 0) {
                        ranked_hospital <- valid_data$Hospital.Name[nrow(valid_data)]
                }
                else {
                        ranked_hospital <- NA
                }
        }
        else if (is.numeric(num)) {
   
                ## Check if the specified rank is greater than the number of hospitals
                if (num > nrow(valid_data)) {
                        ranked_hospital <- NA        
                }
                else {
                        ranked_hospital <- valid_data$Hospital.Name[num]
                }
        }
        else {
                ranked_hospital <- NA      
        }
        
        ## Return hospital name in that state with the given rank 30-day death rate
        ranked_hospital
}
