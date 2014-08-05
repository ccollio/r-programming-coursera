best <- function(state, outcome) {
        
        ## Read outcome data
        outcome_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        ## Check that state is valid
        valid_data <- outcome_data[outcome_data$State == state, ]
        if (nrow(valid_data) == 0) {
                stop("invalid state")
        }
        
        ## Check that outcome is valid. Must be one of the following:
        ##      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        ##      Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
        ##      Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        lowest_rate_hospitals = character()
        best_hospital = ""
        if (outcome == "heart failure") {
                ## Change characters to numerics so we can evaluate
                valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- 
                        as.numeric(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                
                ## Clean out NAs
                valid_data <- valid_data[!is.na(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                
                ## Extract the lowest mortality rate rows
                lowest_rate_hospitals <- valid_data$Hospital.Name[valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == 
                                                                min(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,na.rm = TRUE)]
        }
        else if (outcome == "heart attack") {
                ## Change characters to numerics so we can evaluate
                valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- 
                        as.numeric(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                
                ## Clean out NAs
                valid_data <- valid_data[!is.na(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                
                ## Extract the lowest mortality rate rows
                lowest_rate_hospitals <- valid_data$Hospital.Name[valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == 
                                                                          min(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,na.rm = TRUE)]   
        }
        else if (outcome == "pneumonia") {
                ## Change characters to numerics so we can evaluate
                valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- 
                        as.numeric(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                
                ## Clean out NAs
                valid_data <- valid_data[!is.na(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                
                ## Extract the lowest mortality rate rows
                lowest_rate_hospitals <- valid_data$Hospital.Name[valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == 
                                                                          min(valid_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,na.rm = TRUE)]         
        }        
        else {
                stop("invalid outcome")   
        }
                
                
        ## Break the ties
        if (length(lowest_rate_hospitals > 1)) {
                ## Sort hospital names alphabetically
                best_hospital = sort(lowest_rate_hospitals)[1]    
        }
        else {
                best_hospital = lowest_rate_hospitals[1]
        }
        
        
        ## Return hospital name in that state with lowest 30-day death rate
        best_hospital    
}
