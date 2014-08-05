rankall <- function(outcome, num = "best") {
        
        #####################################################
        ## Read outcome data
        #####################################################
        outcome_data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        
        #####################################################
        ## Check that the outcome is valid
        ## Extract unique state values
        #####################################################
        if (outcome == "heart failure") {
                
                ## Change characters to numerics so we can evaluate
                outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- 
                        as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                
                ## Clean out NAs
                outcome_data <- outcome_data[!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
                
                ## Reduce to just hospital name, state and mortality rate
                outcome_data <- subset(outcome_data, select = c(Hospital.Name,
                                                                State,
                                                                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
        }
        else if (outcome == "heart attack") {
                ## Change characters to numerics so we can evaluate
                outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- 
                        as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                
                ## Clean out NAs
                outcome_data <- outcome_data[!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
                
                ## Reduce to just hospital name, state and mortality rate
                outcome_data <- subset(outcome_data, select = c(Hospital.Name,
                                                                State,
                                                                Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
        }
        else if (outcome == "pneumonia") {
                ## Change characters to numerics so we can evaluate
                outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- 
                        as.numeric(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                
                ## Clean out NAs
                outcome_data <- outcome_data[!is.na(outcome_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
                
                ## Reduce to just hospital name, state and mortality rate
                outcome_data <- subset(outcome_data, select = c(Hospital.Name,
                                                                State,
                                                                Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
        }
        else {
                stop("invalid outcome")  
        }
        
        ## Change column names
        colnames(outcome_data)[1] <- "hospital"
        colnames(outcome_data)[2] <- "state"
        colnames(outcome_data)[3] <- "rate"
        
        ## Get a data frame of unique state values
        unique_states <- unique(subset(outcome_data, select = state))
        total_states = nrow(unique_states)
        
        
        #####################################################
        ## For each state, find the hospital of the given rank
        ## and add it to the final list of ranked hospitals
        #####################################################
        ranked_hospitals <- data.frame(hospital = character(),
                                       state = character(),
                                       stringsAsFactors=FALSE)
        for (state_index in 1:total_states) {
        
                ## Prepare the ORDERED data for the first state
                state_data <- outcome_data[outcome_data$state == unique_states[state_index,1],]
                state_data <- state_data[order(state_data$rate, state_data$hospital),]
                num_state_data <- nrow(state_data)
                
                ## Name the row after the current state
                rownames(ranked_hospitals[state_index,]) <- unique_states[state_index,1]
                
                ## Check each type of num/rank value
                if (num == "best") {

                        if (num_state_data > 0) {
                                ranked_hospitals[state_index,] <- c(state_data[1,]$hospital,
                                                                    state_data[1,]$state)
                        }
                        else {
                                ranked_hospitals[state_index,] <- c(NA,
                                                                    unique_states[state_index,1])
                        }
                }
                else if (num == "worst") {
                        if (num_state_data > 0) {
                                ranked_hospitals[state_index,] <- c(state_data[num_state_data,]$hospital,
                                                                    state_data[num_state_data,]$state)
                        }
                        else {
                                ranked_hospitals[state_index,] <- c(NA,
                                                                    unique_states[state_index,1])
                        }
                }
                else if (is.numeric(num)) {
                        
                        ## Check if the specified rank is greater than the number of hospitals
                        if (num > num_state_data) {
                                ranked_hospitals[state_index,] <- c(NA,
                                                                    unique_states[state_index,1])      
                        }
                        else {
                                ranked_hospitals[state_index,] <- c(state_data[num,]$hospital,
                                                                    state_data[num,]$state) 
                        }
                }
                else {
                        ranked_hospitals[state_index,] <- c(NA,
                                                            unique_states[state_index,1])    
                }   
        }
        
        ## Return a data frame with the hospital names and the (abbreviated) state name
        ranked_hospitals <- ranked_hospitals[order(ranked_hospitals$state),]
        ranked_hospitals
}