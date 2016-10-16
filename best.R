
best <- function(state, outcome) {
        
        ## Read outcome data 
        data <- read_data()
        
        ## Check that state and outcome are valid
        
        valid_states <- unique(data$State)
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if(!(state %in% valid_states))
        {
                stop("invalid state")
        }
        
        if(!(outcome %in% valid_outcomes ))
        {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        best_hospital <- find_best_hospital(data,state, outcome)
        return(best_hospital)
        
}

find_best_hospital <- function(data,state, outcome)
{
        data_for_state <- data[data$State == state,]
        
        attach(data_for_state)
        
        if (outcome == "heart attack")
        {
                sorted_data <- data_for_state[order(heart_attack,Hospital),]
        }
        if (outcome == "heart failure")
        {
                sorted_data <- data_for_state[order(heart_failure,Hospital),]
        }
        if (outcome == "pneumonia")
        {
                sorted_data <- data_for_state[order(pneumonia,Hospital),]
        }
        
        detach(data_for_state)
        
        return(sorted_data[1,1])
        
        
}

read_data <- function()
{
        # Read the data file
        # Extract required colums
        # Make a dataframe which is easy to analyze
        
        data <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        data_to_analyze <-  data[,c(2,7,11,17,23)]
        
        data_to_analyze[,3] <- suppressWarnings(as.numeric(data_to_analyze[,3]))
        data_to_analyze[,4] <- suppressWarnings(as.numeric(data_to_analyze[,4]))
        data_to_analyze[,5] <- suppressWarnings(as.numeric(data_to_analyze[,5]))
        
        names(data_to_analyze) <- c("Hospital","State","heart_attack", "heart_failure", "pneumonia")
        
        return(data_to_analyze)
}