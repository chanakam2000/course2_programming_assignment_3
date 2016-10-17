
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
        
        best_hospital <- sort_hospitals(data,state, outcome)[1,1]
        return(best_hospital)
        
}

sort_hospitals <- function(data,state, outcome)
{
        data_for_state <- data[data$State == state,]
        
        attach(data_for_state)
        
        if (outcome == "heart attack")
        {
                sorted_data <- data_for_state[order(heart_attack,Hospital),c(1,3)]
        }
        if (outcome == "heart failure")
        {
                sorted_data <- data_for_state[order(heart_failure,Hospital),c(1,4)]
        }
        if (outcome == "pneumonia")
        {
                sorted_data <- data_for_state[order(pneumonia,Hospital),c(1,5)]
        }
        
        detach(data_for_state)
        
        return(sorted_data[complete.cases(sorted_data),])
        
}

read_data <- function()
{
        # Read the data file
        # Extract required colums
        # Make a dataframe which is easy to analyze
        
        data <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", na.strings = "Not Available",stringsAsFactors = FALSE)
        data_to_analyze <-  data[,c(2,7,11,17,23)]
        
        # data_to_analyze[,3] <- suppressWarnings(as.numeric(data_to_analyze[,3]))
        # data_to_analyze[,4] <- suppressWarnings(as.numeric(data_to_analyze[,4]))
        # data_to_analyze[,5] <- suppressWarnings(as.numeric(data_to_analyze[,5]))
        
        names(data_to_analyze) <- c("Hospital","State","heart_attack", "heart_failure", "pneumonia")
        
        return(data_to_analyze)
}