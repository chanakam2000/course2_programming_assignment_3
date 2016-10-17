
rankall <- function(outcome, num = "best") {
        ## Read outcome data
        
        data <- read_data()
        
        ## Check that state and outcome are valid
        
        valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if(!(outcome %in% valid_outcomes ))
        {
                stop("invalid outcome")
        }
        
        if ( !(num == "best" | num == "worst" | is.numeric(num))  )
        {
                stop("invalid num")
        }
        
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        results <- make_results(data,outcome,num)
        return(results)
}

make_results <- function(data,outcome,num)
{
        results <- NULL
        
        states <- sort(unique(data$State))
        
        for(state in states)
        {
                hospital <- find_nth_ranked_hospital(data,state,outcome,num)
                results <- rbind(results,c(hospital,state))
        }
        
        results <- as.data.frame(results)
        colnames(results) <- c("hospital","state")
        rownames(results) <- states
        
        return(results)
        
}

# Find nth rank hospital for given state and given outcome

find_nth_ranked_hospital <- function(data,state,outcome,num)
{
        sorted_hospitals <- sort_hospitals(data,state, outcome)
        nth_hospital <- NULL
        
        if(num == "best" )
        {
                nth_hospital <- sorted_hospitals[1,1]         
        }
        else if(num == "worst")
        {
                last_rank <- length(sorted_hospitals[,1])
                nth_hospital <- sorted_hospitals[last_rank,1]
        }
        else if(num > length(sorted_hospitals[,1]))
        {
                nth_hospital <- NA                       
        }
        else
        {
                nth_hospital <- sorted_hospitals[num,1]
        }
        
        
        return(nth_hospital)
        
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