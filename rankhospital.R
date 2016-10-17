source("best.R")

rankhospital <- function(state, outcome, num = "best") {
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
        
        if ( !(num == "best" | num == "worst" | is.numeric(num))  )
        {
                stop("invalid num")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        result <- find_nth_ranked_hospital(data,state,outcome,num)
        result
        
}

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
