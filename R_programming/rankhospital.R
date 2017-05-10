rm(list = ls())

# find the hospital in a state of a certain rank
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
    ## Check that state and outcome are valid
    states <- table(data$State)
    if(!is.element(state, names(states))) {
        stop('invalid state')
    }
    
    outcome_all <- c('heart attack','heart failure','pneumonia')
    index_all <- c(11,17,23)
    if(!is.element(outcome, outcome_all)) {
        stop('invalid outcome')
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    id <- index_all[match(outcome,outcome_all)]
    data_of_interest = data[data$State == state, ]
    data_of_interest[,id] <- suppressWarnings(as.numeric(data_of_interest[,id]))
    data_of_interest <- data_of_interest[complete.cases(data_of_interest),]
    
    ordered_data_of_interest <- order(data_of_interest[id], data_of_interest$Hospital.Name, na.last = NA)
    
    if (num == "best") {
        as.character(data_of_interest$Hospital.Name[ordered_data_of_interest[1]])
    } else if (num == "worst") {
        as.character(data_of_interest$Hospital.Name[ordered_data_of_interest[length(ordered_data_of_interest)]])
    } else if (is.numeric(num)) {
        as.character(data_of_interest$Hospital.Name[ordered_data_of_interest[num]])
    } else {
        stop("invalid num")
    }
}
    