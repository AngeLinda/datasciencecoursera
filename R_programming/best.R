rm(list = ls())

# find the best hospital in a state
best <- function(state,outcome) {
    ## read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
    ## check that state and outome are valid
    states <- table(data$State)
    if(!is.element(state, names(states))) {
        stop('invalid state')
    }
    
    outcome_all <- c('heart attack','heart failure','pneumonia')
    index_all <- c(11,17,23)
    if(!is.element(outcome, outcome_all)) {
        stop('invalid outcome')
    }
    
    ## return hospital name in that state with lowest 30-day death rate
    id <- index_all[match(outcome,outcome_all)]
    data_of_interest = data[data$State == state, ]
    data_of_interest[,id] <- suppressWarnings(as.numeric(data_of_interest[,id]))
    data_of_interest <- data_of_interest[complete.cases(data_of_interest),]
    
    hospital <- data_of_interest[(data_of_interest[, id] == min(data_of_interest[, id])), ]$Hospital.Name
    
    sort(hospital)[1]
}