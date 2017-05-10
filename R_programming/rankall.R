rm(list = ls())

# rank hospitals in all states
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    outcome_all <- c('heart attack','heart failure','pneumonia')
    index_all <- c(11,17,23)
    if(!is.element(outcome, outcome_all)) {
        stop('invalid outcome')
    }
    
    ## For each state, find the hospital of the given rank
    id <- index_all[match(outcome,outcome_all)]
    states <- sort(unique(data$State)) # states <- names(table(data$State))
    
    dataframe <- list()
    
    for(state in states) {
        data_of_interest = data[data$State == state, ]
        data_of_interest[,id] <- suppressWarnings(as.numeric(data_of_interest[,id]))
        data_of_interest <- data_of_interest[complete.cases(data_of_interest),]
        
        ordered_data_of_interest <- order(data_of_interest[id], data_of_interest$Hospital.Name, na.last = NA)
        
        if (num == "best") {
            hospital <- as.character(data_of_interest$Hospital.Name[ordered_data_of_interest[1]])
        } else if (num == "worst") {
            hospital <- as.character(data_of_interest$Hospital.Name[ordered_data_of_interest[length(ordered_data_of_interest)]])
        } else if (is.numeric(num)) {
            hospital <- as.character(data_of_interest$Hospital.Name[ordered_data_of_interest[num]])
        } else {
            stop("invalid num")
        }
        
        dataframe <- rbind(dataframe, list(hospital[1], state))
    }
    
    ## Return a data frame with the hospital names and the (abbreviated) state name
    dataframe <- as.data.frame(dataframe)
    colnames(dataframe) <- c('hospital','state')
    
    dataframe
    
}