# rm(list = ls())

corr <- function(directory, threshold = 0) {
    
    ## directory : "specdata"
    ## threshold : 
    
    correlation = NULL
    setwd(file.path("~/Desktop/R programming",directory))
    
    for (i in 1:332) {
        id_format <- formatC(format = "d",i,flag = "0", width = 3) # width = ceiling(log10(max(id)))
        csv_file  <- read.csv(paste(id_format,".csv",sep = ""), header = TRUE)
        
        if (sum(complete.cases(csv_file)) > threshold) {
            data = csv_file[complete.cases(csv_file), ]
            correlation = c(correlation, cor(data$sulfate, data$nitrate))
        }
        
    }

    return(correlation)
    setwd("..")
}

# for test
cr <- corr("specdata", 150)
head(cr)
summary(cr)

cr <- corr("specdata", 400)
head(cr)
summary(cr)

cr <- corr("specdata", 5000)
summary(cr)
length(cr)

cr <- corr("specdata")
summary(cr)
length(cr)
