# rm(list = ls())

complete <- function(directory, id = 1:332) {
    
    ## directory : "specdata"
    ## id : integer between 1 : 332
    
    dataframe = NULL    
    setwd(file.path("~/Desktop/R programming",directory))
    
    for (i in id) {
        id_format <- formatC(format = "d",i,flag = "0", width = 3) # width = ceiling(log10(max(id)))
        csv_file  <- read.csv(paste(id_format,".csv",sep = ""), header = TRUE)
        
        dataframe = rbind(dataframe,c(i,sum(complete.cases(csv_file)))) 
        
    }
    dataframe = data.frame(dataframe)
    names(dataframe) = c("id","nobs")
    
    return(dataframe)
    setwd("..")
}

# for test
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)