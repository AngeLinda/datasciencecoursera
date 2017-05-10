rm(list = ls())

pollutantmean <- function(directory, pollutant, id = 1:332){
    
    ## directory : "specdata"
    ## pollutant : "sulfate" or "nitrate"
    ## id : integer between 1 : 332
    
    observations <- 0
    select_data_sum <- 0
    setwd(file.path("~/Desktop/R_programming",directory))
  
    for (i in id) {
        id_format <- formatC(format = "d",i,flag = "0", width = 3) # width = ceiling(log10(max(id)))
        csv_file  <- read.csv(paste(id_format,".csv",sep = ""), header = TRUE)
        
        # data <- na.omit(csv_file)
        # observations = observations + nrow(data)
        # 
        # if (pollutant == "sulfate") {
        #     select_data_sum = select_data_sum + sum(data$sulfate)
        # }
        # if (pollutant == "nitrate") {
        #     select_data_sum = select_data_sum + sum(data$nitrate)
        # }
        
        observations = observations + sum(!is.na(csv_file[[pollutant]]))
        select_data_sum = select_data_sum + sum(csv_file[[pollutant]], na.rm = TRUE)
    }
    
    return(select_data_sum/observations)
    setwd("..")
}

# for test
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
