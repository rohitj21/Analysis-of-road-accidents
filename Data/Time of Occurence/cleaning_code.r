dat <- list()
for(i in 2008:2018){
    dat[[i-2007]] = data.frame(read.csv(paste('time_of_occurence_', i, '.csv', sep = '')))
    colnames(dat[[i-2007]]) <- c("Time of Occurence","Number of Accidents" , "Day or Night")

    dat[[i-2007]][,2] <-  as.numeric(gsub(',', '', dat[[i-2007]][,2] ))
    print(dat[[i-2007]])
}
TimeWiseData <- dat
save(TimeWiseData,file=  "Time_wise_data.Rdata")
