library(stringr)
library(dplyr)
library(rvest)
# since every years data is in the same format we can make a function to clean the csv files

clean <- function(file_name){
    x <- data.frame(read.csv(file_name, header = FALSE, skip = 1))
    column_names <- t(read.csv(file_name, header = FALSE, nrows = 1))

    # this dataset contains the data for cities too

    state_data <- x[1:29,3:42]
    colnames(state_data) <- column_names[3:42]

    # every cause of accident is associated with 3 columns in the dataframe
    # 1. number of accidents due to that cause
    # 2. number of deaths due to that cause
    # 3. number of injuries due to that cause
    # identify unique causes (13 in number)
    # last 3 columns are for total accidents, injuries and deaths
    v <- (3*(1:13))
    causes <- column_names[1 + v] %>% str_sub(1, -9)
    # separating diff data
    state_death <- data.frame((state_data[(1+v )]))
    state_cases <- data.frame((state_data[-1+v]))
    state_injured <- data.frame((state_data[v]))

    # setting column names to states/ut/city

    rownames(state_cases) <-   x[1:29, 3]
    rownames(state_death) <-   x[1:29, 3]
    rownames(state_injured) <- x[1:29, 3]
    # setting rownames as causes



    # removing unnecessary columns
    state_cases    <-data.frame(state_cases[,-c(8,9)])
    state_death    <-data.frame(state_death[,-c(8,9)])
    state_injured  <-data.frame(state_injured[,-c(8,9)])

    causes <- c(
        "Careless Driving or Over-taking"
        ,"Over Speeding"
        ,"Driving under Influence of Drug or Alcohol"
        ,"Physical Fatigue of Drivers"
        ,"Defect in Motor Vehicle"
        ,"Animal Crossing"
        ,"Weather Condition"
        ,"Lack of Road Infrastructure"
        ,"Parked Vehicles"
        ,"Causes Not Known"
        ,"Other Causes"
    )

    colnames(state_cases) <-   causes
    colnames(state_death) <-   causes
    colnames(state_injured) <- causes


    return(list("state_death" =  state_death,
                "state_injured"= state_injured,
                "state_cases" =  state_cases))
}
allData <- NULL
file_name <- c("State-UT-City-Wise-Cause-Wise-Distribution-Of-Road-Accidents-And-Unmanned-Railway-Crossing-Accidents-During-2015.csv",
                "State-UT-City-Wise-Cause-Wise-Distribution-Of-Road-Accidents-And-Unmanned-Railway-Crossing-Accidents-During-2016.csv",
               "State-UT-City-Wise-Cause-Wise-Distribution-Of-Road-Accidents-And-Unmanned-Railway-Crossing-Accidents-During-2017.csv",
                "State-UT-City-Wise-Cause-Wise-Distribution-Of-Road-Accidents-And-Unmanned-Railway-Crossing-Accidents-During-2018.csv",
               "State-UT-City-Wise-Cause-Wise-Distribution-Of-Road-Accidents-And-Unmanned-Railway-Crossing-Accidents-During-2019.csv",
               "State-UT-City-Wise-Cause-Wise-Distribution-Of-Road-Accidents-And-Unmanned-Railway-Crossing-Accidents-During-2020.csv"
               )
for(i in 1:6){
    allData[[i]] <- clean(file_name[i])

}

# In 2020 the data for jammu and kashmir was among the UT so this has to be added manually
JnK <- data.frame(read.csv(file_name[6]))[34,3:42]
JnK <- JnK[,-c(23:28)]
allData[[6]][[1]][29,] <-   JnK[1,(1+3*(1:11))]
allData[[6]][[2]][29,] <-   JnK[1,(3*(1:11))]
allData[[6]][[3]][29,] <-   JnK[1,(-1+3*(1:11))]

rownames(allData[[6]][[3]])[29] <- "Jammu & Kashmir"
rownames(allData[[6]][[1]])[29] <- "Jammu & Kashmir"
rownames(allData[[6]][[2]])[29] <- "Jammu & Kashmir"


# reordering rows aphabaticaly
for(i in 1:6){
    for(j in 1:3){
        allData[[i]][[j]] <- data.frame("State" = rownames(allData[[i]][[j]]), allData[[i]][[j]])
        allData[[i]][[j]] <- allData[[i]][[j]][order(allData[[i]][[j]][,1]), ]

    }
}

for(i in 1:6){
    for(j in 1:3){
        allData[[i]][[j]][,1] <- allData[[6]][[1]][,1]
        rownames(allData[[i]][[j]]) <- allData[[6]][[1]][,1]
    }
}
# Changeing column names to better names
causes <- c(
    "State"
    ,"Careless Driving or Over-taking"
    ,"Over Speeding"
    ,"Driving under Influence of Drug or Alcohol"
    ,"Physical Fatigue of Drivers"
    ,"Defect in Motor Vehicle"
    ,"Animal Crossing"
    ,"Weather Condition"
    ,"Lack of Road Infrastructure"
    ,"Parked Vehicles"
    ,"Causes Not Known"
    ,"Other Causes"
)


for(i in 1:6){
    for(j in 1:3){
        colnames(allData[[i]][[j]]) <- causes
    }
}
# now we add population data


html = read_html("https://statisticstimes.com/demographics/india/indian-states-population.php")
pop_data =  html %>% html_table()
pop_data = data.frame(pop_data[[2]])
pop_data = pop_data[,2:3]
pop_data = pop_data[-1,]
pop_data = pop_data[-c(1,11,12,15,9,20,33,37),]
colnames(pop_data) = c("States" , "Population")
pop_data$Population = as.numeric(gsub(",","",pop_data$Population))
pop_data = pop_data[order(pop_data$States),]


# tourist data
("StateWise_TouristData.csv")
allDataTourist <- allData
for(i in 1:6){
    for(j in 1:3){
        for(k in 2:12){
            allDataTourist[[i]][[j]][,k] <- (allData[[i]][[j]][,k] / pop_data) * 1000000
        }
    }
}
for(i in 1:6){
    for(j in 1:3){
        for(k in 2:12){
            allData[[i]][[j]][,k] <- (allData[[i]][[j]][,k] / pop_data$Population) * 1000000
        }
    }
}


save(allData, file = "all_state_Data.Rdata")

