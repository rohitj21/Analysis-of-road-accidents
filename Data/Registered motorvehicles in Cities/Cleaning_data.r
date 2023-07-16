file1 <- "details-registered-motor-vehicles-transport-non-transport-million-plus-cities-india-31st.csv"
file2 <- "State-UT-City-Wise-Cause-Wise-Distribution-Of-Road-Accidents-And-Unmanned-Railway-Crossing-Accidents-During-2017.csv"
reg_city <- read.csv(file1)
reg_city[is.na(reg_city)] = 0
accident_data <- read.csv(file2)
# changing grater mumbai -> mumbai
reg_city[15,2] <- "Mumbai"


city_data <- accident_data[40:93, 49:51]
colnames(city_data) <- c("Cases", "Injured", "Deaths")
rownames(city_data) <- accident_data$State.UT.City[40:93]

vehical_data <- reg_city[1:43, c(18, 19, 23, 29)]
vehical_data[,2] <- vehical_data[,2] + reg_city[1:43, 20] + reg_city[1:43, 21]
rownames(vehical_data) <- reg_city[1:43,2]
colnames(vehical_data) <- c("Transport", "Two_wheeler", "Cars", "Non_transport")


rownames(city_data)[c(9,12 )] <- c("Chandigarh", "Delhi")
# combining both datasets
City1<- rownames(city_data)
City2 <- rownames(vehical_data)
index1 <- numeric(length = length(City1))
index2 <- numeric(length = length(City2))
for(i in 1:length(City1)){
    index1[i] <- sum(City1[i] == City2 )
}

for(i in 1:length(City2)){
    index2[i] <- sum(City2[i] == City1 )
}
city_data <- city_data[index1==1,]
vehical_data <- vehical_data[index2==1,]

city_data<-  city_data[order(rownames(city_data)),]
vehical_data <-  vehical_data[order(rownames(vehical_data)),]

city_vehicle_data <- data.frame(city_data, vehical_data)
save(city_vehicle_data,file = "Registered_vehicle.Rdata")


library(tidyverse)
library(rvest)
# population data of cities
pop_dat <- html_table(read_html("https://en.wikipedia.org/wiki/List_of_cities_in_India_by_population"))[[1]][, 2:3]

pop_dat

pop_dat$City <-  str_replace_all(pop_dat$City, "[^[:alnum:]]", "")

pop_dat$City <- str_replace_all(pop_dat$City, "0", "")
pop_dat$City <- str_replace_all(pop_dat$City, "1", "")
pop_dat$City <- str_replace_all(pop_dat$City, "2", "")
pop_dat$City <- str_replace_all(pop_dat$City, "3", "")
pop_dat$City <- str_replace_all(pop_dat$City, "4", "")
pop_dat$City <- str_replace_all(pop_dat$City, "5", "")
pop_dat$City <- str_replace_all(pop_dat$City, "6", "")
pop_dat$City <- str_replace_all(pop_dat$City, "7", "")
pop_dat$City <- str_replace_all(pop_dat$City, "8", "")
pop_dat$City <- str_replace_all(pop_dat$City, "9", "")



pop_dat[41,2] <- "1,050,721"

colnames(pop_dat)[2]<- "Population"
pop_dat$Population <- as.numeric(gsub(',', '', pop_dat$Population))

City3 <- (pop_dat$City)
index3 <- numeric(length = length(pop_dat$City))
for(i in 1:length(pop_dat$City)){
    index3[i] <- sum(City3[i] ==  rownames(city_vehicle_data))
}
pop_dat <- pop_dat[index3==1,]
pop_dat <- data.frame(pop_dat[order(pop_dat$City),])

# aurangabad has duplicate entries in pop_dat
pop_dat <- pop_dat[c(1:4, 6:34),]

City4 <- rownames(city_vehicle_data)
index4 <- numeric(length = length(City4))
for(i in 1:length(index4)){
    index4[i] <- sum(City4[i] ==  pop_dat$City)
}
city_vehicle_data <- city_vehicle_data[index4== 1,]

# chech if rows are aligned
city_info <- data.frame("City" = rownames(city_vehicle_data), city_vehicle_data, "Population" = pop_dat$Population)

save(city_info, file = "Registered_vehicle.Rdata")

