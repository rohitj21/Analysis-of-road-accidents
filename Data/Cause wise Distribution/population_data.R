library(rvest)
library(dplyr)
html = read_html("https://statisticstimes.com/demographics/india/indian-states-population.php")
pop_data =  html %>% html_table()
pop_data = data.frame(pop_data[[2]])
pop_data = pop_data[,2:3]
pop_data = pop_data[-1,]
pop_data = pop_data[-c(1,11,12,15,9,20,33,37),]
colnames(pop_data) = c("States" , "Population")
pop_data$Population = as.numeric(gsub(",","",pop_data$Population))
pop_data = pop_data[order(pop_data$States),]

tour_data = read.csv("StateWise_TouristData.csv")
save(pop_data, tour_data , file = "population_data.RData")
