library(rvest)
library(tidyverse)
Guj_data <- data.frame(html_table( read_html("https://cot.gujarat.gov.in/road-accidents.htm"))[[1]])
Guj_data[13,1] <- "2010"
Guj_data <- data.frame(lapply(Guj_data[6:20,1:4], as.numeric))
colnames(Guj_data) <- c("Year", "Accidents", "Deaths", "Injured")
X_Gj <- melt(Guj_data[,2:4], variable.name = "Type", value.name = "Incidents")
X_Gj <- data.frame(X_Gj, "Year" = Guj_data$Year)
save(Guj_data, X_Gj ,file = "Guj_data.Rdata")
