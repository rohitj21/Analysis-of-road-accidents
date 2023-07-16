library(rvest)
data = html_table(read_html("https://www.classmate4u.com/latitude-and-longitude-of-india/"))
data = data.frame(data[[1]])
data = data[,-3]
data = data[-1,]
data = rbind(data,c("Gujarat","22.3094° N"))
data = data[order(data$X1),]
colnames(data) = c("States and UTs", "Latitude")
read = read.csv("total-no-of-accident-2016_2017_2018_2019.csv")

read$X2018 = as.numeric(gsub(",","",read$X2018))

data$Accidents_2018 = read$X2018

data$Latitude = as.numeric(gsub("° N", "", data$Latitude))

library(ggplot2)

#2018
load("population_data.Rdata")

X <- data.frame(data$Latitude, data$Accidents_2018, data$`States and UTs`)
X <- X[-c(1,6,8,9,10,19,27), ]
colnames(X) = c("Latitude", "Accidents_2018", "State")
X<- data.frame(X, "Population" = as.character( round(log10(pop_data$Population))))

p <- ggplot(X, aes(x = Latitude, y = log(Accidents_2018) ,label = `State`, color = `Population`) )+
     geom_point() + #geom_smooth(method = "lm",  formula = y ~ x, alpha = .0)+
     labs(x = "Latitude in Degrees", y = "Natural log of Number of Accidents") + ggtitle("Number of Accidents V/S Latitude for Indian States") +
    theme_classic() +theme(plot.title=element_text(hjust=0.5)) + theme(plot.title = element_text(face="bold"))
latitude2018 <- p + guides(color =guide_legend(title="Log10 of the Population")) + ylim(min =0 , max = 16)
latitude2018

save(latitude2018, file = "latitude.Rdata")

