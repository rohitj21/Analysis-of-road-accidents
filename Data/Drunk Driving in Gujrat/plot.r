library(ggplot2)
library(reshape2)
load("Guj_data.Rdata")
X <- melt(Guj_data[,2:4], variable.name = "Type", value.name = "Incidents")
X <- data.frame(X, "Year" = Guj_data$Year)
p <- ggplot(X, aes(x = Year, y = Incidents, color = Type) )
p <- p + geom_point() + geom_smooth(method = "gam",  formula = y ~ s(x, bs = "cs"), alpha = .2) + geom_vline(xintercept = 2009, col = "brown", lwd = 1)
p + geom_text(aes(x = 2009, y = 20000 , label = "\n ALCOHOL BANNED") , color = "brown", angle = 90, lwd = 4)

