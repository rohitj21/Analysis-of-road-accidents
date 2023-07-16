library(plotly)
load("Registered_vehicle.Rdata")
library(ggplot2)
p <- ggplot(city_info, aes(x = Two_wheeler, y = Cases, color = Population, label = City)) +
    geom_point(alpha= 0.6, aes(size = Deaths)) +
    geom_text(aes(label=ifelse(Population>2500000,City,''), size = 12),hjust=0,vjust=0, size = 3)
p <-p + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), alpha = .3)
ggplotly(p)

