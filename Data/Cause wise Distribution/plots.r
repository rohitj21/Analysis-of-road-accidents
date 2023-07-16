load("all_state_Data.Rdata")
library(ggplot2)
library(reshape2)
X <- melt(allData[[6]][[1]], value.name = "Deaths", variable.name = "Cause")
p <- ggplot(X, aes(State,Deaths, fill = Cause)) +
    geom_bar(stat = 'identity') + coord_flip() + theme_classic()
p
