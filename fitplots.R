## Menarche Fit Comparisons
library(ggplot2)
menarche <- read.csv("menarche.csv", header=TRUE)


## Comparison of prediction fits for the Menarche dataset
## (cloglog versus generalized cloglog)
ggplot(data = menarche, aes(Age, ActualRate)) + 
geom_line(aes(x = Age, y = CLogLog), colour="green", size=3) + 
geom_line(aes(x = Age, y = GenCLogLog), colour="blue", size=3) +
geom_point(aes(x = Age, y = ActualRate), colour="black", size=5) + 
xlab("Age") + 
ylab("Proportion at Menarche") +
theme(text = element_text(size=20))


## Bacteria Fit Comparisons
bacteria <- read.csv("bacteria.csv", header=TRUE)
ggplot(data = bacteria, aes(TimeInd, PopDensity)) + 
geom_line(aes(x = TimeInd, y = CLogLog), colour="green", size=3) + 
geom_line(aes(x = TimeInd, y = GenCLogLog), colour="blue", size=3) +
geom_point(aes(x = TimeInd, y = PopDensity), colour="black", size=5) + 
xlab("Time Index") + 
ylab("Bacteria Population Density") +
theme(text = element_text(size=20))

