library(ggplot2)
require(grid)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#########################################################
g <- function(x) { (1 - exp( - 2 * exp(x) ) )^2 }
h <- function(x) { (1 - exp( - 0.5 * exp(x) ) )^0.5 }
i <- function(x) { (1 - exp( - 3 * exp(x) ) )^3 }
f <- function(x) { (1 - exp( - 1 * exp(x) ) )^1 }


## Plots of a=b, full view
a1 <- ggplot(data.frame(x=c(-4, 4)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))


## Plots of a=b, zoomed right 
a2 <- ggplot(data.frame(x=c(0.5, 2)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))


## Plots of a=b, zoomed left
a3 <- ggplot(data.frame(x=c(-3, -1)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))

## Put plots into one grid
multiplot(a1, a3, a2, cols=1)


############################################################
g <- function(x) { (1 - exp( - 1 * exp(x) ) )^2 }
h <- function(x) { (1 - exp( - 1 * exp(x) ) )^0.5 }
i <- function(x) { (1 - exp( - 1 * exp(x) ) )^3 }
f <- function(x) { (1 - exp( - 1 * exp(x) ) )^1 }


## Plots of a=1, b varied, full view
b1 <- ggplot(data.frame(x=c(-4, 4)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))


## Plots of a=1, b varied, zoomed right
b2 <- ggplot(data.frame(x=c(0.5, 2)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))


## Plots of a=1, b varied, zoomed left
b3 <- ggplot(data.frame(x=c(-3, -1)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))

## Put plots into one grid
multiplot(b1, b3, b2, cols=1)


###################################################
g <- function(x) { (1 - exp( - 2 * exp(x) ) )^1 }
h <- function(x) { (1 - exp( - 0.5 * exp(x) ) )^1 }
i <- function(x) { (1 - exp( - 3 * exp(x) ) )^1 }
f <- function(x) { (1 - exp( - 1 * exp(x) ) )^1 }


## Plots of a varied, b=1, full view
c1 <- ggplot(data.frame(x=c(-4, 4)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))


## Plots of a varied, b=1, zoomed right
c2 <- ggplot(data.frame(x=c(0.5, 2)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))


## Plots of a varied, b=1, zoomed left
c3 <- ggplot(data.frame(x=c(-3, -1)), aes(x)) +
stat_function(fun=g, geom="line", colour = "red", size = 1) + 
stat_function(fun=h, geom="line", colour = "blue", size = 1) +
stat_function(fun=i, geom="line", colour = "green", size = 1) + 
stat_function(fun=f, geom="line", colour = "black", size = 1) + 
labs(x = "u", y = "F(u)") +
theme(text = element_text(size=15))

## Put plots into one grid
multiplot(c1, c3, c2, cols=1)


