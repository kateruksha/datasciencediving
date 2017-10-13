# generate a plot
set.seed(1)
x <- rnorm(1000, mean = 5, sd=1)
y <- 5*x+7 + rnorm(1000, mean=2, sd=2)
plot(x,y)

# plot first component
abline(9,5.1, col = "red", lty = 1)
abline(117,-16.7, col = "blue")


# projection onto the first component
pc1 = x
O = rep(0,1000)
plot(pc1,O)

# straight line plot
y1 = 5*x + 7
plot(x,y1)


# projection onto two components
pc1<-x
pc2 <-  rnorm(1000, mean=2, sd=2)
plot(pc1,pc2)
