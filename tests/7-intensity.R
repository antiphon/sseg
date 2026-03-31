# Test intensity estimation
library(devtools)
load_all(".")

source("tests/load-data.R")
bwv <- seq(30, 400, l=10)
x1 <- x[x$marks==x$marks[[1]]]
v <- intensity_bandwidth_profile(x1)
w <- intensity_optimal(x, bwv=bwv, v=T)

z <- "PRI2CO"
x1 <- x[x$marks==z]

i <- intensity_im(x1, bw=w$bandwidth[names(w$b)==z])
plot(i)
points(x1, cex=.2, col="green")

p <- w$profiles
plot(p[,1], , "l", ylim=c(0,20))
apply(p, 2, lines)
