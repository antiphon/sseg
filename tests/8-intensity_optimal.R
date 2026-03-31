# test the optimal intensity calculator
library(spatstat)
library(devtools)
load_all(".")

source("tests/load-data.R")

xx <- x
#x <- unmark(x[x$marks == x$marks[1]])

bwv <- c(seq(10, 500, length=50), 1000, 5000, 10000)

#p <- intensity_bandwidth_profile(x, bwv, scale=T, keep=T)
#p2 <- intensity_bandwidth_profile(x, bwv, scale=F, keep=T)

#plot(bwv,p2$err)
#plot(bwv,p$err)

#print( all.equal(p2$intensity_best, p$intensity_best) )
l <- intensity_optimal(x, bwv, verb=T)

z <- x[x$marks=="TAB2AR"]

#p <- intensity_bandwidth_profile(z, bwv, scale=T)
plot(bwv,p$err, log="y")
plot(density(z, p$bw_best))
points(z)

