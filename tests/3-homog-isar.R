# test homog ISAR

library(devtools)
load_all(".")

source("R/load-data.R")
library(spatstat)


r <- seq(0,20, length=100)
target <- 1

l <- unlist(sapply(split(x), function(v) rep(intensity(v), v$n)))

g0 <- ISAR(x, 1, r)
g1 <- ISAR_inhom(x, 1, r, l)

plot(r, g0[,2])
lines(r, g1[,2], col=2)
