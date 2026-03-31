# test the image intensity for D_inhom

library(devtools)
load_all(".")
library(spatstat)

x <- nztrees

lambda <- intensity_optimal(x, verb=T)

int <- density(x, lambda$bandwidth)


t0<-system.time(g0 <- Ginhom(x, lambda = int))
t00<-system.time(g00 <- Ginhom(x, lambda = lambda$intensity))

t1<-system.time(g1 <- Dinhom(x, g0$r, lambda$intensity))
t11<-system.time(g11 <- Dinhom(x, g0$r, int))

print(rbind(t0,t00,t1,t11))

plot(cbind(g0,g00,g1,g11))
