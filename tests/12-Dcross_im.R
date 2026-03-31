# test the image intensity for D_inhomcross

library(devtools)
load_all(".")
library(spatstat)

x <- lansing

lambda <- intensity_optimal(x, verb=T)

int <- density_ppplist(split(x), lambda$bandwidth)

t1<-system.time(g1 <- Dinhom_cross(x, int=lambda$intensity))
t11<-system.time(g11 <- Dinhom_cross(x, int=int))

print(rbind(t1,t11))

plot(g1)
lines(g11, col=2, lty=2)
