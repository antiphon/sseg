# test the image intensity for ISAR_inhom

library(devtools)
load_all(".")
library(spatstat)

x <- lansing

if(0)lambda <- intensity_optimal(x, verb=T)

int <- density_ppplist(split(x), lambda$bandwidth)

t1<-system.time(g1 <- ISAR(x, i=4, int=lambda))
t11<-system.time(g11 <- ISAR(x, i=4, int=int))

print(rbind(t1,t11))

plot(g1)
lines(g11, col=2, lty=2)

