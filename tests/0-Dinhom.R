# compare Dinhom to spatstat
# uses graph now

library(devtools)
load_all(".")
library(intensitybw)

library(spatstat)
x <- rThomas(20, 0.04, 50)
lambda <- density(x, at = "points")

t0<-system.time(g0 <- Ginhom(x, lambda = lambda))

t1<-system.time(g1 <- Dinhom(x, g0$r, lambda))

print(rbind(t0,t1))

plot(g1$r, g1$inhD, lty=3, col=6, lwd=3, "l")
lines(g0$r, g0$bord, col=1, lty=2)

print(all.equal(g0$Dinhom, g1$Dinhom))
