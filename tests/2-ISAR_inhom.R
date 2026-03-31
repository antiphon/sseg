# ISAR Inhom
library(devtools)
load_all(".")

library(spatstat)

source("tests/load-data.R")
sx <- split(x)
lambda <- lapply(sx, intensity)

library(spatialsegregation)

# all marks to all marks
x0 <- unmark(x)
marks(x0) <- factor(as.integer(x$marks))

r <- seq(0, 30, length=20)

l1 <- intensity_optimal(x, bwv=seq(20, 500, l=20), v=T)$int
l0 <- rep(intensity(x0), table(x0$marks))

par(mfrow=c(4,4))
for(target in 1:16){
  t0 <- system.time( g0 <- isarF(x0, target=target, r = r) )
  t00 <- system.time( g00 <- ISAR_homog(x0, i=target, r = r) )
  t1 <- system.time( g1 <- ISAR_inhom(x0, i=target, r=r, lambda=l0) )
  # separate intensities for true inhomg ISAR
  t2 <- system.time( g2 <-  ISAR_inhom(x0, r=r, lambda = l1, i=target) )


  plot(r, g0$ISAR-g0$theo, "l", ylim=c(-1,1)*10)
  lines(r, g00[[3]]-g00[[1]], col=2)
  lines(r, g1[[2]]-g0$theo, lty=2, col=3)
  lines(r, g2[[2]]-g0$theo, lty=3, col=4)

  print(rbind(t0,t00, t1,t2))
}
