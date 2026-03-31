# check rPSNC simulation

library(devtools)
load_all(".")

library(spatstat)

range <- c(0.1, 0.05, .01)

xi <- rbind(c(0,-1, 0),
            c(-1,0, 0),
            c(1, 10, 0))

kappa <- c(5,15, 30)
window <- square()
rho <- c(300, 300,500)


x <- rPSNC(rho=rho, kappa=kappa, alpha = xi, omega = range)


# ISAR?
r <- seq(0, .3, length=50)
i1 <- ISAR(x, 1, r)
i2 <- ISAR(x, 2, r)
i3 <- ISAR(x, 3, r)

par(mfrow=c(2,1))
plot(x, cex=.4, cols=c(1,2,3))
plot(i1[,-3], t= "l")
lines(i2[,-3], col=2)
lines(i3[,-3], col=3)
legend("bottomright", col=1:3, le=1:3, lty=1)
