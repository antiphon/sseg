## ---- include=FALSE------------------------------------------------------
library(knitr)
opts_chunk$set(echo=TRUE, fig.height=6, fig.width=6)

## ---- fig.width=8, fig.height=5------------------------------------------
library(spatstat)
library(sseg)
xl <- split(lansing)
par(mar=c(0,0,0,0))
plot(xl, cex=.4, main="")

## ---- fig.width=6, fig.height=5------------------------------------------
A <- intensity_optimal(lansing, verb = T)
print(summary(A))
par(mfrow=c(2,3))
plot(A, i=1:6, type="l", log="y")

## ---- fig.width=8, fig.height=5------------------------------------------
ims <- density_ppplist(xl, sigma = A$bandwidth)
plot(ims, equal.ribbon = F)

## ------------------------------------------------------------------------
I <- ISAR(lansing, "hickory", CSR=TRUE)
plot(I)

## ------------------------------------------------------------------------
Lh <- Linhom(xl[["hickory"]], ims[["hickory"]], correction = "trans")
plot(Lh, .-r~r)

## ------------------------------------------------------------------------
Il <- lapply(names(xl), ISAR, x=lansing)
ISAR <- do.call(cbind, Il)
plot(ISAR)

## ------------------------------------------------------------------------
Iil <- lapply(names(xl), ISAR, x=lansing, intensity = A$intensity)
# or
# Iil <- lapply(names(xl), ISAR, x=lansing, intensity = ims)

inhISAR <- do.call(cbind, Iil)
plot(inhISAR)

## ------------------------------------------------------------------------
Idl <- lapply(seq_along(xl), function(s) {
    k <- Il[[s]]
    k[,2] <- Il[[s]]$ISAR - Iil[[s]]$inhISAR
    k
  })
difference <- do.call(cbind, Idl)
plot(difference)
abline(h=0)

