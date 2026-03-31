# test the optimal intensity calculator, leaveout vs leaveout=FALSE
library(spatstat)
library(devtools)
load_all(".")

source("tests/load-data.R")
xl <- split(x)
x <- xl[[15]]

l1 <- intensity_bandwidth_profile(x, verb=T, leaveoneout = TRUE,  edge=FALSE)
l2 <- intensity_bandwidth_profile(x, verb=T, leaveoneout = FALSE, edge=FALSE)

plot(l1, log="y", type="l")
lines(l2$profile, col=2)

bw_vector <- seq(50,3000, by = 50)

# wth
y <- x
S<-Se <- NULL
for(bw in bw_vector) {
  o <- density.ppp(y, sigma = bw, at = "points", leaveoneout = FALSE, edge=!TRUE)
  Se <- c(Se, sum(1/o))
  o <- density.ppp(y, sigma = bw, at = "points", leaveoneout = TRUE, edge=!TRUE)
  S <- c(S, sum(1/o))
}

a <- area(y)
plot(bw_vector, S-a, "l")
lines(bw_vector, Se-a, col=2)
#plot(bw_vector, S-a, col=2)
abline(h = 0, col=3)


#l0 <- intensity_optimal(x, bw_vector = c(10, 50, 1000, 10000), leaveoneout=F)
l0 <- intensity_optimal(x, leaveoneout=F)
plot(l0$profiles$`1`, log="y")
i0 <- density(x, sigma = l0$bandwidth)
plot(i0)


# Hmm
D <- pairdist(x)
est <- function(bw){
  v <- apply(D/bw, 1, dnorm)
  l <- rowSums(v)/bw
  s <- sum(1/l)
}

l0 <- density.ppp(x, at="points", edge=F, leaveoneout = F, sigma = bw)


S <- NULL
for(bw in bw_vector * 5){
  S <- c(S, est(bw))
}

