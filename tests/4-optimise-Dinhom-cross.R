# compare Dinhom_cross versions, optimisation
# removed the options, checks cleared and was a lot faster
if(0){
library(devtools)
load_all(".")

source("tests/load-data.R")
sx <- split(x)
lambda <- lapply(sx, intensity)

# all marks to all marks
x0 <- unmark(x)
marks(x0) <- factor(as.integer(x$marks))
l0 <- rep(intensity(x0), table(x0$marks))

r <- seq(0, 15, l=100)
#t0 <- system.time(g0 <- Gcross(x0, 1,2,r,correction="rs"))

t1 <- system.time(g1 <- Dinhom_cross(x0, r=r, lambda = l0, from=1, to=2))
t2 <- system.time(g2 <- Dinhom_cross(x0, r=r, lambda = l0, from=1, to=2, v2=T))

print(all.equal(g1$Dinhom, g2$Dinhom))

print(rbind(t1,t2))

}
