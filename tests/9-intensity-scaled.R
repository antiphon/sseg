# intensity profiling, scaling issue

library(devtools)
load_all(".")

library(spatstat)

x <- runifpoint(200, owin(c(0,1),c(0,2)))

print(intensity(x))

x <- affine(x, diag(2,2))

bw_vector <- bwv <- seq(.1, 10, length=105)

i <- intensity_bandwidth_profile(x, bw_vector = bwv, scale=T, edge=F)
#j <- intensity_bandwidth_profile(x, bw_vector = bwv, scale=T, b=0)
j <- intensity_bandwidth_profile(x)
print(c(i$bw_best, j$bw_best))
par(mfrow=c(2,2))

print(cbind(range(i$intensity_best), range(j$intensity_best)))
with(i, plot(bw, err, log="y"))
with(j, plot(bw, err, log="y"))
plot(density(x, i$bw_best), zlim=c(0,50)); points(x, cex=.2)
plot(density(x, j$bw_best), zlim=c(0,50)); points(x, cex=.2)

#print(c(sum(1/i$i), sum(1/j$i), area(x) ) )
