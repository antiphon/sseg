# test graph_c
library(devtools)
load_all(".")


library(spatgraphs)
x <- matrix(runif(200),ncol=2)


g0 <- g <- spatgraph(x, "knn", 4, maxR = R<-2)
e <- graph_c(x, diag(1,1), 2, 4, MaxR = R)

print(all.equal(e, g0$edges))

g0$edges <- e

par(mfrow=c(2,1))
plot(g,x)
plot(g0,x)
