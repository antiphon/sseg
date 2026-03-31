# dev and chekc minling index

devtools::load_all(".")
library(spatstat)

x <- lansing
i<-2
if(0){
y <- mingling_index(x,  i = i, correction = "b", version=1)
y2 <- mingling_index(x,  i = i, correction = "b", version=2)
z <- spatialsegregation::minglingF(x, target = levels(x$marks)[i], r = y$r, minusRange = TRUE)

plot(z, ylim = c(0,1), lty=2)
lines(y, col=1, lwd=2)
lines(y2, col=2, lwd=2)
}

if(0){
  y <- mingling_index(x,  i = i, correction = "b", version=1)

}


if(1){
  y <- mingling_index(x,  i = i, ntype="knn", r = 1:100)
  plot(y, ylim = c(0,1), lty=2)
}
