# test the main ISAR function
library(spatstat)
library(devtools)
load_all(".")

if(1 & !exists("l1")){
  #source("tests/load-data.R")
  x <- lansing
  sx <- split(x)
  intensity <- lapply(sx, intensity)
  # constant intensities
  l0 <- rep(intensity(x), table(x$marks))
  # construct separate intensities
  l1 <- intensity_optimal(x, bw_vector = 0.1, verb = TRUE)
  # fields
  ims <- lapply(1:length(sx), function(i) density(sx[[i]], l1$bandwidth[i]))
}

par(mfrow=c(2,2))
for(i in 1:2+4){
  plot(ims[[i]])
  points(sx[[i]])
  k <- ISAR(x, i=i, CSR=F)
  ki <- ISAR(x, int=l1$i, i=i)
  plot(cbind(k,ki))
}

