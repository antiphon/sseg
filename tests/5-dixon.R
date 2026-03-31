# test dixon
#library(devtools)
#load_all(".")

library(aninISAR)
library(spatstat)
library(spatialsegregation)

data("amacrine")
x <- amacrine

v <- spatialsegregation::dixon(x)
w <- aninISAR::dixon(x)

print(all.equal(v,w))
