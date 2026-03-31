# test data
if(!file.exists("bci00_sub_bigger.rda")) {
library(bciexplorer)
data(bciall)
x <- bcisubsample(bci05, xlim=c(0,200), ylim=c(0,100), atleast=1000, status="A")
x <- bcisubsample(x, xlim=c(0,200), ylim=c(0,100), atleast=30, status="A")
saveRDS(x, "bci00_sub_bigger.rda")
}
x <- readRDS("bci00_sub_bigger.rda")
