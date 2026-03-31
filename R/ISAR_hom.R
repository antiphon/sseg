#' Individual species-area-interaction or ISAR function
#'
#'
#' @param x Point pattern
#' @param i i species
#' @param r r vector
#' @param CSR Include CSR? (not very useful)
#'
#' @details CSR mean everything is Poisson. Not usually a realistic model.
#'
#'
#' @import spatstat
#' @export

ISAR_homog <- function(x, i, r, CSR=TRUE){
  m <- parse_marks(x)
  mi <- as.integer(m)
  marknames <- levels(m)
  nm <- length(unique(mi))
  #x$marks <- as.factor(mi)
  #
  if(missing(r)){
    rmax <- rmax.rule("K", x$window, intensity(x))
    r <- seq(0, rmax, length = 50)
  }
  # check i
  if(missing(i)) i <- marknames[1]
  if(is.numeric(i)) i <- marknames[i]
  if(any(is.na(match(i, marknames)))) stop("'i' not understood.")
  if(length(i)>1) {i <- i[1]; warning("Using only the first element of 'i'")}
  others <- setdiff(marknames, i)

  # collect the empty space functions
  l <- sapply(others, function(j){
    v <- Gcross(x, r, i=i, j=j, correction="rs")
    data.frame(v$r, v$rs, v$theo)
  }
  )
  out <- rowSums(sapply(l[2,], c))
  theo <- rowSums(sapply(l[3,], c))
  df <- data.frame(r=r)
  if(CSR) df$CSR <- theo
  df[[fn<-paste0("ISAR_",i)]] <- out
  fv(df, valu=fn, ylab =  substitute(ISAR[list(i)](r), list(i=i)),
     fmla = formula(.~r),
     desc = c("range", if(CSR) "Value when everything is independent and Poisson" else NULL,
              paste("ISAR for type", i)))
}
