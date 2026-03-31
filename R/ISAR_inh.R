#' Inhomogeneous ISAR
#'
#' @param x Point pattern
#' @param i Target species
#' @param r r vector
#' @param intensity Intensity lambda_m(x) at each point (x,m)
#' @param lambda same as intensity
#' @param ...
#'
#' @export

ISAR_inhom <- function(x, i, r, intensity, lambda = intensity, ...){
  m <- parse_marks(x)
  mi <- as.integer(m)
  marknames <- levels(m)
  nm <- length(unique(mi))
  if(missing(intensity)) intensity <- lambda
  if(nm<2) stop("x needs to be at least bivariate.")

  # range
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

  # run
  l <- sapply(others, function(j) Dinhom_cross(x, r, intensity, i=i, j=j))
  # gather
  out <- rowSums(sapply(l[2,], c))
  df <- data.frame(r=r)
  df[[fn<-paste0("inhISAR_",i)]] <- out

  fv(df, valu=fn, fmla = formula(.~r), ylab =  substitute(inhISAR[list(i)](r), list(i=i)),
     desc = c("range", paste("Inhomogeneous ISAR for type", i)))

}
