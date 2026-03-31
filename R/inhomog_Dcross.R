#' Inhomogeneous cross-Nearest neighbour function
#'
#' Compute the inhomogeneous D_ij(r). (or G_ij as in spatstat)
#'
#' @param x point pattern
#' @param i Type from which to look. Index number(s).
#' @param j Type(s) to which to look. Index number(s).
#' @param intensity A vector, giving at point (x,m) the intensity intensity_m(x)
#' @param v2 dev, dont use.
#' @details
#' intensity could also be an im-object.
#' Note the difference: The infimum needed for the estimator
#' will be computed from the image, not just data locations.
#'
#' @import spatstat
#' @useDynLib sseg
#' @export

Dinhom_cross <- function(x, r, intensity, i=1, j=2, v2=FALSE) {
  from <- i
  to <- j
  if(!is.ppp(x)) stop("x should be a ppp object")
  #
  # parse marks
  marks <- parse_marks(x)
  marknames <- levels(marks)
  marksi <- as.integer(marks)
  nmarks <- length(unique(marks))
  if(nmarks < 2) stop("x should be at least bivariate.")
  npoints <- table(marks)
  #
  coord <- as.matrix(coords(x))
  bbox <- apply(coord, 2, range)
  el <- apply(bbox, 2, diff)
  #
  if(missing(r)) {
    rmax <- rmax.rule("G", x$window, intensity(x[marksi==i]))
    r <- seq(0, rmax, l=50)
  }
  # targets
  if(missing(from)) from <- marknames[1]
  if(missing(to)) to <- marknames[2]
  if(is.numeric(from)) from <- marknames[from]
  if(is.numeric(to)) to <- marknames[to]
  if(length(from)>1) {from <- from[1]; warning("Using only the first element of 'i'")}
  if(length(to)>1) {to <- to[1]; warning("Using only the first element of 'j'")}

  fromi <- match(from, marknames)
  toi <- match(to, marknames)
  if(is.na(fromi)) stop("'i' not understood.")
  if(is.na(toi)) stop("'j' not understood.")


  # check intenstity. Needs to be a list of images
  if(is(intensity, "int_profile_stack")) intensity <- intensity$intensity
  if(is.list(intensity)){
    if(length(intensity) < nmarks) stop("'intensity' im-stack needs to be of length",nmarks)
    inf_int <- sapply(intensity, min)
    # per point, need only to-points
    int0 <- rep(0, x$n)
    int0[marksi == toi] <- intensity[[toi]][x[marksi==toi]]   # per point
    intensity <- int0
  }
  else if(is.numeric(intensity)){
    if(length(intensity) != x$n) stop("'intensity' vector should be of length", x$n)
    inf_int <- sapply(split(intensity, marksi), min)
  }
  else stop("Can't interpret 'intensity'")

  # border correction
  bd <- bdist.points(x)
  # go
  out <- if(v2)
      Dinhom_pair_graph_c(fromi, toi, coord, bbox, intensity, marksi, bd, inf_int, r, nmarks)
    else
      Dinhom_pair_2_c(fromi, toi, coord, bbox, intensity, marksi, bd, inf_int, r, nmarks)
  #
  out <- 1-out
  #
  df <- data.frame(r=r, inhDcross=out)
  # convert to spatstat's format
  out <- fv(df, valu = "inhDcross", desc = c("range", "Inhomogeneous nearest neighbour cdf"),
            ylab =  substitute(Dinhom[list(i, j)](r), list(i=from, j=to )))
  out
}
