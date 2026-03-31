#' Inhomogeneous Nearest neighbour function
#'
#' The same as spatstat's Ginhom with correction = "border"
#'
#' @param x point pattern
#' @param r vector of ranges
#' @param intensity vector of intensities at points
#'
#' @import spatstat
#' @useDynLib sseg
#' @export

Dinhom <- function(x, r, intensity) {
  x <- check_pp(x)
  coord <- get_coords(x)
  bbox <- get_bbox(x) # not really used
  if(missing(r)) {
    rmax <- rmax.rule("G", x$window, intensity(x))
    r <- seq(0, rmax, l=50)
  }
  # check intenstity
  if(is(intensity, "int_profile_stack")) intensity <- intensity$intensity
  if(is.im(intensity)) {
    inf_int <- min(intensity)
    intensity <- intensity[x]
  }
  else if(is.numeric(intensity)){
    inf_int <- min(intensity)
  }
  else stop("Can not interpret 'intensity'")
  # border
  bd <- bdist.points(x)
  #
  out <- Dinhom_c(coord, bbox, intensity, bd, inf_int, r)
  out <- 1-out
  df <- data.frame(r=r, inhD=out)

  out <- fv(df, valu = "inhD", desc = c("range", "Inhomogeneous nearest neighbour cdf"),
            ylab =  "inhD(r)" )
}
