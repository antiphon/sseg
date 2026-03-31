#' Spatial Minling index
#'
#' Compute the Mingling index for a given multitype point pattern. Assumes homogeneous
#' patterns.
#'
#' @param X Multitype point pattern of class \code{ppp} (see package 'spatstat')
#' @param r Vector o neighbourhoods scales
#' @param  target Default NULL. Calculate only for target type. If NULL computes
#'   for each type + mean over all types.
#' @param ratio Default FALSE. If TRUE, scale the typewise values \code{$M_t$}
#'   using formula \code{$(1-M_tau)/lambda_tau$} which equals 1 for Poisson CSR.
#' @param ntype The original mingling index uses \code{knn} neighbourhood type.
#'
#' @details Extension of Mingling index introduced by Lewandowski\&Pommerening
#' 1997. Measures the proportion of alien points in the neighbourhood of a
#' specific type typical point of the pattern.
#'
#' ntype is "geo" for geometric by default (look in disc with radius r).
#' The original definition was for "k-nearest neighbours" ("knn"),
#' with just a single scale, "r=4" where r=k in the knn.
#'
#' If no specific type is given, the function takes mean over all types. A
#' typewise value is more useful, so they are also included.
#'
#' @references
#' Graz: The behaviour of the species mingling index \code{$m_{sp}$}
#' in relation to species dominance and dispersion. Eur. J. forest research.
#' 123:87-92, 2004.
#'
#' Lewandowski, Pommerening: Zur Beschreibung der Waldstruktur - Erwartete und
#' beobachtete Arten-Durchmischung. Forstwiss Centralbl, 116:129-139, 1997.
#'
#' Rajala, Illian: A family of spatial biodiversity measures based on graphs,
#' Env. Ecol. Stat. 2012
#'
#' @export

mingling <- function(X, r, target=NULL, ratio=FALSE, ntype="geo", ...){
  stop("TODO")
}
