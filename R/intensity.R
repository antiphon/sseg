#' #' Estimate the intensity at points using optimal bandwidth and within a type
#' #'
#' #' @param x point pattern, ppp object
#' #' @param bwv Vector of bandwidths to optimise over
#' #' @param verb verbosity
#' #' @param ... passed on to \code{\link{intensity_optimal_bw}} from package 'intensitybw'
#' #' @param border see \code{\link{intensity_at_points}}
#' #' @details
#' #' First, we split the pattern per type.
#' #'
#' #' Then we optimise the
#' #' kernel bandwidth using \code{\link{intensity_optimal_bw}}.
#' #'
#' #' Then we re-estimate with border correction.
#' #'
#' #' Finally, we concatenate the estimates to a vector.
#' #'
#' #' Caution: For a big pattern with many types this will take time.
#' #
#' #' @return
#' #' A list with: a vector of intensities, one per point; the profiles; the best bandwidths.
#' #'
#' #' @import spatstat intensitybw
#' #' @export
#' intensity_optimal <- function(x, bwv, verb=FALSE, ..., border = "local"){
#'   x <- check_pp(x)
#'   if(!is.marked(x)) x <- setmarks(x, factor(1))
#'   m <- levels(x$marks)
#'   bbox <- with(boundingbox(x$window), cbind(xrange, yrange))
#'
#'   # If bwv missing, generate one
#'   if(missing(bwv)) {
#'     sl <- apply(bbox, 2, diff)
#'     bwv <- exp(seq(log(0.001), log(2), l = 30)) * mean(sl)
#'   }
#'
#'
#'   int <- rep(NA, x$n)
#'   cat2 <- if(verb) cat else function(...) NULL
#'   obw <- NULL
#'   errs <- NULL
#'   cat2(length(m), "types: ")
#'   profs <- list()
#'   for(k in seq_along(m)) {
#'     i <- which( x$marks == m[k] )
#'     xi <- x[i]
#'     pf <- intensitybw::intensity_optimal_bw(x=xi, bw = bwv, ...)
#'     # re-estimate intensity with border correction:
#'     int_i <- intensitybw::intensity_at_points(as.matrix(coords(xi)), bbox = bbox, bw = pf$opt, ...)
#'     int[i] <- int_i
#'     errs <- cbind(errs, err1 <- pf$loss)
#'     # check
#'     bi <- which.min(err1)
#'     if( ed<- bi %in% c(1, length(pf$bw)) ) warning(paste0(m[k], ": optimum at edge of bandwith grid"))
#'     pf$edge <- ed
#'     pf$bw_best <- pf$opt
#'     obw[k] <- pf$opt
#'     profs[[m[k]]] <- pf
#'     cat2(k, "")
#'   }
#'   colnames(errs) <- names(obw) <- m
#'   names(profs) <- m
#'   cat2("\n")
#'   #
#'   o <- list(intensity=int, bandwidth=obw, profiles=profs)
#'   class(o) <- c("int_profile_stack")
#'   o
#' }
#'
#' #' Summary for intensity optimal stack
#' #' @export
#' summary.int_profile_stack <- function(x, ...) {
#'   p <- x$profiles
#'   df <- data.frame(type = names(p),
#'     best_bw = sapply(p, getElement, "bw_best"),
#'                      on_edge=sapply(p, getElement, "edge") , row.names = NULL)
#'   df
#' }
#'
#'
#' #' Intensity Estimate for Ppp List
#' #'
#' #' @param xl list of ppp-objects
#' #' @param sigmas vector of bandwidths, as in density.ppp (sd of kernel)
#' #' @param ... further arguments passed on to density.ppp
#' #'
#' #' @details
#' #' Wrapper for computing intensity estimates for a list of point patterns. Note that this is like
#' #' \code{density.ppp} in spatstat except each pattern is assigned own bandwidth.
#' #'
#' #' @export
#'
#' density_ppplist <- function(xl, sigmas) {
#'   n <- length(xl)
#'   out <- lapply(1:n, function(i){
#'     density.ppp(xl[[i]], sigma = sigmas[i])
#'   })
#'   nn <- names(xl)
#'   if(is.null(nn)) nn <- names(sigmas)
#'   if(!is.null(nn)) names(out) <- nn
#'   as.solist(out)
#' }
