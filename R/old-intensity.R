# # Estimate the intensity at points using optimal bandwidth and within a type
# #
# # @param x point pattern, ppp object
# # @param bwv Vector of bandwidths to optimise over
# # @param verb verbosity
# # @param ... passed on to \code{\link{intensity_bandwidth_profile}}
# # @details
# # First, we split the pattern per type. Then we optimise the
# # intensity using \code{\link{intensity_bandwidth_profile}}. Then
# # we concatenate the estimates to a vector.
# #
# # Caution: For a big pattern with many types this will take time.
# #
# # @return
# # A list with: a vector of intensities, one per point; the profiles; the best bandwidths.
# #
# # @import spatstat
# # @export
# intensity_optimal <- function(x, verb=FALSE, ...){
#   x <- check_pp(x)
#   if(!is.marked(x)) x <- setmarks(x, factor(1))
#   m <- levels(x$marks)
#   int <- rep(NA, x$n)
#   cat2 <- if(verb) cat else function(...) NULL
#   obw <- NULL
#   errs <- NULL
#   cat2(length(m), "types: ")
#   profs <- list()
#   for(k in seq_along(m)) {
#     i <- which( x$marks == m[k] )
#     xi <- x[i]
#     pf <- intensity_bandwidth_profile(x=xi, ...)
#     int[i] <- pf$intensity
#     pf$intensity <- NULL
#     profs[[m[k]]] <- pf
#     bw <- pf$profile$bw
#     errs <- cbind(errs, err1 <- pf$profile$loss)
#     # check
#     bi <- which.min(err1)
#     if( pf$edge ) warning(paste0(m[k], ": optimum at edge of bandwith grid"))
#     cat2(k, "")
#     obw[k] <- pf$bw_best
#   }
#   colnames(errs) <- names(obw) <- m
#   names(profs) <- m
#   cat2("\n")
#   #
#   o <- list(intensity=int, bandwidth=obw, profiles=profs)
#   class(o) <- c("int_profile_stack")
#   o
# }
#
# # Summary for intensity optimal stack
# # @export
# summary.int_profile_stack <- function(x, ...) {
#   p <- x$profiles
#   df <- data.frame(type = names(p),
#     best_bw = sapply(p, getElement, "bw_best"),
#                      on_edge=sapply(p, getElement, "edge") , row.names = NULL)
#   df
# }
#
# # Compute intensity surface for each component of multitype pattern
# #
# # @param x a multitype pattern list
# # @param bw the bandwidths, vector.
# # @param ... passed on to density.ppp
# #
# # @details
# # Uses density.ppp. Does what density.ppplist does but this one can take different bandwidths
# # for different types.
# #
# # @export
# density_ppplist <- function(x, sigma, ...) {
#   if(!is.list(x)) x <- split(x)
#   n <- length(x)
#   if(length(sigma)!=n) stop("sigma should be a vector giving ", n, " bandwidths.")
#   y <- lapply(1:n, function(i) density(x[[i]], sigma[i], ...) )
#   names(y) <- names(x)
#   as.solist(y, demote = TRUE)
# }
#
#
#
# # Print
# print.int_profile_stack <- function(x, ...){
#   print( s <- summary(x, ...) )
#   invisible(s)
# }
#
# # plot intensity optimisation
# #
# # @param x output of intensity_optimal
# # @param i which types. Default is the first.
# # @param ... passed on to plot.intensity_profile
# #
# # @export
# plot.int_profile_stack <- function(x, i, ...){
#   p <- x$profiles
#   nam <- names(p)
#   if(missing(i)) i <- 1
#   if(is.numeric(i)) i <- nam[i]
#   for(n in i) plot(p[[n]], main=n, ...)
# }
#
# #
# # # Estimate intensity of a univariate point pattern at given locations
# # #
# # # @param x points, list with $x and $bbox or just a matrix of coordinates.
# # # @param loc Matrix of locations to estimate the intensity at
# # # @param bw bandwidth, Epanechnikov kernel domain [-bw,bw]
# # # @param b Type of border correction estimate.
# # # @param ... ignored
# # # @details
# # # Uses Epanechnikov kernel smoothing. Border corrections controlled by what b is:
# # # 0: 2D exact, not available for 3D atm;
# # # 1: Box rectangle approximation;
# # # 2: 2D Biased box integral, 3D none;
# # # >3: fine grid sum with resolution b^dim (slow).
# # #
# # # if b < 0 no edge correction is applied.
# # # @useDynLib aninISAR
# # # @import Rcpp
# # # @export
# #
# # intensity_somewhere <- function(x, loc, bw, b=0, ...) {
# #   x <- check_pp(x)
# #   bbox <- get_bbox(x)
# #   coord <- get_coords(x)
# #   if(ncol(loc)!=ncol(coord)) stop("pattern and locations of different dimension.")
# #   intensity_at_other_points_c(coord, loc, bbox, bw, b)
# # }
#
# # # Estimate intensity of point pattern at the points of the pattern
# # #
# # # @param x points, list with $x and $bbox or just a matrix of coordinates.
# # # @param bw bandwidth, Epanechnikov kernel domain [-bw,bw]
# # # @param b Type of border correction estimate.
# # # @param ... ignored
# # # @details
# # # Uses Epanechnikov kernel smoothing. Border corrections controlled by what b is:
# # # 0: 2D exact, not available for 3D atm;
# # # 1: Box rectangle approximation;
# # # 2: 2D Biased box integral, 3D none;
# # # >3: fine grid sum with resolution b^dim (slow).
# # #
# # # if b < 0 no edge correction is applied.
# # #
# # # @useDynLib aninISAR
# # # @import Rcpp
# # # @export
# #
# # intensity_at_points <- function(x, bw, b=0, ...) {
# #   x <- check_pp(x)
# #   bbox <- get_bbox(x)
# #   coord <- get_coords(x)
# #   intensity_at_points_c(coord, bbox, bw, b)
# # }
#
#
# # # Intensity on a grid
# # #
# # # @param x a point pattern, ppp
# # # @param bw Epanechnikov bandwidth
# # # @param nx Grid resolution in x-dimension
# # # @param ny Grid resolution in y-dimension
# # # @param b border correction, see \code{\link{intensity_somewhere}}
# # # @return
# # # An spatstat im-object
# # # @import spatstat
# # # @export
# #
# # intensity_im <- function(x, bw, nx=50, ny, b=0) {
# #   x <- check_pp(x)
# #   bb <- get_bbox(x)
# #   if(missing(ny)){
# #     el <- apply(bb, 2, diff)
# #     asp <- el[2]/el[1]
# #     ny <- round(nx * asp)
# #   }
# #   gx <- seq(bb[1,1], bb[2,1], length = nx)
# #   gy <- seq(bb[1,2], bb[2,2], length = ny)
# #   loc <- as.matrix( expand.grid(gx, gy) )
# #   M <- t( matrix( intensity_somewhere(x, loc, bw, b), nrow=nx) )
# #   im(M, gx, gy)
# # }
# #
#
# # Find the optimal smoothing for intensity estimation kernel width
# #
# # @param x point pattern
# # @param bw_vector Vector of bandwidth values to optimize over
# # @param scale Scale before computing? Recommended for numerical stability.
# # @param leaveoneout default FALSE
# # @param ... passed on to density.ppp
# # @details
# #
# # Optimize bandwidth $h$ of the Gaussian kernel using loss function
# # \deqn{latex}{\sum 1/\hat\lambda_h(x_i) - |W|)^2}
# #
# # The bandwidth corresponds to the standard deviation of the Gaussian density.
# #
# # The pattern will be scale to approx 1 unit window for numerical stability.
# # @return
# #
# # Vector of losses, or if keep_min=TRUE, list with a vector of losses and
# # a vector of intensity values with the bw that gives the minimum loss.
# #
# # @export
#
# intensity_bandwidth_profile <- function(x, bw_vector, scale=TRUE, leaveoneout = FALSE, ...){
#   x <- check_pp(x)
#
#   if(missing(bw_vector)){
#     el <- max(diff(x$window$yrange), diff(x$window$xrange))
#     bw_vector <- exp( seq(log(el*0.1), log(5*el), length=100 / ifelse(x$n < 10000, 1, 2)) )
#   }
#   V <- area(x$window)
#   err <- Inf
#   S <- if(scale) sqrt(1/V) else 1
#   y <- affine(x, diag(S, 2))
#   W <- area(y$window)
#
#   for(bw in bw_vector) {
#       o <- density.ppp(y, sigma = bw*S, at = "points", leaveoneout = leaveoneout, ...)
#       #o <- intensity_at_points(x = y, bw = bw * S, b = b)
#       # discrepancy
#       err1 <- (sum(1/o)-W)^2
#       if(err1 <= min(err)) i <- o * S^2
#       err <- c(err, err1)
#   }
#   err <- err[-1]
#   bi <- which.min(err)
#   o <- list(profile=data.frame(bw=bw_vector, loss = err),
#        bw_best=bw_vector[bi],
#        intensity_best=i, edge=bi%in%c(1, length(bw_vector)))
#   class(o) <- c("int_profile", is(o))
#   o
#  }
#
#
#
# # Print method for int_profile
# #
# # @export
# print.int_profile <- function(x,...) {
#   cat("Profile for optimising kernel density bandwidth for intensity\n")
#   cat("* Gaussian kernel\n")
#   cat("* best bw:", x$bw_best)
#   cat("\n* Search grid range: [", paste(range(x$profile$bw), collapse = ","), "]", sep="")
#   cat("\n")
#   if(x$edge) cat("Note: Optimum at the border.\n")
# }
#
# # Plot int_profile
# #
# # @export
# plot.int_profile <- function(x,...) {
#   with(x$profile, plot(bw, loss, ...) )
# }
#
