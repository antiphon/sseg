#' Mingling Index
#'
#' Compute the ratio of own type and all type neighbours
#'
#' @param x multitype point pattern
#' @param i optional target type. If omitted, compute for all.
#' @param r neighbourhood radius / other parameter
#' @param normalise if TRUE, divide by CSR value 1 - lam[i]/lam.
#' @param correction "border" or "none" (defaut)
#' @param version 1 or 2
#' @param ntype "geometric" or "knn"
#' @details correction = "border" is minus-sampling. Version 1 is 1 - E(same type)/E(any type),
#'  version 2 is 1 - E [ (same type)/(any type) ]
#' @export

mingling_index <- function(x, i, r, normalise = FALSE,
                           correction = "border",
                           version = 1, ntype = "geometric") {
  m <- parse_marks(x)
  lam <- intensity(x)
  coord <- get_coords(x)
  bbox <- get_bbox(x)
  mi <- as.integer(m)
  marknames <- levels(m)
  nm <- length(unique(mi))
  #
  # type of neighbourhood
  ntypei <- pmatch(ntype, c("geometric", "knn"))
  if(!ntypei %in% 1:2 ) stop("Given 'ntype' not supported.")
  # check parameter
  if(missing(r)){
    rmax <- rmax.rule("K", x$window, intensity(x))
    r <- seq(0, rmax, length = 50)
    if(ntypei == 2) r <- 1:round(pi * mean(lam) * max(r^2))
  }
  # check i
  all <- missing(i)
  if(all) {
    i <- 1:nm
  }
  if(is.numeric(i)) i <- marknames[i]
  ii <- match(i, marknames)
  if(any(is.na(ii))) stop("'i' not understood.")
  if(length(ii)>1) {ii <- ii[1]; warning("Using only the first element of 'i'")}
  target <- marknames[ii]

  # edge correction
  corr <- pmatch(correction, c("none", "border"))
  bdist <- if(corr==2) bdist.points(x) else rep(max(r)+1, length(m))


  # compute
  mingling_index_c <- if(version == 1) mingling_index_c_1st else mingling_index_c_2nd
  out <- mingling_index_c(coord, mi, nm, bbox, bdist, r, ii, ntypei)

  #browser()
  if(normalise) {
    norm <- 1 - lam[ii]/sum(lam)
    out <- ( apply(out, 1, function(v) v/norm) )
  }
  # gather
  df <- data.frame(r=r)
  df[target] <- out
  fn <- target
  if(length(ii) > 1){ # add mean
    df[fn <- "mean"] <- rowMeans(out)
    target <- c(target, fn)
  }

  fv(df, valu=fn, fmla = formula(.~r),
     ylab ="Mingling(r)",
     desc = c("range", paste("Mingling Index for", target)))

}
