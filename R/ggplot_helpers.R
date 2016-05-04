
#' Helper geom for the Jandhala case study
#'
#' A specific ggplot2 geom with the structure of the units, the border and the
#' observations.
#'
#' It keeps equal coordinates and removes axis labels.
#' @export
geom_india <- function()
  list(geom_polygon(aes(long, lat, group = group),
                    data = walls),
       geom_point(aes(x, y), data = Jandhala, alpha = 0.5, size = 1),
       coord_equal(),
       labs(x = NULL, y = NULL))




#' Plotting empirical variograms with ggplot
#'
#' Defines a \code{variogram} method for \code{ggplot2}, for plotting empirical
#' variograms.
#'
#' @param data a variogram object, as created for example with \code{variog}
#' @param ... not used
#' @import ggplot2
#' @export
ggplot.variogram <- function(data, ...) {
  idx <- match(c('u', 'v', 'n'), names(data))
  xdf <- as.data.frame(data[idx])
  names(xdf) <- c('distance', 'semivariance', 'n')
  ggplot(xdf, aes(distance, semivariance)) +
    geom_point(aes(size = n)) +
    expand_limits(y=0)
}

#' Helper geom for variogram objects
#'
#' Adds a \code{ggplot2} layer with a line plot of a variogram model.
#'
#' @param x a \code{variomodel} object
#' @param max.dist miximum distance (x-axis) to compute and draw the variogram
#' @param scaled logical. If TRUE the total sill in the plot equals 1
#' @param ... not used.
#'
#' This function is an adaptation to \code{ggplot2} of the original function
#' \code{lines.variomodel} in \code{geoR}.
#'
#' @examples
#' ## Computing and plotting empirical variogram
#' library(geoRcb)
#' library(ggplot2)
#' vg <- variog(s100, max.dist = 1)
#' ggplot(vg)
#'
#' ## Estimating parameters by wheighted least squares
#' vgmdl <- variofit(vg, ini = c(1, .3), fix.nugget = TRUE)
#'
#' ## Adding fitted model to the empirical variogram
#' ggplot(vg) + geom_variogram(vgmdl)
#' @export
geom_variogram <- function(x, max.dist, scaled = FALSE, ...) {
  my.l <- list()
  if (missing(max.dist)) {
    my.l$max.dist <- x$max.dist
    if (is.null(my.l$max.dist))
      stop("argument max.dist needed for this object")
  }
  else my.l$max.dist <- max.dist
  if (any(x$cov.model == c("matern", "powered.exponential",
                           "cauchy", "gencauchy", "gneiting.matern")))
    my.l$kappa <- x$kappa
  else kappa <- NULL
  if (is.vector(x$cov.pars))
    my.l$sill.total <- x$nugget + x$cov.pars[1]
  else my.l$sill.total <- x$nugget + sum(x$cov.pars[, 1])
  my.l$nugget <- x$nugget
  my.l$cov.pars <- x$cov.pars
  my.l$cov.model <- x$cov.model
  if (scaled) {
    if (is.vector(x$cov.model))
      my.l$cov.pars[1] <- my.l$cov.pars[1]/my.l$sill.total
    else my.l$cov.pars[, 1] <- my.l$cov.cov.pars[, 1]/my.l$sill.total
    my.l$sill.total <- 1
  }
  gamma.f <- function(x, my.l) {
    if (any(my.l$cov.model == c("linear", "power")))
      return(my.l$nugget + my.l$cov.pars[1] * (x^my.l$cov.pars[2]))
    else return(my.l$sill.total - cov.spatial(x, cov.model = my.l$cov.model,
                                              kappa = my.l$kappa, cov.pars = my.l$cov.pars))
  }
  dat <- transform(data.frame(x = seq(0, my.l$max.dist, length = 101)),
                   y = gamma.f(x, my.l = my.l), ...)
  geom_line(aes(x, y), data = dat)
}


