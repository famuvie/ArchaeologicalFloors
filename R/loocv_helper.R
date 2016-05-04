#' Leave-one-out Cross Validation Kriging
#'
#' Implements a Leave-one-out Cross Validation (LOOCV) procedure with either
#' Euclidean (\code{std}) or Cost-based (\code{cst}) kriging.
#'
#' @param method string. Either \code{std} or \code{cst} for \emph{standard} Euclidean kriging or \emph{cost}-based kriging respectively.
#' @param geodata a geodata object
#' @param ddm   cost-based distance matrices as in the output of \code{distmatGen}.
#'
#' @export
krige.loocv <- function(
  method = c('std', 'cst'),
  geodata,
  ddm) {

  method <- match.arg(method)

  ## Fit an exponential variogram model
  ## and return a kriging prediction on the given loc
  fit.single <- function(geodata, loc, ddm, method) {

    ## fit variogram
    likfit_args <- list(geodata = geodata,
                        data = geodata$data,
                        fix.nugget = FALSE,
                        fix.kappa = FALSE,
                        ini = c(10, 5),  # sigma^2 (partial sill) and phi (range parameter)
                        cov.model = "exponential",
                        lik.method = "REML")

    if (method == 'cst') {
      likfit_args$dists.mat <-  ddm$obs
    }

    vgmdl <- do.call('likfit', likfit_args)

    ## kriging
    KC = krige.control(obj.model = vgmdl)
    kriging_args <- list(geodata,
                         data = geodata$data,
                         locations = loc,
                         krige = KC)

    if (method == 'cst') {
      kriging_args$dd.dists.mat = ddm$obs
      kriging_args$dl.dists.mat = ddm$loc
    }

    kriging <- do.call('krige.conv', kriging_args)

    ## Return prediction in observation locations
    kriging$predict
  }

  ## Leave observation i out, fit without
  ## and return predicted value
  pred.loo <- function(i, geodata, ddm, method) {

    ## Prediction location
    loc_i <- geodata$coords[i, , drop = FALSE]

    ## Remove observation from data
    dat <- geodata
    dat$coords <- dat$coords[-i, ]
    dat$data   <- dat$data[-i]

    ## Modify the distance matrices
    d_obs <- ddm$obs[-i, -i]
    d_loc <- ddm$obs[i, -i, drop = FALSE]

    ## Predict silently
    sink('/dev/null')
    ans <- suppressMessages(
      fit.single(dat, loc_i, list(obs = d_obs, loc = d_loc), method)
    )
    sink()

    return(unname(ans))
  }

  # debug:
  #   geodata <- obs.gd
  #   geodata$data <- obs.gd$data[, 'Ca']
  #   res <- fit.single(geodata, loc, ddm, 'std')
  #   qplot(kriging.std.Ca$predict, res) + geom_abline(int=0, sl=1)
  #   res <- fit.single(geodata, loc, ddm, 'cst')
  #   qplot(kriging.cst.Ca$predict, res) + geom_abline(int=0, sl=1)

  N <- length(geodata$data)

  res <- sapply(seq_len(N), pred.loo, geodata, ddm, method)

  return(res)
}

