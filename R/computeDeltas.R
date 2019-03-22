
#' Computes Major Model Components Necessary for GOF Test
#'
#' This function takes the important estimated model components and returns
#' the two necessary arguments for \code{computeD}.
#'
#'
#' @param theta_glmm estimated glmer coefficients
#' @param sigma_glmm estimated variance w/r/t glmer model
#' @param idx_glmm indices of specific glmer coefficients that vary between
#' clusters
#' @param theta_clogit estimated clogit coefficients
#' @param sigma_clogit estimated variance w/r/t clogit model
#' @param idx_clogit indices of clogit model (generally, all of them) that line
#' up with \code{idx_glmm}.
#' @param ... additional arguments
#'
#' @examples
#'
#' library(glmerGOF)
#' set.seed(1)
#' n <- 50
#' m <- 4
#' beta <- c(2, 2)
#' id <- rep(1:n, each=m)
#' x <- rnorm(m*n)
#' b <- rep(rnorm(n), each=m)
#' y <- rbinom(m*n, 1, plogis(cbind(1, x) %*% beta + b))
#' my_data <- data.frame(y,x,id)
#'
#' variable_names <- list(DV = "y", grouping = "id")
#'
#' library(lme4)
#' fit_glmm <- lme4::glmer(
#'   formula = y ~ x + (1|id),
#'   family = "binomial",
#'   data = my_data
#' )
#' library(survival)
#' fit_clogit <- survival::clogit(
#'   formula = y ~ x + strata(id),
#'   data = my_data,
#'   method = "exact"
#' )
#'
#' TC_test <- testGOF(
#'   data = my_data,
#'   fitted_model_clogit = fit_clogit,
#'   fitted_model_glmm  = fit_glmm,
#'   var_names = variable_names
#' )
#' TC_test
#'
#' deltas <- do.call(computeDeltas, TC_test$fits)
#'
#' deltas
#' TC_test$fits[c("delta", "sigma_delta")]
#'
#' @export
computeDeltas <- function(
  theta_glmm,
  sigma_glmm,
  idx_glmm,
  theta_clogit,
  sigma_clogit,
  idx_clogit,
  ...
){

  delta <- (theta_clogit[idx_clogit] - theta_glmm[idx_glmm])*-1
  sigma_delta <- sigma_clogit[idx_clogit, idx_clogit] - sigma_glmm[idx_glmm, idx_glmm]
  list(
    delta = delta,
    sigma_delta = sigma_delta
  )
}
