

#' Wrangle important information from fitted models
#'
#' This function selects the estimated coefficients from each model, and the
#' estimated variance component from the clogit model object. It then attempts
#' to match the coefficients, if possible, and throws errors if not.
#'
#' @inheritParams testGOF
#'
#' @details It is important to use the same coefficient names in the model
#' formulas for both models. It is required that the coefficients names in the
#' clogit model are a subset of those from the glmm. Future versions may have
#' improved matching functionality without this requirement.
#'
#' @return A list including the estimated components from the
#' \code{\link[survival]{clogit}} model object, the estimated coefficients from
#' the \code{\link[lme4]{glmer}} model object, and a vector of indices to match
#' the coefficient names between the models.
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
#' model_ests <- grabModelEstimates(
#'   fitted_model_clogit = fit_clogit,
#'   fitted_model_glmm = fit_glmm
#' )
#' names(model_ests)
#'
#' @export
grabModelEstimates <- function(
  fitted_model_clogit
  , fitted_model_glmm
){

  fixef_glmm <- lme4::getME(fitted_model_glmm, c("fixef"))
  raneff_glmm <- lme4::getME(fitted_model_glmm, c('theta'))
  theta_glmm <- c(fixef_glmm, raneff_glmm)


  theta_clogit <- stats::coef(fitted_model_clogit)
  sigma_clogit <- fitted_model_clogit$var
  colnames(sigma_clogit) <- rownames(sigma_clogit) <- names(theta_clogit)

  out_list <- list(
    theta_glmm = theta_glmm,
    theta_clogit = theta_clogit,
    sigma_clogit = sigma_clogit
  )

  out_list$align <- alignCoefficients(theta_clogit, theta_glmm)

  out_list
}


alignCoefficients <- function(theta_clogit,theta_glmm){
  names_clogit <- names(theta_clogit)
  names_glmm <- names(theta_glmm)

  if (any(is.na(theta_clogit))){
    stop("no fitted coefficients in clogit should be NA")
  }
  if (!all(names_clogit %in% names_glmm)){
    stop("All fitted coefficients from clogit model should also be fitted coefficients in glmm model")
  }

  which_glmm_in_clogit <- which(names_glmm %in% names_clogit)
  subnames_glmm <- names_glmm[which_glmm_in_clogit]
  idx_glmm <- match(names_clogit,names_glmm)

  if (!all(names_clogit == names_glmm[idx_glmm])){
    stop('names do not align')
    #TODO: setup unit test
  }

  list(
    idx_glmm = idx_glmm,
    names_clogit = names_clogit,
    names_glmm = names_glmm
  )
}

