
#' Test Goodness Of Fit of Gaussian Random Effect Distribution in Logistic
#' Mixed Model
#'
#' This function implements the GOF test introduced Tchetgen & Coull (2006)
#' Biometrika. The test is designed to determine whether the presumed Gaussian
#' distribution of the random effect in a logsitic mixed model has adequate fit.
#' A major contribution of the glmerGOF package is to estimate the variance
#' matrix from a \code{\link[lme4]{glmer}} model for computing the chi-squared
#' statistic introduced in Tchetgen & Coull (2006).
#'
#'
#' @param data The original dataset.
#' @param fitted_model_clogit A fitted conditional logistic model object, from
#' \code{\link[survival]{clogit}}.
#' @param fitted_model_glmm  A fitted generalized linear (logistic) mixed model
#' object, from \code{\link[lme4]{glmer}}.
#' @param var_names A list indicating the column names (as strings) of two
#' important variables
#' @param gradient_derivative_method User must select either 'simple' (faster)
#' or 'Richardson' (slower but more accurate) as the desired method for the
#' numerical derivative procedure. For more information, see the \code{method}
#' argument in \code{\link[numDeriv]{grad}}.
#' @param use_Richardson_derivative defaults to FALSE
#'
#' @return An object of class \code{"TCGOF"}, with the following list items:
#' \describe{
#'   \item{results}{The test statistic and p-value. See the \code{print()}
#'   generic.}
#'   \item{var_names}{The list of \code{var_names}.}
#'   \item{split_data}{A list object of the data after it has been split into
#'   clusters and NA rows have been removed.}
#'   \item{fits}{Estimated components from the two models, and other important
#'   computations. See the \code{summary()} generic.}
#' }
#'
#' @details The method is based on an asymptotically chi-squared test statistic
#' comparing the difference between estimated components of a generalized linear
#' mixed model (\emph{glmm}) and a conditional logistic model (\emph{clogit}).
#' Before using this function, the user should fit his/her desired glmm with
#' \code{\link[lme4]{glmer}}, and a clogit model with
#' \code{\link[survival]{clogit}}. The fitted model objects are required
#' arguments, and their estimated components are wrangled in
#' \code{\link{grabModelEstimates}}.
#'
#' @references Tchetgen, E. J., & Coull, B. A. (2006) \emph{A Diagnostic Test for the Mixing
#' Distribution in a Generalised Linear Mixed Model.} Biometrika, 93(4),
#' 1003-1010. \url{https://doi.org/10.1093/biomet/93.4.1003}
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
#'
#' @export
testGOF <- function(
  data
  , fitted_model_clogit
  , fitted_model_glmm
  , var_names = list(DV = NULL, grouping = NULL)
  , gradient_derivative_method = NULL
  , use_Richardson_derivative = NULL
){
  all_output <- list(
    results = NULL,
    var_names = var_names
  )


  if (is.null(gradient_derivative_method)){
    stop("Please choose a method for the derivative, either 'simple' or 'Richardson'. see the 'method' argument in numDeriv::grad()")
  } else
    if (!(gradient_derivative_method %in% c("simple", "Richardson"))){
      stop("Please choose a method for the derivative, either 'simple' or 'Richardson'. see the 'method' argument in numDeriv::grad()")
    }
  if (!all(c("clogit", "coxph" ) %in% class(fitted_model_clogit))) {
    stop("fitted_model_clogit should be a fitted survival::clogit() model object")
  }
  if (!all("glmerMod" %in% class(fitted_model_glmm))) {
    stop("fitted_model_glmm should be a fitted lme4::glmer() model object")
  }
  if (!all(unlist(var_names) %in% colnames(data))){
    stop("var_names must exist in dataset")
  }
  if (!all(c("DV","grouping") == sort(names(var_names)) )){
    stop("var_names must be a list with elements named 'DV' and 'grouping'")
  }



  if (!is.null(use_Richardson_derivative)){
    stop("remove richardson option for other arg")
  }


  model_fits_list <- list(
    fitted_model_clogit = fitted_model_clogit
    , fitted_model_glmm = fitted_model_glmm
  )

  estimate_list <- do.call(grabModelEstimates, model_fits_list)

  theta_glmm <- estimate_list$theta_glmm
  idx_glmm <- estimate_list$align$idx_glmm
  theta_clogit <- estimate_list$theta_clogit
  sigma_clogit <- estimate_list$sigma_clogit

  idx_clogit <- 1:length(theta_clogit)
  chi_sq_df <- length(idx_glmm)


  split_data <- splitData(
    fitted_model_glmm = fitted_model_glmm
    , data = data
    , var_names = var_names
  )
  all_output$split_data <- split_data

  num_clusters <- length(split_data)

  ## This is the computationally intensive step
  score_vecs_list <- computeScores(
    split_data = split_data
    , gradient_derivative_method = gradient_derivative_method
    , theta_glmm = theta_glmm
  )

  vcov_mat <- computeVCov(score_vecs_list = score_vecs_list)

  rownames(vcov_mat) <- colnames(vcov_mat) <- names(theta_glmm)
  sigma_glmm <- solve(vcov_mat)

  args_for_deltas <- list(
    theta_glmm = theta_glmm,
    sigma_glmm = sigma_glmm,
    idx_glmm = idx_glmm,
    theta_clogit = theta_clogit,
    sigma_clogit = sigma_clogit,
    idx_clogit = idx_clogit
  )

  delta_comps <- do.call(computeDeltas, args_for_deltas)


  all_output$fits <- c(
    args_for_deltas,
    list(
      num_clusters = num_clusters,
      delta = delta_comps$delta,
      sigma_delta = delta_comps$sigma_delta
    )
  )


  D_Stat <- as.numeric(do.call(computeD, delta_comps))
  p_val <- 1-stats::pchisq(D_Stat, chi_sq_df)

  all_output$results <- list(
    D = D_Stat,
    p_value = p_val,
    df = chi_sq_df
  )

  class(all_output) <- "TCGOF"
  all_output
}
