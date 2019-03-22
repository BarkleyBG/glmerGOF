

#' Computes the main Goodness of Fit Test Statistic
#'
#' This function takes the results of \code{computeDeltas} as formal arguments,
#' and outputs the main test statistic introduced in Tchetgen and Coull (2006).
#'
#' @param delta the difference in estimated model coefficients
#' @param sigma_delta the difference in estimated variance matrices (with
#' respect to the variables that vary within cluster)
#'
#' @references Tchetgen, E. J., & Coull, B. A. (2006) \emph{A Diagnostic Test
#' for the Mixing Distribution in a Generalised Linear Mixed Model.} Biometrika,
#' 93(4), 1003-1010. \url{https://doi.org/10.1093/biomet/93.4.1003}
#'
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
#' deltas
#'
#' test_results <- do.call(computeD, deltas)
#' test_results
#'
#' TC_test$results
#'
#' @export
computeD <- function(delta, sigma_delta){
  t((delta)) %*% solve(sigma_delta) %*% delta
}


