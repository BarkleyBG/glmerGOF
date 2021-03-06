% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/computeD.R
\name{computeD}
\alias{computeD}
\title{Computes the main Goodness of Fit Test Statistic}
\usage{
computeD(delta, sigma_delta)
}
\arguments{
\item{delta}{the difference in estimated model coefficients}

\item{sigma_delta}{the difference in estimated variance matrices (with
respect to the variables that vary within cluster)}
}
\description{
This function takes the results of \code{computeDeltas} as formal arguments,
and outputs the main test statistic introduced in Tchetgen and Coull (2006).
}
\examples{

library(glmerGOF)
set.seed(1)
n <- 50
m <- 4
beta <- c(2, 2)
id <- rep(1:n, each=m)
x <- rnorm(m*n)
b <- rep(rnorm(n), each=m)
y <- rbinom(m*n, 1, plogis(cbind(1, x) \%*\% beta + b))
my_data <- data.frame(y,x,id)

variable_names <- list(DV = "y", grouping = "id")

library(lme4)
fit_glmm <- lme4::glmer(
  formula = y ~ x + (1|id),
  family = "binomial",
  data = my_data
)
library(survival)
fit_clogit <- survival::clogit(
  formula = y ~ x + strata(id),
  data = my_data,
  method = "exact"
)

test_results <- testGOF(
  data = my_data,
  fitted_model_clogit = fit_clogit,
  fitted_model_glmm  = fit_glmm,
  var_names = variable_names,
  gradient_derivative_method = "simple"
)
test_results

deltas <- do.call(computeDeltas, test_results$fits)
deltas

test_results_by_hand <- do.call(computeD, deltas)
test_results_by_hand

test_results$results

}
\references{
Tchetgen, E. J., & Coull, B. A. (2006) \emph{A Diagnostic Test
for the Mixing Distribution in a Generalised Linear Mixed Model.} Biometrika,
93(4), 1003-1010. \url{https://doi.org/10.1093/biomet/93.4.1003}
}
