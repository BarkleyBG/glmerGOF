
createEstimatingFunction <- function(data_list, gradient_derivative_method){

  # I don't know if this is necessary in newer versions of R
  force(data_list)

  function_to_return <- function(theta){

    I_model_vec <- derivLogLike(
      model_parameters = theta,
      model_DV_vec =  data_list$model_DV_vec,
      model_matrix = data_list$model_matrix,
      gradient_derivative_method = gradient_derivative_method
    )
  }
}

derivLogLike <- function(
  model_parameters,
  model_DV_vec,
  model_matrix,
  gradient_derivative_method
){
  deriv <- numDeriv::grad(
    func = modelLoglihood,
    x = model_parameters,
    method = gradient_derivative_method,
    model_matrix = model_matrix,
    model_DV_vec = model_DV_vec
  )
}

modelLoglihood <- function( x, model_matrix, model_DV_vec ){

  ranef_std_dev <- x[length(x)]
  fixefs <- x[-length(x)]

  model_likelihood <- integrateIntegrand(
    fixefs = fixefs,
    ranef_std_dev = ranef_std_dev,
    model_matrix = model_matrix,
    model_DV_vec = model_DV_vec
  )

  log(model_likelihood)

}

integrateIntegrand <- function(
  fixefs,
  ranef_std_dev,
  model_matrix,
  model_DV_vec
){

  ### defensive programming
  if (length(fixefs) != ncol(model_matrix)) {stop(
    "number of columns in model_matrix must equal number of fixed effects"
  )}
  if (length(ranef_std_dev) != 1) {stop(
    "ranef_std_dev must be of length 1"
  )}
  if (!is.matrix(model_matrix)) {stop(
    "model_matrix must be a matrix"
  )}
  if (!all(model_DV_vec %in% 0:1)) {
    stop('supplied model_DV_vec is not binary')
  }
  ### defensive programming

  integral <- try(cubature::adaptIntegrate(
    f = modelIntegrand,
    lowerLimit = -5*ranef_std_dev,
    upperLimit = 5*ranef_std_dev,
    fixefs = fixefs,
    ranef_std_dev = ranef_std_dev,
    model_matrix = model_matrix,
    model_DV_vec = model_DV_vec
  ))

  if("try-error" %in% class(integral)){
    return(0)
  } else if (is.na(integral$integral)){
    return(0)
  }

  integral$integral
}
modelIntegrand <- function(
  random_intercept,
  fixefs,
  ranef_std_dev,
  model_matrix,
  model_DV_vec
){

  lin_pred <- (model_matrix %*% fixefs) + random_intercept
  eta <- stats::plogis(lin_pred)

  eta_bern <- (eta)^(model_DV_vec)*(1-(eta))^(1-model_DV_vec)


  integrand <- prod(eta_bern)*
    stats::dnorm(random_intercept, mean=0, sd = ranef_std_dev)

}

