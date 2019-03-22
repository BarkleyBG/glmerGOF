

computeScores <- function(
  split_data
  , gradient_derivative_method
  , theta_glmm
){

  psi_list <- lapply(
    X = split_data,
    FUN = function(data_list_i) {

      # I don't know if this is necessary in newer versions of R
      force(data_list_i)

      estimating_function_i <- createEstimatingFunction(
        data_list = data_list_i,
        gradient_derivative_method = gradient_derivative_method
      ) ## returns an estimating function

      estimating_function_i
    }
  )

  B_i <- lapply(
    X = psi_list,
    FUN = function(estimating_function_i) {

      # I don't know if this is necessary in newer versions of R
      force(estimating_function_i)

      ## calculate the value of estimating_function_i(theta = theta_glmm)
      estimating_function_i_output <- do.call(
        estimating_function_i,
        args = list(theta = theta_glmm)
      )

      estimating_function_i_output
    }
  )

  B_i
}
