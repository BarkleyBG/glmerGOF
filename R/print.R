

#' Prints a "TCGOF" object
#'
#' @param x object of class "TCGOF"
#' @param ... dots
#'
#' @method print TCGOF
#'
#' @author Brian G. Barkley
#'
#' @export
print.TCGOF <- function(x, ...){

  results <- x$results
  # df <- x$fits$chi_sq_df
  num_digits <- 3

  output_message1 <- paste0(
    "The test statistic is D=", round(results$D, digits = num_digits)
  )
  output_message2 <- paste0(
    "D is Chi-Squared with df=", results$df
    # " degrees of freedom."
  )
  output_message3 <- paste0(
    "The resulting p-value is p=", round(results$p_value, digits = num_digits)
  )

  cat(
    "--- Results of GOF Test ---", "\n",
    output_message1, '\n',
    output_message2, '\n',
    output_message3,
    sep = ""
  )
}



#' Prints a "summary_TCGOF" object
#'
#' @param x object of class "summary_TCGOF"
#' @param ... dots
#'
#' @method print summary_TCGOF
#'
#' @author Brian G. Barkley
#'
#' @export
print.summary_TCGOF <- function(x, ...){

  coefs_glmm <- cbind(
    Est = x$glmm_components$theta,
    SD  = sqrt(diag(x$glmm_components$sigma))
  )
  coefs_clogit <- cbind(
    Est = x$clogit_components$theta,
    SD  = sqrt(diag(x$clogit_components$sigma))
  )

  # print(x)
  cat("\n", sep = "")
  cat("------- Estimates, GOF test -------\n", sep = "")
  cat("-- delta: --", "\n", sep = "")
  print(round(x$test_components$delta, digits = 2))
  cat("-- sigma_delta: --", "\n", sep = "")
  print(round(x$test_components$sigma_delta, digits = 2))
  cat("------- Estimates, glmer -------\n", sep = "")
  print(round(coefs_glmm, digits = 2))
  cat("------- Estimates, clogit -------\n", sep = "")
  print(round(coefs_clogit, digits = 2))
  # cat("------- Data summary, after dropping NA rows -------\n", sep = "")
  cat("------- Names and indices -------\n", sep = "")
  cat("-- Important variables: --", "\n", sep = "")
  # browser()
  print(unlist(x$var_names))
  cat("-- Matched glmer indices: --", "\n", sep = "")
  print(unlist(x$glmm_components$matched_indices))
  # cat("-- Number of clusters: --", "\n", sep = "")
  # cat(x$fits$num_clusters, "\n", sep = "")
  # cat("-- Summary of cluster size: --", "\n", sep = "")
  # print(cluster_size_summary, row.names = FALSE)

}
