

#' Summarizes a "TCGOF" Object
#'
#' @param object object of class "TCGOF"
#' @param ... dots
#'
#' @method summary TCGOF
#'
#' @author Brian G. Barkley
#'
#' @return \describe{
#'   \item{results}{The results of the test}
#'   \item{test_components}{Inputs for \code{\link{computeD}}.}
#'   \item{glmm_components}{Components from the glmm model.}
#'   \item{clogit_components}{Components from the clogit model.}
#'   \item{prepared_data}{Modeled data after rows with \code{NA}s have been
#'   removed.}
#'   \item{var_names}{Important variable names.}
#' }
#'
#' @export
summary.TCGOF <- function(object, ...){


  prepared_data = list(
    model_DV = unlist(lapply(object$split_data, function(x) x$model_DV_vec)),
    cluster_id = unlist(lapply(object$split_data, function(x) x$cluster_id_vec)),
    model_matrix = do.call(rbind, lapply(object$split_data, function(x) x$model_matrix))
  )

  modeled_data <- do.call(cbind,prepared_data)

  ## glmer info

  theta_glmm <- object$fits$theta_glmm
  sigma_glmm <- object$fits$sigma_glmm
  theta_clogit <- object$fits$theta_clogit
  sigma_clogit <- object$fits$sigma_clogit
  delta <- object$fits$delta
  sigma_delta <- object$fits$sigma_delta

#
#
#   ## cluster size info
#
#   cluster_size_fivenum <- round(stats::fivenum(
#     unlist(lapply(
#       X = object$split_data,
#       FUN = function(x){ NROW(x$model_matrix) }
#     ))
#   ), digits =1)
#   names_cluster_size_fivenum <- c("min", "q1", "med", "q3", "max")
#   names(names_cluster_size_fivenum) <- names_cluster_size_fivenum
#   cluster_size_summary <- data.frame(
#     stat = names_cluster_size_fivenum,
#     value = cluster_size_fivenum,
#     stringsAsFactors = FALSE,
#     row.names = NULL
#   )


  output <- list(
    results = object$results,
    test_components = list(
      delta = delta,
      sigma_delta = sigma_delta
      # df = object$fits$chi_sq_df
    ),
    glmm_components = list(
      theta = theta_glmm,
      sigma = sigma_glmm,
      matched_indices = object$fits$idx_glmm
    ),
    clogit_components = list(
      theta = theta_clogit,
      sigma = sigma_clogit
    ),
    prepared_data = modeled_data,
    var_names = object$var_names
  )

  class(output) <- "summary_TCGOF"
  output
}

