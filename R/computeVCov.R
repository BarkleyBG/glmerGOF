computeVCov <- function(
  score_vecs_list
){
  Meat_mat <- matrix(0, ncol = length(score_vecs_list[[1]]), nrow = length(score_vecs_list[[1]]))

  N_tot <- 0
  for (ii in 1:length(score_vecs_list)){
    outer_psi <- score_vecs_list[[ii]] %*% t(score_vecs_list[[ii]])
    if (!any(is.nan(outer_psi))) {
      N_tot <- N_tot + 1
      Meat_mat <- Meat_mat+outer_psi
    }
  }
  Meat_mat
}
