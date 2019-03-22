
splitData <- function(
  fitted_model_glmm
  , data
  , var_names
){

  model_matrix <- lme4::getME(fitted_model_glmm, "X")
  data_smaller <- data[as.numeric(row.names(model_matrix)), ]

  split_dfm_list <- by(
    data_smaller, data_smaller[[var_names$grouping]],
    function(group_data){
      model_DV_vec <- group_data[[var_names$DV]]
      grouping_vec <- group_data[[var_names$grouping]]
      cluster_name <- grouping_vec[1]
      out_list <- list(
        model_DV_vec = model_DV_vec,
        cluster_id_vec = grouping_vec,
        cluster_name = cluster_name
      )
      out_list
    }
  )

  split_model_matrix <- by(
    model_matrix, data_smaller[[var_names$grouping]], function(x){as.matrix(x)}
  )

  split_data_list <- mapply(
    FUN = function(model_mat, data_list){
      append(data_list, list(model_matrix=model_mat ))
    },
    model_mat = split_model_matrix,
    data_list = split_dfm_list,
    SIMPLIFY = FALSE
  )

  split_data_list
}
