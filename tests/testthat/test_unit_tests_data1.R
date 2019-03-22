
context("unit test with simple data")

test_that(
  "grabModelEstimates works correctly",
  {
    input_data <- readRDS(
      rprojroot::find_testthat_root_file("test_data", "grabModelEstimates_indata.Rds")
    )
    output_data <- readRDS(
      rprojroot::find_testthat_root_file("test_data", "grabModelEstimates_outdata.Rds")
    )
    colnames(output_data$sigma_clogit) <-
      rownames(output_data$sigma_clogit) <-
      names(output_data$theta_clogit)

    output <- do.call(
      grabModelEstimates,
      input_data[c("fitted_model_clogit", "fitted_model_glmm")]
    )


    expect_equal(
      output,
      output_data,
      tolereance = 1e-7
    )
  }
)


test_that(
  "splitData works correctly",
  {
    input_data <- readRDS(
      rprojroot::find_testthat_root_file("test_data", "splitData_data.Rds")
    )

    output_to_compare <- input_data$split_data

    output <- do.call(splitData, input_data[names(input_data) != "split_data"])

    expect_equal(
      output,
      output_to_compare$splitdt,
      tol=1e-7
    )
  }
)


test_that(
  "computeScores works correctly",
  {
    input_data <- readRDS(
      rprojroot::find_testthat_root_file("test_data", "computeScores_data.Rds")
    )


    input_args1 <- list(
      split_data = input_data$split_data$splitdt,
      theta_glmm = input_data$theta_glmm,
      gradient_derivative_method = "simple"
    )

    output_to_compare <- input_data$score_vecs_list

    output <- do.call(
      computeScores,
      args = input_args1
    )

    expect_equal(
      output,
      output_to_compare,
      tol=1e-7
    )
  }
)



test_that(
  "computeVCov works correctly",
  {
    input_data <- readRDS(
      rprojroot::find_testthat_root_file("test_data", "computeScores_data.Rds")
    )

    vcov_input <- input_data$score_vecs_list


    vcov_mat <- computeVCov(score_vecs_list = vcov_input)

    vcov_reference <- structure(
      c(9.53990045261663, -3.34608218746882, -2.36081742575628,
        -3.34608218746882, 8.59191439395491, -2.74910376436904,
        -2.36081742575628, -2.74910376436904, 5.74981673583168),
      .Dim = c(3L, 3L)
    )

    expect_equal(
      vcov_mat,
      vcov_reference,
      tol=1e-7
    )
  }
)



