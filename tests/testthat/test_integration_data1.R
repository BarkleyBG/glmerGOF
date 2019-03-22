
context("simple test dataset")

test_that(
  "integration test for basic dataset",
  {


    # Sample dataset of 30 clusters
    ### This data is similar to
    ### https://bsaul.github.io/geex/articles/articles/lme4_discussion.html#setup
    data1 <- readRDS(
      rprojroot::find_testthat_root_file("test_data", "glmerGOF_indata1.Rds")
    )

    library(lme4)
    library(survival)

    fit_glmm <- lme4::glmer(
      formula = y ~ x + (1|id),
      family = "binomial",
      data = data1
    )
    fit_clogit <- survival::clogit(
      formula = y ~ x + strata(id),
      data = data1,
      method = "exact"
    )

    TC_stat <- testGOF(
      data = data1,
      fitted_model_clogit = fit_clogit,
      fitted_model_glmm  = fit_glmm,
      var_names = list(DV = "y", grouping = "id"),
      gradient_derivative_method = "simple"
    )

    expect_equal(
      TC_stat$results$D,
      0.06745314,
      tol = 1e-5
    )
    expect_equal(
      TC_stat$results$p_value,
      0.7950817,
      tol = 1e-5
    )

  }
)
