context("test-sim_sv")

test_that("Test sim_sv", {
# Data --------------------------------------------------------------------

  param <- list(phi = 0.9, 
                sigma_h = 0.4,
                sigma_y = 0.2,
                df = 5, 
                alpha = 5,
                rho = -0.7)
  nobs <- 1000
  models <- c("gaussian", "t", "leverage", "skew_gaussian")
  
  for (model in models) {
    
    data <- sim_sv(param = param, nobs = nobs, model = model)
    
    # Tests -------------------------------------------------------------------
    expect_equal(nrow(data), nobs)
    expect_equal(ncol(data), 2)
    expect_named(data, c("y", "h"))
    expect_type(data$y, "double")
    expect_type(data$h, "double")
    expect_equal(attributes(data)$param, param)
    expect_equal(attributes(data)$model, model)
    
  }
  
  expect_error(sim_sv(param = c(1, 2, 3)))
  expect_error(sim_sv(model = "tmp"))
  
})