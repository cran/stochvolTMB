## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 3
)
library(stochvolTMB)

## -----------------------------------------------------------------------------
data(spy)
plot(spy$date, spy$log_return, type = "l", xlab = "", ylab = "", main = "Log-returns of S&P500")
plot(spy$date, spy$price, type = "l", xlab = "", ylab = "", main = "Price of S&P500")

## ----warning=FALSE------------------------------------------------------------
comparison <- compare_models(spy$log_return)
fits <- attr(comparison, "fits")
gaussian <- fits$gaussian
t_dist <- fits$t
skew_gaussian <- fits$skew_gaussian
leverage <- fits$leverage
skew_gaussian_leverage <- fits$skew_gaussian_leverage
skew_t <- fits$skew_t
skew_t_leverage <- fits$skew_t_leverage

## -----------------------------------------------------------------------------
summary(t_dist, report = "transformed")

## -----------------------------------------------------------------------------
summary(skew_gaussian, report = "transformed")

## -----------------------------------------------------------------------------
summary(leverage, report = "transformed")

## -----------------------------------------------------------------------------
summary(skew_gaussian_leverage, report = "transformed")

## -----------------------------------------------------------------------------
comparison[, .(model, n_par, log_lik, aic, bic, delta_aic, converged, pd_hessian)]

## -----------------------------------------------------------------------------
summary(skew_t_leverage, report = "transformed")

## -----------------------------------------------------------------------------
coef(skew_t_leverage)
sqrt(diag(vcov(skew_t_leverage)))

## -----------------------------------------------------------------------------
param <- list(phi = 0.95, sigma_h = 0.25, sigma_y = 0.01, alpha = -2, rho = -0.7)
sim <- sim_sv(param, nobs = 2000, seed = 1, model = "skew_gaussian_leverage")
fit <- estimate_parameters(sim$y, model = "skew_gaussian_leverage", silent = TRUE)
summary(fit, report = "transformed")

## -----------------------------------------------------------------------------
plot(leverage, include_ci = TRUE, plot_log = TRUE, dates = spy$date)
plot(leverage, include_ci = TRUE, plot_log = FALSE, dates = spy$date)

## -----------------------------------------------------------------------------

pred = predict(leverage, steps = 10, include_parameters = TRUE)
summary(pred)

# plot the forecast
plot(leverage, forecast = 50) + ggplot2::xlim(3200, nrow(spy) + 50)

## -----------------------------------------------------------------------------
plot_returns(skew_t_leverage, dates = spy$date)

## ----fig.height=3.5-----------------------------------------------------------
plot_diagnostics(gaussian)
plot_diagnostics(skew_t_leverage)

## -----------------------------------------------------------------------------
risk <- var_es(skew_t_leverage, level = c(0.95, 0.99))
risk[is.na(y)]

## -----------------------------------------------------------------------------
backtest_var(risk)
backtest_var(var_es(gaussian, level = c(0.95, 0.99)))

## ----fig.height=4-------------------------------------------------------------
plot_var(risk, level = 0.99, dates = spy$date)

## -----------------------------------------------------------------------------
set.seed(1)
forecast_var_es(skew_t_leverage, steps = 10, level = 0.99)

## ----include=FALSE------------------------------------------------------------
stochvol_gauss <- readRDS("stochvol_gauss.rds")
stochvol_lev <- readRDS("stochvol_lev.rds")
stochvolTMB_gauss  <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE)
stochvolTMB_lev  <- estimate_parameters(spy$log_return, "leverage", silent = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# library(stochvol)
# 
# stochvol_gauss <- svsample(spy$log_return, quiet = T)
# stochvolTMB_gauss  <- estimate_parameters(spy$log_return, "gaussian", silent = TRUE)
# 
# stochvol_lev <- svlsample(spy$log_return, quiet = T)
# stochvolTMB_lev  <- estimate_parameters(spy$log_return, "leverage", silent = TRUE)

## -----------------------------------------------------------------------------

stochvol_gauss$para
summary(stochvolTMB_gauss, report = "transformed")
stochvol_lev$para
summary(stochvolTMB_lev, report = "transformed")


## ----fig.height=3.5-----------------------------------------------------------
param <- list(phi = 0.95, sigma_h = 0.3, sigma_y = 0.01, alpha = -3, rho = -0.7)
sim <- sim_sv(param, nobs = 3000, seed = 2, model = "skew_gaussian_leverage")
fit_sim <- estimate_parameters(sim$y, model = "skew_gaussian_leverage", silent = TRUE)
plot_diagnostics(fit_sim)

