library(robNB)
y <- data$Victims.Counts
###########scale 
y_a <- data_new2$Victims.Counts

# Predictors: corrected column names
predictors_a<- c("health.exp", "internet.rate", "CPI.score", 
                 "Unemployment", "Crime", "Annual.Mean", "HDI")
X_a <- as.matrix(data_new2[, predictors_a])
X_scaled <- scale(X_a)  # Standardize predictors
designX_a <- cbind(1, X_scaled)  # Add intercept column
offset_new <- log(data_new2$Population)
#######not scale 
designX <- cbind(
  rep(1, nrow(data)),  # Intercept
  data$health.exp,
  data$internet.rate,
  data$CPI.score,
  data$Unemployment,
  data$Crime,
  data$Annual.Mean,
  data$HDI
)
colnames(designX) <- c("Intercept", "health_exp", "internet.rate", "CPI.score", "Unemployment", "Crime", "Annual.Mean", "HDI")
offset <- log(data$Population)
set.seed(123)

default_model <- nb.glm.rob(
  y = y,
  designX = designX,
  offset = offset,
  c.tukey.beta = 10,
  c.tukey.sigma = 10,
  weights.on.x = "none",
  quantile.used = floor(length(y) * 0.8),
  minsig = 0.001,
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,
  tol = 1e-05,
  maxit.sig = 30,
  tol.sig = 1e-06,
  warn = FALSE
)

params<- list(
  y = y,
  designX = designX[, -1],  # Exclude intercept, all covariates
  offset = offset,
  c.beta = 10,
  c.sigma = 10,
  weights.on.x = "none",
  quantile.used = floor(length(y) * 0.8),
  minsig = 0.001,
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,
  tol = 1e-05,
  maxit.sig = 30,
  tol.sig = 1e-06,
  warn = FALSE
)
###################################joint tests
wt_joint <- wt.rob(d = 7, y = params$y, designX = params$designX, offset = params$offset, 
                    c.beta = params$c.beta, c.sigma = params$c.sigma, weights.on.x = params$weights.on.x,
                    quantile.used = params$quantile.used, minsig = params$minsig, maxsig = params$maxsig,
                    minmu = params$minmu, maxmu = params$maxmu, maxit = params$maxit, tol = params$tol,
                    maxit.sig = params$maxit.sig, tol.sig = params$tol.sig, warn = param2s$warn)
cat(sprintf("Wald Test: Statistic = %.4f, P-value = %.4f\n", wt_joint2$value, compute_p_value(wt_joint2$value, 7)))
####################individual tests
designX_crime <- as.matrix(data$Crime)  # Single column for Crime
designX_unemp <- as.matrix(data$Unemployment)  
designX_HDI <- as.matrix(data$HDI)  
designX_Annual.Mean <- as.matrix(data$Annual.Mean)  
designX_internet.rate <- as.matrix(data$internet.rate)  
designX_CPI.score <- as.matrix(data$CPI.score)  
designX_health_exp <- as.matrix(data$health.exp)  

# Function to compute p-value
compute_p_value <- function(test_value, d) {
  pchisq(test_value, df = d, lower.tail = FALSE)
}

# Test for Crime (d=1) - Isolate Crime column
cat("\nTesting Crime (d=1)\n")
cat("==================\n")
##model2
wt_crime2 <- wt.rob(d = 1, y = params$y, designX = designX_crime, offset = params$offset, 
                    c.beta = params$c.beta, c.sigma = params$c.sigma, weights.on.x = params$weights.on.x,
                    quantile.used = params$quantile.used, minsig = params$minsig, maxsig = params$maxsig,
                    minmu = params$minmu, maxmu = params$maxmu, maxit = params$maxit, tol = params$tol,
                    maxit.sig = params$maxit.sig, tol.sig = params$tol.sig, warn = params$warn)
cat(sprintf("Wald Test for Crime: Statistic = %.4f, P-value = %.4f\n", wt_crime$value, compute_p_value(wt_crime$value, 1)))

st_crime <- st.rob(d = 1, y = params$y, designX = designX_crime, offset = params$offset, 
                   c.beta = params$c.beta, c.sigma = params$c.sigma, weights.on.x = params$weights.on.x,
                   quantile.used = params$quantile.used, minsig = params$minsig, maxsig = params$maxsig,
                   minmu = params$minmu, maxmu = params$maxmu, maxit = params$maxit, tol = params$tol,
                   maxit.sig = params$maxit.sig, tol.sig = params$tol.sig, warn = params$warn)
cat(sprintf("Score Test for Crime: Statistic = %.4f, P-value = %.4f\n", st_crime$value, compute_p_value(st_crime$value, 1)))

sdpt_crime <- sdpt.rob(d = 1, y = params$y, designX = designX_crime, offset = params$offset, 
                       c.beta = params$c.beta, c.sigma = params$c.sigma, weights.on.x = params$weights.on.x,
                       quantile.used = params$quantile.used, minsig = params$minsig, maxsig = params$maxsig,
                       minmu = params$minmu, maxmu = params$maxmu, maxit = params$maxit, tol = params$tol,
                       maxit.sig = params$maxit.sig, tol.sig = params$tol.sig, warn = params$warn)
cat(sprintf("Saddlepoint Test for Crime: Statistic = %.4f, P-value = %.4f\n", sdpt_crime$value, compute_p_value(sdpt_crime$value, 1)))

esdpt_crime <- esdpt.rob(d = 1, y = params$y, designX = designX_crime, offset = params$offset, 
                         c.beta = params$c.beta, c.sigma = params$c.sigma, weights.on.x = params$weights.on.x,
                         quantile.used = params$quantile.used, minsig = params$minsig, maxsig = params$maxsig,
                         minmu = params$minmu, maxmu = params$maxmu, maxit = params$maxit, tol = params$tol,
                         maxit.sig = params$maxit.sig, tol.sig = params$tol.sig, warn = params$warn)
cat(sprintf("Empirical Saddlepoint Test for Crime: Statistic = %.4f, P-value = %.4f\n", esdpt_crime$value, compute_p_value(esdpt_crime$value, 1)))

tett_crime <- tett.rob(d = 1, y = params$y, designX = designX_crime, offset = params$offset, 
                       c.beta = params$c.beta, c.sigma = params$c.sigma, weights.on.x = params$weights.on.x,
                       quantile.used = params$quantile.used, minsig = params$minsig, maxsig = params$maxsig,
                       minmu = params$minmu, maxmu = params$maxmu, maxit = params$maxit, tol = params$tol,
                       maxit.sig = params$maxit.sig, tol.sig = params$tol.sig, warn = params$warn)
cat(sprintf("Tilted Exponential Tilting Test for Crime: Statistic = %.4f, P-value = %.4f\n", tett_crime$value, compute_p_value(tett_crime$value, 1)))
##
plot_data_default <- data.frame(
  fitted = default_model$fitted,
  residuals.Pearson =default_model$residuals.Pearson,
  weights.y = default_model$weights.y  # or replace with your specific weights vector
)
p_default<- ggplot(plot_data_s, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_log10() +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Residuals vs. Fitted Values:model_s", x = "Fitted Values (log scale)", y = "Pearson Residuals") +
  theme_minimal()
p_default_notscaled  <- ggplot(plot_data_s, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Residuals vs. Fitted Values:model_s", x = "Fitted Values (without scale)", y = "Pearson Residuals") +
  theme_minimal()

ggsave("fitted_vs_res_notscaled_model_default_.png",
       plot = p_default_notscaled,
       width = 13.66,      # 1366px / 100dpi
       height = 7.05,      # 705px / 100dpi
       dpi = 100,         
       bg = "white")
#######godness of fit
null_default_model <- nb.glm.rob(y = y_a, designX = matrix(1, ncol = 1, nrow = length(y_a)),
                         offset = offset,
                         c.tukey.beta = 10,
                         c.tukey.sigma = 10,
                         weights.on.x = "none",
                         quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
                         minsig = 0.001,
                         maxsig = 50,
                         minmu = 1e-10,
                         maxmu = 1e+20,
                         maxit = 100,
                         tol = 1e-5,
                         maxit.sig = 50,
                         tol.sig = 1e-6,
                         warn = TRUE)

# 1. Pearson Chi-Squared Statistic
pearson_residuals <- (y - default_model $fitted) / sqrt(default_model $fitted * (1 + default_model $fitted *  0.62771432 )) #model sigma 0.62771432 لو عرفت تستدعيها علي طول ياريت عشان بتتغير حسب الموديل
pearson_chi_sq <- sum(default_model $weights.y * pearson_residuals^2)
df_pearson <- length(y) - length(default_model $coef)  # Degrees of freedom: n - p
p_value_pearson <- 1 - pchisq(pearson_chi_sq, df_pearson)
cat("Pearson Chi-Squared:", pearson_chi_sq, " (df =", df_pearson, ", p-value =", p_value_pearson, ")\n")
deviance_residuals <- residuals(default_model , type = "deviance")
deviance_residuals_null <- residuals(null_default_model, type = "deviance")

# 2. Deviance Statistic
deviance <- sum(default_model $weights.y * deviance_residuals^2)
df_deviance <- length(y) - length(default_model $coef)
p_value_deviance <- 1 - pchisq(deviance, df_deviance)
cat("Deviance:", deviance, " (df =", df_deviance, ", p-value =", p_value_deviance, ")\n")

# 2. Deviance null Statistic
deviance_null <- sum(null_default_model$weights.y * deviance_residuals_null^2)
df_deviance_null <- length(y) - length(null_default_model$coef)
p_value_deviance_null <- 1 - pchisq(deviance_null, df_deviance_null)
cat("Deviance:", deviance_null, " (df =", df_deviance_null, ", p-value =", p_value_deviance_null, ")\n")


# R2 

size <- 1/ 0.62771432 #1/sigma
size_null<- 1/1.55978 

logLik_full <- -sum(default_model $weights.y * dnbinom(y, mu = default_model $fitted, size = size, log = TRUE))
logLik_null <- -sum(null_default_model$weights.y * dnbinom(y, mu = null_default_model$fitted, size = size_null, log = TRUE))
pseudo_r2 <- 1 - (logLik_full / logLik_null)
pseudo_r2





library(ggplot2)


# 2. Residuals Plot
ggplot(data.frame(Fitted = fitted_vals, Residuals = residuals), aes(Fitted, Residuals)) +
  geom_point(color = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Raw Residuals") +
  theme_minimal()
plot(data$internet.rate,log(data$Victims.Counts))

