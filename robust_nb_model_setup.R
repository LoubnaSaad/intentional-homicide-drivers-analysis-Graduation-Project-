library(robNB)
y <- data$Victims.Counts
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

params3<- list(
  y = y,
  designX = designX[, -1],  # Exclude intercept, all covariates
  offset = offset,
  c.beta = 15,
  c.sigma = 15,
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
wt_joint2 <- wt.rob(d = 7, y = params2$y, designX = params2$designX, offset = params2$offset, 
                    c.beta = params2$c.beta, c.sigma = params2$c.sigma, weights.on.x = params2$weights.on.x,
                    quantile.used = params2$quantile.used, minsig = params2$minsig, maxsig = params2$maxsig,
                    minmu = params2$minmu, maxmu = params2$maxmu, maxit = params2$maxit, tol = params2$tol,
                    maxit.sig = params2$maxit.sig, tol.sig = params2$tol.sig, warn = param2s$warn)
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
wt_crime2 <- wt.rob(d = 1, y = params2$y, designX = designX_crime, offset = params2$offset, 
                    c.beta = params2$c.beta, c.sigma = params2$c.sigma, weights.on.x = params$weights.on.x,
                    quantile.used = params2$quantile.used, minsig = params2$minsig, maxsig = params$maxsig,
                    minmu = params2$minmu, maxmu = params2$maxmu, maxit = params2$maxit, tol = params$tol,
                    maxit.sig = params2$maxit.sig, tol.sig = params2$tol.sig, warn = params2$warn)
cat(sprintf("Wald Test for Crime: Statistic = %.4f, P-value = %.4f\n", wt_crime$value, compute_p_value(wt_crime$value, 1)))

st_crime <- st.rob(d = 1, y = params2$y, designX = designX_crime, offset = params$offset, 
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
plot_data_s <- data.frame(
  fitted = model_s$fitted,
  residuals.Pearson =model_s$residuals.Pearson,
  weights.y = model_s$weights.y  # or replace with your specific weights vector
)
p_s  <- ggplot(plot_data_s, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_x_log10() +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Residuals vs. Fitted Values:model_s", x = "Fitted Values (log scale)", y = "Pearson Residuals") +
  theme_minimal()
p_s_notscaled  <- ggplot(plot_data_s, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Residuals vs. Fitted Values:model_s", x = "Fitted Values (without scale)", y = "Pearson Residuals") +
  theme_minimal()
###########scale 
y_a <- data_new2$Victims.Counts

# Predictors: corrected column names
predictors_a<- c("health.exp", "internet.rate", "CPI.score", 
                 "Unemployment", "Crime", "Annual.Mean", "HDI")
X_a <- as.matrix(data_new2[, predictors_a])
X_scaled <- scale(X_a)  # Standardize predictors
designX_a <- cbind(1, X_scaled)  # Add intercept column
offset_new <- log(data_new2$Population)

