# 1. Pearson Chi-Squared Statistic
pearson_residuals <- (y -  model_new1  $fitted) / sqrt( model_new1  $fitted * (1 +  model_new1  $fitted *  0.68992845 ))
pearson_chi_sq <- sum( model_new1  $weights.y * pearson_residuals^2)
df_pearson <- length(y) - length( model_new1  $coef)  # Degrees of freedom: n - p
p_value_pearson <- 1 - pchisq(pearson_chi_sq, df_pearson)
cat("Pearson Chi-Squared:", pearson_chi_sq, " (df =", df_pearson, ", p-value =", p_value_pearson, ")\n")
deviance_residuals <- residuals( model_new1  , type = "deviance")
deviance_residuals_null <- residuals(model_trans4_vv_null, type = "deviance")

# 2. Deviance Statistic
deviance <- sum( model_new1  $weights.y * deviance_residuals^2)
df_deviance <- length(y) - length( model_new1  $coef)
p_value_deviance <- 1 - pchisq(deviance, df_deviance)
cat("Deviance:", deviance, " (df =", df_deviance, ", p-value =", p_value_deviance, ")\n")

# 2. Deviance null Statistic
deviance_null <- sum(null_model_new1 $weights.y * deviance_residuals_null^2)
df_deviance_null <- length(y) - length(null_model_new1 $coef)
p_value_deviance_null <- 1 - pchisq(deviance_null, df_deviance_null)
cat("Deviance:", deviance_null, " (df =", df_deviance_null, ", p-value =", p_value_deviance_null, ")\n")
pR2(model_new1)
library(pscl)
MSE()
logLik(model_new1)
library(p)
??robNB
# R2 
AIC(model_new1)
BIC(model_sv22).
size <- 1/ 0.68992845 
size_null<- 1/1.732613 

logLik_full <- -sum( model_new1  $weights.y * dnbinom(y, mu =  model_new1  $fitted, size = size, log = TRUE))
logLik_null <- -sum(model_trans4_vv_null$weights.y * dnbinom(y, mu = model_trans4_vv_null$fitted, size = size_null, log = TRUE))
pseudo_r2 <- 1 - (logLik_full / logLik_null)
pseudo_r2
###trans 
y <- data$Victims.Counts
data$trans.Food.insequire<- (data$Food.insequire)^2
data$trans.Annual.Mean<- (data$Annual.Mean)^2

# Define predictors
predictors <- c("CPI.score","internet.rate","trans2.Food.insequire", "Unemployment", "Crime", "trans.Annual.Mean", "HDI")
data$trans2.Food.insequire<-sqrt(data$Food.insequire)

# Standardize predictors
X <- scale(data[, predictors])

# Design matrix with intercept
designX_a2 <- cbind(1, X)
model_trans1 <- nb.glm.rob(
  y = y_a,
  designX = designX_a2,
  offset = offset_new,
  c.tukey.beta = 7,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)
model_trans3 <- nb.glm.rob(
  y = y_a,
  designX = designX_a2,
  offset = offset_new,
  c.tukey.beta = 7,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)

predictors2 <- c("CPI.score","internet.rate","trans2.Food.insequire", "Unemployment", "Crime", "trans.Annual.Mean", "HDI")
X2 <- scale(data[, predictors2])
designX_a22 <- cbind(1, X2)

model_trans2 <- nb.glm.rob(
  y = y_a,
  designX = designX_a22,
  offset = offset_new,
  c.tukey.beta = 7,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)

model_trans4 <- nb.glm.rob(
  y = y_a,
  designX = designX_a22,
  offset = offset_new,
  c.tukey.beta = 7,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)
model_trans4_vv <- nb.glm.rob(
  y = y_a,
  designX = designX_a22,
  offset = offset_new,
  c.tukey.beta = 2,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)

predictors3 <- c("CPI.score","internet.rate","Food.insequire", "Unemployment", "Crime", "Annual.Mean", "HDI")
X2 <- scale(data[, predictors2])
designX_a22 <- cbind(1, X2)

model_trans2 <- nb.glm.rob(
  y = y_a,
  designX = designX_a22,
  offset = offset_new,
  c.tukey.beta = 7,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)

model_trans5 <- nb.glm.rob(
  y = y_a,
  designX = designX_a22,
  offset = offset_new,
  c.tukey.beta = 4,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)
model_no_trans5 <- nb.glm.rob(
  y = y_a,
  designX = designX_a,
  offset = offset_new,
  c.tukey.beta = 4,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)


library(robNB)


####null
model_trans1_null <- nb.glm.rob(
  y = y_a, 
  designX = matrix(1, ncol = 1, nrow = length(y_a)),
  offset = offset_new,
  c.tukey.beta = 7,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)
null_model <- nb.glm.rob(y = y_a, designX = matrix(1, ncol = 1, nrow = length(y_a)),
                         offset = offset,
                         c.tukey.beta = 5,
                         c.tukey.sigma = 5,
                         weights.on.x = "none",
                         quantile.used = floor(length(y_a) * 0.5),  # 70% of data for MVE
                         minsig = 0.001,
                         maxsig = 50,
                         minmu = 1e-10,
                         maxmu = 1e+20,
                         maxit = 100,
                         tol = 1e-5,
                         maxit.sig = 50,
                         tol.sig = 1e-6,
                         warn = TRUE)
null_model_svl4 <- nb.glm.rob(y = y_a, designX = matrix(1, ncol = 1, nrow = length(y_a)),
                              offset = offset,
                              c.tukey.beta = 4,
                              c.tukey.sigma = 5,
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
null_model_svl2 <- nb.glm.rob(y = y_a, designX = matrix(1, ncol = 1, nrow = length(y_a)),
                              offset = offset,
                              c.tukey.beta = 2,
                              c.tukey.sigma = 5,
                              weights.on.x = "robCov",
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

model_trans4_vv_null <- nb.glm.rob(
  y = y_a, 
  designX = matrix(1, ncol = 1, nrow = length(y_a)),
  offset = offset_new,
  c.tukey.beta = 2,         # Robust tuning for beta
  c.tukey.sigma = 10,        # Robust tuning for sigma
  weights.on.x = "none",  # Robust weights for continuous predictors
  quantile.used = floor(length(y_a) * 0.8),  # 70% of data for MVE
  minsig = 0.001,           # Default numerical bounds
  maxsig = 50,
  minmu = 1e-10,
  maxmu = 1e+20,
  maxit = 50,               # Default iteration settings
  tol = 1e-5,
  maxit.sig = 30,
  tol.sig = 1e-6,
  warn = TRUE               # Enable warnings for diagnostics
)


null_model_new1 <- nb.glm.rob(y = y_a, designX = matrix(1, ncol = 1, nrow = length(y_a)),
                         offset = offset,
                         c.tukey.beta = 2,
                         c.tukey.sigma = 15,
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


null_model_new1 <- nb.glm.rob(y = y_a, designX = matrix(1, ncol = 1, nrow = length(y_a)),
                              offset = offset,
                              c.tukey.beta = 2,
                              c.tukey.sigma = 15,
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









