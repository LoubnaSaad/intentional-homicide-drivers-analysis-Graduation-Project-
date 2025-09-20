# Load required packages
library(robNB)
library(ggplot2)
library(moments)
library(dplyr)
par(mfrow=c(4,3))
# Load and prepare data
data <- read.csv("phase1_missing2.csv")
data<-na.omit(data)
data <- data %>%
  rename(
    Temperature = Annual.Mean,
    Health.expenditure = health.exp,
    Counts.of.Victims = Victims.Counts,
    Food.security  =  Food.insequire,
    Corruption.Score=CPI.score,
    Crime.Index=Crime
  )
y_a <- data$Counts.of.Victims
predictors <- c("Temperature", "Crime.Index", "Food.security", "Unemployment", "Corruption.Score")
X<- data[, predictors]
designX <- cbind(
  rep(1, nrow(data)),  # Intercept
  data$Temperature,
  data$Crime.Index,
  data$Unemployment,
  data$Corruption.Score,
  data$Food.security
 
)
colnames(designX) <- c("Intercept", "Temperature", "Crime.Index", "Unemployment", "Corruption.Score", "Food.security")
offset <- log(data$Population)
######################################Model2: model(4,3) with lasso vars#############################
set.seed(123)

model_4.3 <- nb.glm.rob(y = y_a,
designX = designX,
offset = offset,
c.tukey.beta = 4,
c.tukey.sigma = 3,
weights.on.x = "none",
minsig = 0.001,
maxsig = 50,
minmu = 1e-10,
maxmu = 1e+20,
maxit = 50,
tol = 1e-05,
maxit.sig = 30,
tol.sig = 1e-06,
warn = FALSE)

# Extract residuals, weights, and NB parameters
residuals_pearson_4.3 <- model_4.3$residuals.Pearson
weights_y_4.3 <- model_4.3$weights.y
theta_4.3 <- model_4.3$coef[1]  # Dispersion parameter
mu_4.3 <- mean(model_4.3$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_4.3 <- length(residuals_pearson_4.3)

# Manually compute quantiles
sample_quantiles_4.3 <- sort(residuals_pearson_4.3)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_4.3 <- qnorm((1:n_4.3 - 0.5) / n_4.3) # Normal quantiles
# Combine into a data frame for plotting
qq_data_4.3 <- data.frame(
  theoretical = theoretical_quantiles_4.3,
  sample = sample_quantiles_4.3,
  weight = weights_y_4.3[order(residuals_pearson_4.3)] ,
  Country = data$Country[order(residuals_pearson_4.3)] # Match weights to sorted residuals
)

# Create weighted Q-Q plot using ggplot2
p_4.3 <- ggplot(qq_data_4.3, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") +  # Reference line
  geom_text(data = subset(qq_data_4.3, weight == 0), 
                         aes(label =Country), 
                         size = 1.5, 
                         vjust = -1, 
                         hjust = 0.5, 
                         color = "black") +
   labs(title = "Model 2(4,3):Weighted Q-Q Plot of Pearson Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_4.3), max(sample_quantiles_4.3))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_4.3), max(theoretical_quantiles_4.3)))  # Adjust x-limits


plot_data_4.3 <- data.frame(
  fitted = model_4.3$fitted,
  residuals.Pearson =model_4.3$residuals.Pearson,
  weights.y = model_4.3$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_4.3 <- ggplot(plot_data_4.3, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,3): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_4.3_zoom1 <- ggplot(plot_data_4.3, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,3): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_4.3_zoom2 <- ggplot(plot_data_4.3, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,3): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )

p_4.3_notscaled  <- ggplot(plot_data_4.3,aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_4.3, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  
  labs(title = "Model 2(4,3): Residuals versus Fitted Values with Robust Weighting", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal()
####
# Extract data
y_4.3 <-data$Counts.of.Victims 
y_hat_4.3 <- round(model_4.3$fitted)
w_4.3 <- model_4.3$weights.y
sum(model_4.3$weights.y==0)
# Weighted means
y_bar_w_4.3 <- sum(w_4.3 * y_4.3) / sum(w_4.3)
y_hat_bar_w_4.3 <- sum(w_4.3 * y_hat_4.3) / sum(w_4.3)

# Numerator and denominators for correlation
numerator_4.3 <- sum(w_4.3 * (y_4.3 - y_bar_w_4.3) * (y_hat_4.3 - y_hat_bar_w_4.3))
denominator_y_4.3 <- sqrt(sum(w_4.3 * (y_4.3 - y_bar_w_4.3)^2))
denominator_y_hat_4.3 <- sqrt(sum(w_4.3 * (y_hat_4.3 - y_hat_bar_w_4.3)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_4.3 <- (numerator_4.3 / (denominator_y_4.3 * denominator_y_hat_4.3))^2

# Ensure values are within [0, 1]
R2_corr_weighted_4.3 <- max(0, min(1, R2_corr_weighted_4.3))
print(R2_corr_weighted_4.3)
###########################Model 1: Model (5,5) with lasso vars#####################################
set.seed(123)
model_5.5 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 5,
                        c.tukey.sigma = 5,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
residuals_pearson_5.5 <- model_5.5$residuals.Pearson
weights_y_5.5 <- model_5.5$weights.y
theta_5.5 <- model_5.5$coef[1]  # Dispersion parameter
mu_5.5 <- mean(model_5.5$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_5.5 <- length(residuals_pearson_5.5)

# Manually compute quantiles
sample_quantiles_5.5 <- sort(residuals_pearson_5.5)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_5.5 <- qnorm((1:n_5.5 - 0.5) / n_5.5) # Normal quantiles
# Combine into a data frame for plotting
qq_data_5.5 <- data.frame(
  theoretical = theoretical_quantiles_5.5,
  sample = sample_quantiles_5.5,
  weight = weights_y_5.5[order(residuals_pearson_5.5)] ,
  Country = data$Country[order(residuals_pearson_5.5)])
par(mfrow=c(4,3))

# Create weighted Q-Q plot using ggplot2
p_5.5 <- ggplot(qq_data_5.5, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_5.5, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+
labs(title = "Model 1(5,5):Weighted Q-Q Plot of Pearson Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_5.5), max(sample_quantiles_5.5))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_5.5), max(theoretical_quantiles_5.5)))  # Adjust x-limits
p_5.5

plot_data_5.5 <- data.frame(
  fitted = model_5.5$fitted,
  residuals.Pearson =model_5.5$residuals.Pearson,
  weights.y = model_5.5$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_5.5 <- ggplot(plot_data_5.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(
    title = "Model 1(5,5): Comparison of Fitted and Actual Counts with Robust Weighting",
    x = "Fitted Values",
    y = "Observed Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11)
  )

actual_5.5_zoom1 <- ggplot(plot_data_5.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 1(5,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_5.5_zoom2 <- ggplot(plot_data_5.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 1(5,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
p_5.5_notscaled  <- ggplot(plot_data_5.5,aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_5.5, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  labs(title = "Model 1(5,5): Residuals versus Fitted Values with Robust Weighting", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal()

####
# Extract data
y_5.5<-data$Counts.of.Victims 
y_hat_5.5<- round(model_5.5$fitted)
w_5.5<- model_5.5$weights.y
sum(model_5.5$weights.y==0)
# Weighted means
y_bar_w_5.5<- sum(w_5.5* y_5.5) / sum(w_5.5)
y_hat_bar_w_5.5<- sum(w_5.5* y_hat_5.5) / sum(w_5.5)

# Numerator and denominators for correlation
numerator_5.5<- sum(w_5.5* (y_5.5- y_bar_w_5.5) * (y_hat_5.5- y_hat_bar_w_5.5))
denominator_y_5.5<- sqrt(sum(w_5.5* (y_5.5- y_bar_w_5.5)^2))
denominator_y_hat_5.5<- sqrt(sum(w_5.5* (y_hat_5.5- y_hat_bar_w_5.5)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_5.5<- (numerator_5.5/ (denominator_y_5.5* denominator_y_hat_5.5))^2

# Ensure values are within [0, 1]
R2_corr_weighted_5.5<- max(0, min(1, R2_corr_weighted_5.5))
print(R2_corr_weighted_5.5)
###########################Model 3: Model (5,4) with lasso vars#####################################
set.seed(123)
model_5.4 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 5,
                        c.tukey.sigma = 4,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
residuals_pearson_5.4 <- model_5.4$residuals.Pearson
weights_y_5.4 <- model_5.4$weights.y
theta_5.4 <- model_5.4$coef[1]  # Dispersion parameter
mu_5.4 <- mean(model_5.4$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_5.4 <- length(residuals_pearson_5.4)

# Manually compute quantiles
sample_quantiles_5.4 <- sort(residuals_pearson_5.4)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_5.4 <- qnorm((1:n_5.4 - 0.5) / n_5.4) # Normal quantiles
# Combine into a data frame for plotting
qq_data_5.4 <- data.frame(
  theoretical = theoretical_quantiles_5.4,
  sample = sample_quantiles_5.4,
  weight = weights_y_5.4[order(residuals_pearson_5.4)],
  Country = data$Country[order(residuals_pearson_5.4)]
)
qq_data_4.5
# Create weighted Q-Q plot using ggplot2
p_5.4 <- ggplot(qq_data_5.4, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") +
  geom_text(data = subset(qq_data_5.4, weight == 0), 
                         aes(label =Country), 
                         size = 1.5, 
                         vjust = -1, 
                        hjust = 0.5, 
                        color = "black")   +
  labs(title = "Model 3(5,4):Weighted Q-Q Plot of Pearson Residuals",
       x = "Theoretical Quantiles", 
       y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_5.4), max(sample_quantiles_5.4))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_5.4), max(theoretical_quantiles_5.4)))  # Adjust x-limits


plot_data_5.4 <- data.frame(
  fitted = model_5.4$fitted,
  residuals.Pearson =model_5.4$residuals.Pearson,
  weights.y = model_5.4$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_5.4 <- ggplot(plot_data_5.4, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(
    title = "Model 3(5,4): Comparison of Fitted and Actual Counts with Robust Weighting",
    x = "Fitted Values",
    y = "Observed Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )

actual_5.4

actual_5.4_zoom1 <- ggplot(plot_data_5.4, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 3(5,4): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_5.4_zoom2 <- ggplot(plot_data_5.4, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 3(5,4): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )

p_5.4_notscaled  <- ggplot(plot_data_5.4,aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_5.4, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+  
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 3(5,4): Residuals versus Fitted Values with Robust Weighting", x = "Fitted Values", y = "Pearson Residuals") +
  theme_minimal()

####
####
# Extract data
y_5.4<-data$Counts.of.Victims 
y_hat_5.4<- round(model_5.4$fitted)
w_5.4<- model_5.4$weights.y
sum(model_5.4$weights.y==0)
# Weighted means
y_bar_w_5.4<- sum(w_5.4* y_5.4) / sum(w_5.4)
y_hat_bar_w_5.4<- sum(w_5.4* y_hat_5.4) / sum(w_5.4)

# Numerator and denominators for correlation
numerator_5.4<- sum(w_5.4* (y_5.4- y_bar_w_5.4) * (y_hat_5.4- y_hat_bar_w_5.4))
denominator_y_5.4<- sqrt(sum(w_5.4* (y_5.4- y_bar_w_5.4)^2))
denominator_y_hat_5.4<- sqrt(sum(w_5.4* (y_hat_5.4- y_hat_bar_w_5.4)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_5.4<- (numerator_5.4/ (denominator_y_5.4* denominator_y_hat_5.4))^2

# Ensure values are within [0, 1]
R2_corr_weighted_5.4<- max(0, min(1, R2_corr_weighted_5.4))
print(R2_corr_weighted_5.4)
###########################Model 4: Model (4,5) with lasso vars#####################################
set.seed(123)
model_4.5 <- nb.glm.rob(y = y_a,
                          designX = designX,
                          offset = offset,
                          c.tukey.beta = 4,
                          c.tukey.sigma = 5,
                          weights.on.x = "none",
                          minsig = 0.001,
                          maxsig = 50,
                          minmu = 1e-10,
                          maxmu = 1e+20,
                          maxit = 50,
                          tol = 1e-05,
                          maxit.sig = 30,
                          tol.sig = 1e-06,
                          warn = FALSE)
####
residuals_pearson_4.5 <- model_4.5$residuals.Pearson
weights_y_4.5 <- model_4.5$weights.y
theta_4.5 <- model_4.5$coef[1]  # Dispersion parameter
mu_4.5 <- mean(model_4.5$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_4.5 <- length(residuals_pearson_4.5)

# Manually compute quantiles
sample_quantiles_4.5 <- sort(residuals_pearson_4.5)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_4.5 <- qnorm((1:n_4.5 - 0.5) / n_4.5) # Normal quantiles
# Combine into a data frame for plotting
qq_data_4.5  <- data.frame(
  theoretical = theoretical_quantiles_4.5,
  sample = sample_quantiles_4.5,
  weight = weights_y_4.5[order(residuals_pearson_4.5)],
  Country = data$Country[order(residuals_pearson_4.5)]
)

# Create weighted Q-Q plot using ggplot2
p_4.5 <- ggplot(qq_data_4.5, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_4.5, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 4(4,5):Weighted Q-Q Plot of Pearson Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_4.5), max(sample_quantiles_4.5))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_4.5), max(theoretical_quantiles_4.5)))  # Adjust x-limits


plot_data_4.5 <- data.frame(
  fitted = model_4.5$fitted,
  residuals.Pearson =model_4.5$residuals.Pearson,
  weights.y = model_4.5$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_4.5 <- ggplot(plot_data_4.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 4(4,5): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_4.5_zoom1 <- ggplot(plot_data_4.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 4(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_4.5_zoom2 <- ggplot(plot_data_4.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 4(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )

p_4.5_notscaled <- ggplot(plot_data_4.5,aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_4.5, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 4(4,5): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()

####
# Extract data
y_4.5<-data$Counts.of.Victims 
y_hat_4.5<- round(model_4.5$fitted)
w_4.5<- model_4.5$weights.y
sum(model_4.5$weights.y==0)
# Weighted means
y_bar_w_4.5<- sum(w_4.5* y_4.5) / sum(w_4.5)
y_hat_bar_w_4.5<- sum(w_4.5* y_hat_4.5) / sum(w_4.5)

# Numerator and denominators for correlation
numerator_4.5<- sum(w_4.5* (y_4.5- y_bar_w_4.5) * (y_hat_4.5- y_hat_bar_w_4.5))
denominator_y_4.5<- sqrt(sum(w_4.5* (y_4.5- y_bar_w_4.5)^2))
denominator_y_hat_4.5<- sqrt(sum(w_4.5* (y_hat_4.5- y_hat_bar_w_4.5)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_4.5<- (numerator_4.5/ (denominator_y_4.5* denominator_y_hat_4.5))^2

# Ensure values are within [0, 1]
R2_corr_weighted_4.5<- max(0, min(1, R2_corr_weighted_4.5))
print(R2_corr_weighted_4.5)
###########################Model 5: Model (4,4) with lasso vars#####################################
set.seed(123)
model_4.4 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 2,
                        c.tukey.sigma = 10,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
residuals_pearson_4.4 <- model_4.4$residuals.Pearson
weights_y_4.4 <- model_4.4$weights.y
theta_4.4 <- model_4.4$coef[1]  # Dispersion parameter
mu_4.4 <- mean(model_4.4$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_4.4 <- length(residuals_pearson_4.4)

# Manually compute quantiles
sample_quantiles_4.4 <- sort(residuals_pearson_4.4)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_4.4 <- qnorm((1:n_4.4 - 0.5) / n_4.4) # Normal quantiles
# Combine into a data frame for plotting
# Combine into a data frame for plotting
qq_data_4.4  <- data.frame(
  theoretical = theoretical_quantiles_4.4,
  sample = sample_quantiles_4.4,
  weight = weights_y_4.4[order(residuals_pearson_4.4)],
  Country = data$Country[order(residuals_pearson_4.4)]
)

# Create weighted Q-Q plot using ggplot2
p_4.4 <- ggplot(qq_data_4.4, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_4.4, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 5(4,4):Weighted Q-Q Plot of Pearson Residuals",
                                    x = "Theoretical Quantiles",
                                    y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_4.4), max(sample_quantiles_4.4))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_4.4), max(theoretical_quantiles_4.4)))  # Adjust x-limits


plot_data_4.4 <- data.frame(
  fitted = model_4.4$fitted,
  residuals.Pearson =model_4.4$residuals.Pearson,
  weights.y = model_4.4$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_4.4 <- ggplot(plot_data_4.4, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 5(4,4): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_4.4_zoom1 <- ggplot(plot_data_4.4, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_4.4_zoom2 <- ggplot(plot_data_4.4, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
library(ggrepel)
p_4.4_notscaled <- ggplot(plot_data_4.4,aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_4.4, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 8(4,4): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()

####
# Extract data
y_4.4<-data$Counts.of.Victims 
y_hat_4.4<- round(model_4.4$fitted)
w_4.4<- model_4.4$weights.y
sum(model_4.4$weights.y==0)
# Weighted means
y_bar_w_4.4<- sum(w_4.4* y_4.4) / sum(w_4.4)
y_hat_bar_w_4.4<- sum(w_4.4* y_hat_4.4) / sum(w_4.4)

# Numerator and denominators for correlation
numerator_4.4<- sum(w_4.4* (y_4.4- y_bar_w_4.4) * (y_hat_4.4- y_hat_bar_w_4.4))
denominator_y_4.4<- sqrt(sum(w_4.4* (y_4.4- y_bar_w_4.4)^2))
denominator_y_hat_4.4<- sqrt(sum(w_4.4* (y_hat_4.4- y_hat_bar_w_4.4)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_4.4<- (numerator_4.4/ (denominator_y_4.4* denominator_y_hat_4.4))^2

# Ensure values are within [0, 1]
R2_corr_weighted_4.4<- max(0, min(1, R2_corr_weighted_4.4))
print(R2_corr_weighted_4.4)
###########################Model 6: Model (3,5) with lasso vars#####################################
set.seed(123)
model_3.5 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 3,
                        c.tukey.sigma = 5,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
####
residuals_pearson_3.5 <- model_3.5$residuals.Pearson
weights_y_3.5 <- model_3.5$weights.y
theta_3.5 <- model_3.5$coef[1]  # Dispersion parameter
mu_3.5 <- mean(model_3.5$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_3.5 <- length(residuals_pearson_3.5)

# Manually compute quantiles
sample_quantiles_3.5 <- sort(residuals_pearson_3.5)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_3.5 <- qnorm((1:n_3.5 - 0.5) / n_3.5) # Normal quantiles
# Combine into a data frame for plotting
# Combine into a data frame for plotting
qq_data_3.5  <- data.frame(
  theoretical = theoretical_quantiles_3.5,
  sample = sample_quantiles_3.5,
  weight = weights_y_3.5[order(residuals_pearson_3.5)],
  Country = data$Country[order(residuals_pearson_3.5)]
)

# Create weighted Q-Q plot using ggplot2
p_3.5 <- ggplot(qq_data_3.5, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_3.5, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 6(3,5):Weighted Q-Q Plot of Pearson Residuals",
                                    x = "Theoretical Quantiles",
                                    y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_3.5), max(sample_quantiles_3.5))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_3.5), max(theoretical_quantiles_3.5)))  # Adjust x-limits


plot_data_3.5 <- data.frame(
  fitted = model_3.5$fitted,
  residuals.Pearson =model_3.5$residuals.Pearson,
  weights.y = model_3.5$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_3.5 <- ggplot(plot_data_3.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 8(5,6): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_3.5_zoom1 <- ggplot(plot_data_3.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_3.5_zoom2 <- ggplot(plot_data_3.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
library(ggrepel)
p_3.5_notscaled <- ggplot(plot_data_3.5, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_3.5, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 6(3,5): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()

####
# Extract data
y_3.5<-data$Counts.of.Victims 
y_hat_3.5<- round(model_3.5$fitted)
w_3.5<- model_3.5$weights.y
sum(model_3.5$weights.y==0)
# Weighted means
y_bar_w_3.5<- sum(w_3.5* y_3.5) / sum(w_3.5)
y_hat_bar_w_3.5<- sum(w_3.5* y_hat_3.5) / sum(w_3.5)

# Numerator and denominators for correlation
numerator_3.5<- sum(w_3.5* (y_3.5- y_bar_w_3.5) * (y_hat_3.5- y_hat_bar_w_3.5))
denominator_y_3.5<- sqrt(sum(w_3.5* (y_3.5- y_bar_w_3.5)^2))
denominator_y_hat_3.5<- sqrt(sum(w_3.5* (y_hat_3.5- y_hat_bar_w_3.5)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_3.5<- (numerator_3.5/ (denominator_y_3.5* denominator_y_hat_3.5))^2

# Ensure values are within [0, 1]
R2_corr_weighted_3.5<- max(0, min(1, R2_corr_weighted_3.5))
print(R2_corr_weighted_3.5)
###########################Model 7: Model (6,5) with lasso vars#####################################
set.seed(123)
model_6.5 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 6,
                        c.tukey.sigma = 5,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
residuals_pearson_6.5 <- model_6.5$residuals.Pearson
weights_y_6.5 <- model_6.5$weights.y
theta_6.5 <- model_6.5$coef[1]  # Dispersion parameter
mu_6.5 <- mean(model_6.5$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_6.5 <- length(residuals_pearson_6.5)

# Manually compute quantiles
sample_quantiles_6.5 <- sort(residuals_pearson_6.5)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_6.5 <- qnorm((1:n_6.5 - 0.5) / n_6.5) # Normal quantiles
# Combine into a data frame for plotting
# Combine into a data frame for plotting
qq_data_6.5  <- data.frame(
  theoretical = theoretical_quantiles_6.5,
  sample = sample_quantiles_6.5,
  weight = weights_y_6.5[order(residuals_pearson_6.5)],
  Country = data$Country[order(residuals_pearson_6.5)]
)

# Create weighted Q-Q plot using ggplot2
p_6.5 <- ggplot(qq_data_6.5, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_6.5, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 7(6,5):Weighted Q-Q Plot of Pearson Residuals",
                                    x = "Theoretical Quantiles",
                                    y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_6.5), max(sample_quantiles_6.5))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_6.5), max(theoretical_quantiles_6.5)))  # Adjust x-limits


plot_data_6.5 <- data.frame(
  fitted = model_6.5$fitted,
  residuals.Pearson =model_6.5$residuals.Pearson,
  weights.y = model_6.5$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_6.5 <- ggplot(plot_data_6.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 8(5,6): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_6.5_zoom1 <- ggplot(plot_data_6.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_6.5_zoom2 <- ggplot(plot_data_6.5, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
library(ggrepel)
p_6.5_notscaled <- ggplot(plot_data_6.5, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_6.5, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 7(6,5): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()

####
# Extract data
y_6.5<-data$Counts.of.Victims 
y_hat_6.5<- round(model_6.5$fitted)
w_6.5<- model_6.5$weights.y
sum(model_6.5$weights.y==0)
# Weighted means
y_bar_w_6.5<- sum(w_6.5* y_6.5) / sum(w_6.5)
y_hat_bar_w_6.5<- sum(w_6.5* y_hat_6.5) / sum(w_6.5)

# Numerator and denominators for correlation
numerator_6.5<- sum(w_6.5* (y_6.5- y_bar_w_6.5) * (y_hat_6.5- y_hat_bar_w_6.5))
denominator_y_6.5<- sqrt(sum(w_6.5* (y_6.5- y_bar_w_6.5)^2))
denominator_y_hat_6.5<- sqrt(sum(w_6.5* (y_hat_6.5- y_hat_bar_w_6.5)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_6.5<- (numerator_6.5/ (denominator_y_6.5* denominator_y_hat_6.5))^2

# Ensure values are within [0, 1]
R2_corr_weighted_6.5<- max(0, min(1, R2_corr_weighted_6.5))
print(R2_corr_weighted_6.5)
###########################Model 8: Model (5,6) with lasso vars#####################################
set.seed(123)
model_5.6 <- nb.glm.rob(y = y_a,
                         designX = designX,
                         offset = offset,
                         c.tukey.beta = 5,
                         c.tukey.sigma = 6,
                         weights.on.x = "none",
                         minsig = 0.001,
                         maxsig = 50,
                         minmu = 1e-10,
                         maxmu = 1e+20,
                         maxit = 50,
                         tol = 1e-05,
                         maxit.sig = 30,
                         tol.sig = 1e-06,
                         warn = FALSE)
####
residuals_pearson_5.6 <- model_5.6$residuals.Pearson
weights_y_5.6 <- model_5.6$weights.y
theta_5.6 <- model_5.6$coef[1]  # Dispersion parameter
mu_5.6 <- mean(model_5.6$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_5.6 <- length(residuals_pearson_5.6)

# Manually compute quantiles
sample_quantiles_5.6 <- sort(residuals_pearson_5.6)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_5.6 <- qnorm((1:n_5.6 - 0.5) / n_5.6) # Normal quantiles
# Combine into a data frame for plotting
# Combine into a data frame for plotting
qq_data_5.6  <- data.frame(
  theoretical = theoretical_quantiles_5.6,
  sample = sample_quantiles_5.6,
  weight = weights_y_5.6[order(residuals_pearson_5.6)],
  Country = data$Country[order(residuals_pearson_5.6)]
)

# Create weighted Q-Q plot using ggplot2
p_5.6 <- ggplot(qq_data_5.6, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_5.6, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 8(5,6):Weighted Q-Q Plot of Pearson Residuals",
                                    x = "Theoretical Quantiles",
                                    y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_5.6), max(sample_quantiles_5.6))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_5.6), max(theoretical_quantiles_5.6)))  # Adjust x-limits


plot_data_5.6 <- data.frame(
  fitted = model_5.6$fitted,
  residuals.Pearson =model_5.6$residuals.Pearson,
  weights.y = model_5.6$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_5.6 <- ggplot(plot_data_5.6, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 8(5,6): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_5.6_zoom1 <- ggplot(plot_data_5.6, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_5.6_zoom2 <- ggplot(plot_data_5.6, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
library(ggrepel)
p_5.6_notscaled <- ggplot(plot_data_5.6,aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_5.6, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 8(5,6): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()

####
# Extract data
y_5.6<-data$Counts.of.Victims 
y_hat_5.6<- round(model_5.6$fitted)
w_5.6<- model_5.6$weights.y
sum(model_5.6$weights.y==0)
# Weighted means
y_bar_w_5.6<- sum(w_5.6* y_5.6) / sum(w_5.6)
y_hat_bar_w_5.6<- sum(w_5.6* y_hat_5.6) / sum(w_5.6)

# Numerator and denominators for correlation
numerator_5.6<- sum(w_5.6* (y_5.6- y_bar_w_5.6) * (y_hat_5.6- y_hat_bar_w_5.6))
denominator_y_5.6<- sqrt(sum(w_5.6* (y_5.6- y_bar_w_5.6)^2))
denominator_y_hat_5.6<- sqrt(sum(w_5.6* (y_hat_5.6- y_hat_bar_w_5.6)^2))

# Robust Weighted Correlation R²
R2_corr_weighted_5.6<- (numerator_5.6/ (denominator_y_5.6* denominator_y_hat_5.6))^2

# Ensure values are within [0, 1]
R2_corr_weighted_5.6<- max(0, min(1, R2_corr_weighted_5.6))
print(R2_corr_weighted_5.6)
###########################Model 9: Model (2,10) with lasso vars#####################################
set.seed(123)
model_2.10 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 2,
                        c.tukey.sigma = 10,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
residuals_pearson_2.10 <- model_2.10$residuals.Pearson
weights_y_2.10 <- model_2.10$weights.y
theta_2.10 <- model_2.10$coef[1]  # Dispersion parameter
mu_2.10 <- mean(model_2.10$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_2.10 <- length(residuals_pearson_2.10)

# Manually compute quantiles
sample_quantiles_2.10 <- sort(residuals_pearson_2.10)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_2.10 <- qnorm((1:n_2.10 - 0.5) / n_2.10) # Normal quantiles
# Combine into a data frame for plotting
# Combine into a data frame for plotting
qq_data_2.10  <- data.frame(
  theoretical = theoretical_quantiles_2.10,
  sample = sample_quantiles_2.10,
  weight = weights_y_2.10[order(residuals_pearson_2.10)],
  Country = data$Country[order(residuals_pearson_2.10)]
)

# Create weighted Q-Q plot using ggplot2
p_2.10 <- ggplot(qq_data_2.10, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_2.10, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 9(2,10):Weighted Q-Q Plot of Pearson Residuals",
                                    x = "Theoretical Quantiles",
                                    y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_2.10), max(sample_quantiles_2.10))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_2.10), max(theoretical_quantiles_2.10)))  # Adjust x-limits
sum(model_2.10$weights.y==0)

plot_data_2.10 <- data.frame(
  fitted = model_2.10$fitted,
  residuals.Pearson =model_2.10$residuals.Pearson,
  weights.y = model_2.10$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_2.10 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 9(2,10): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_2.10_zoom1 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_2.10_zoom2 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
library(ggrepel)
p_2.10_notscaled <- ggplot(plot_data_2.10, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_2.10, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 9(2,10): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()
###########################Model 11: Model (2,2) with lasso vars#####################################
set.seed(123)
model_2.2 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 2,
                        c.tukey.sigma = 2,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
residuals_pearson_2.2 <- model_2.2$residuals.Pearson
weights_y_2.2 <- model_2.2$weights.y
theta_2.2 <- model_2.2$coef[1]  # Dispersion parameter
mu_2.2 <- mean(model_2.2$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_2.2 <- length(residuals_pearson_2.2)

# Manually compute quantiles
sample_quantiles_2.2 <- sort(residuals_pearson_2.2)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_2.2 <- qnorm((1:n_2.2 - 0.5) / n_2.2) # Normal quantiles
# Combine into a data frame for plotting
# Combine into a data frame for plotting
qq_data_2.2  <- data.frame(
  theoretical = theoretical_quantiles_2.2,
  sample = sample_quantiles_2.2,
  weight = weights_y_2.2[order(residuals_pearson_2.2)],
  Country = data$Country[order(residuals_pearson_2.2)]
)

# Create weighted Q-Q plot using ggplot2
p_2.2 <- ggplot(qq_data_2.2, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_2.10, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 11(2,2):Weighted Q-Q Plot of Pearson Residuals",
                                    x = "Theoretical Quantiles",
                                    y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_2.10), max(sample_quantiles_2.10))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_2.10), max(theoretical_quantiles_2.10)))  # Adjust x-limits


plot_data_2.2 <- data.frame(
  fitted = model_2.2$fitted,
  residuals.Pearson =model_2.2$residuals.Pearson,
  weights.y = model_2.2$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_2.10 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 8(5,6): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_2.10_zoom1 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_2.10_zoom2 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
sum(model_2.2$weights.y==0)
library(ggrepel)
p_2.2_notscaled <- ggplot(plot_data_2.2,aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_2.2, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 11(2,2): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()
###########################Model 10: Model (10,2) with lasso vars#####################################
set.seed(123)
model_10.2 <- nb.glm.rob(y = y_a,
                        designX = designX,
                        offset = offset,
                        c.tukey.beta = 10,
                        c.tukey.sigma = 2,
                        weights.on.x = "none",
                        minsig = 0.001,
                        maxsig = 50,
                        minmu = 1e-10,
                        maxmu = 1e+20,
                        maxit = 50,
                        tol = 1e-05,
                        maxit.sig = 30,
                        tol.sig = 1e-06,
                        warn = FALSE)
####
residuals_pearson_10.2 <- model_10.2$residuals.Pearson
weights_y_10.2 <- model_10.2$weights.y
theta_10.2 <- model_10.2$coef[1]  # Dispersion parameter
mu_10.2 <- mean(model_10.2$fitted)  # Mean of fitted values as an approximation of mu

# Number of observations
n_10.2 <- length(residuals_pearson_10.2)

# Manually compute quantiles
sample_quantiles_10.2 <- sort(residuals_pearson_10.2)
# Use qnbinom for NB theoretical quantiles (adjust probabilities to match sample size)
theoretical_quantiles_10.2 <- qnorm((1:n_10.2 - 0.5) / n_10.2) # Normal quantiles
# Combine into a data frame for plotting
# Combine into a data frame for plotting
qq_data_10.2  <- data.frame(
  theoretical = theoretical_quantiles_10.2,
  sample = sample_quantiles_10.2,
  weight = weights_y_10.2[order(residuals_pearson_10.2)],
  Country = data$Country[order(residuals_pearson_10.2)]
)

# Create weighted Q-Q plot using ggplot2
p_10.2 <- ggplot(qq_data_10.2, aes(x = theoretical, y = sample, size = weight)) +
  geom_point() +  # Use geom_point for manual control
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  geom_text(data = subset(qq_data_2.10, weight == 0), 
            aes(label =Country), 
            size = 1.5, 
            vjust = -1, 
            hjust = 0.5, 
            color = "black")+  labs(title = "Model 10(10,2):Weighted Q-Q Plot of Pearson Residuals",
                                    x = "Theoretical Quantiles",
                                    y = "Sample Quantiles (Residuals)") +
  scale_size_continuous(range = c(0.5, 3), name = "Weight") +  # Adjust point size by weight
  scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +  # Adjust transparency by weight
  theme_minimal() +
  coord_cartesian(ylim = c(min(sample_quantiles_2.10), max(sample_quantiles_2.10))) +  # Adjust y-limits
  coord_cartesian(xlim = c(min(theoretical_quantiles_2.10), max(theoretical_quantiles_2.10)))  # Adjust x-limits


plot_data_10.2 <- data.frame(
  fitted = model_10.2$fitted,
  residuals.Pearson =model_10.2$residuals.Pearson,
  weights.y = model_10.2$weights.y 
  ,y=y,
  Country=data$Country
  # or replace with your specific weights vector
)
actual_10.2 <- ggplot(plot_data_10.2, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 8(5,6): Comparison of Fitted and Actual Counts with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_2.10_zoom1 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 1000), xlim = c(0, 1000))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–1000) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
actual_2.10_zoom2 <- ggplot(plot_data_2.10, aes(x = fitted, y = y, size = weights.y, label = Country)) +
  geom_point(alpha = 0.8, color = "#BF3131") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#2C2C2C") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  coord_cartesian(ylim = c(0, 250), xlim = c(0, 250))+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 2(4,5): Comparison of Fitted and Actual Counts (Range 0–250) with Robust Weighting",
       x = "Fitted Values", y = "Observed Values") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 11)  # Use a readable size (e.g., 10–12)
  )
sum(model_10.2$weights.y==0)
library(ggrepel)
p_10.2_notscaled <- ggplot(plot_data_10.2, aes(x = fitted, y = residuals.Pearson, size = weights.y)) +
  geom_point(alpha = 0.6,color="#8A0000") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#2C2C2C") +
  geom_text_repel(data = subset(plot_data_10.2, weights.y == 0), 
                  aes(label = Country),
                  size = 2.5,              # Smaller text size for readability
                  angle = 45,              # Rotate labels for better fit
                  vjust = 1,               # Shift labels slightly above points
                  hjust = 0.5,             # Center horizontally
                  color = "#2C2C2c",         # Ensure contrast
                  box.padding = 0.5,       # Add padding around labels
                  max.overlaps = 10)+
  scale_size_continuous(range = c(1, 5), name = "Robust Weight") +
  labs(title = "Model 10(10,2): Residuals versus Fitted Values with Robust Weighting", 
       x = "Fitted Values", 
       y = "Pearson Residuals") +
  theme_minimal()
