library(parallel)
detectCores() 
library(isotree)
library(dplyr)
library(ggplot2)

# Set seed for reproducibility
set.seed(42)
set.seed(123)  # Try a different seed

# Load your data (adjust path if needed)
data <- read.csv("phase1_missing2.csv")
data<-na.omit(data)
colnames(data)
names(data)[names(data) == "Food.insequire"] <- "Food.insec"
x_vars <- c( "Unemployment", "health.exp", "internet.rate", 
            "Food.insec", "HDI","rural", "GDP")
data_x <- data[, x_vars]

# Standardize the features (mean = 0, sd = 1)
data_scaled <- as.data.frame(scale(data_x))
# Apply Isolation Forest
iso_model <- isolation.forest(
  data_scaled,
  ntrees = 200,           # Number of trees
  sample_size = 107,      # Use all 107 countries per tree
  nthreads = detectCores(),           # Use single thread for simplicity
  scoring_metric = "depth" # Use path length for scoring
)

# Predict anomaly scores
scores <- predict(iso_model, data_scaled)

# Add scores and outlier flags to original data
data$Anomaly_Score <- scores
data$Outlier <- ifelse(scores > 0.56, "Outlier", "Normal") # Threshold at 0.6
library(dplyr)
# Print outliers (countries with score > 0.6)
outliers <- data %>%
  filter(Outlier == "Outlier") %>%
  dplyr::select(Country, Anomaly_Score, dplyr::all_of(x_vars))

print("Outlier Countries:")
print(outliers)

# Save outliers to a CSV file
write.csv(outliers, "outliers_isoforest.csv", row.names = FALSE)

# Validation 1: Scatter plot of Unemployment vs. HDI
ggplot(data, aes(x = Unemployment, y = HDI, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in Unemployment vs. HDI",
       x = "Unemployment (%)", y = "HDI") +
  theme_minimal()
ggsave("outliers_plot_unemployment_hdi.png")
# Validation 1: Scatter plot of Unemployment vs. HDI
ggplot(data, aes(x = Annual.Mean, y = Crime, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in Annual.Mean vs. Crime",
       x = "Annual.Mean (%)", y = "Crime") +
  theme_minimal()
ggsave("outliers_plot_Annual.Mean_Crime.png")
# Validation 1: Scatter plot of Unemployment vs. HDI
ggplot(data, aes(x = Crime, y = internet.rate, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in Crime vs. internet.rate",
       x = "Crime", y = "internet.rate") +
  theme_minimal()
ggplot(data, aes(x = GDP, y = Food.insec, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in GDP vs. Food.insec",
       x = "GDP (%)", y = "Food.insec") +
  theme_minimal()

ggplot(data, aes(x = GDP, y = CPI.score, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in GDP vs CPI.score",
       x = "GDP", y = "CPI.score") +
  theme_minimal()
ggplot(data, aes(x = GDP, y = health.exp, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in GDP vs health.exp",
       x = "GDP", y = "health.exp") +
  theme_minimal()
ggplot(data, aes(x = GDP, y = Unemployment, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in GDP vs Unemployment",
       x = "GDP", y = "Unemployment ") +
  theme_minimal()

ggplot(data, aes(x = HDI, y = health.exp, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in GDP vs Health expenduire",
       x = "GDP", y = "Health expenduire ") +
  theme_minimal()


ggplot(data, aes(x = rural, y = Food.insec, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in rural vs Food insecure",
       x = "rural", y = "Food.insec") +
  theme_minimal()
colnames(data)
# Set seed
# Print top 10 countries by score
top_scores <- data %>% 
  arrange(desc(Anomaly_Score)) %>% 
  dplyr::select(Country, Anomaly_Score, dplyr::all_of(x_vars))

print("Top 10 Countries by Anomaly Score:")
print(top_scores[1:10, ])

# Print outliers
outliers <- data %>% 
  filter(Outlier == "Outlier") %>% 
  dplyr:: select(Country, Anomaly_Score, dplyr::all_of(x_vars))
print("Outliers with Threshold 0.55:")
print(outliers)

# Save outliers
write.csv(outliers, "outliers_isoforest_updated.csv", row.names = FALSE)

# Scatter plot for validation
ggplot(data, aes(x = Crime, y = Food.insec, color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "black") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest Outliers: Crime vs. Food insecure",
       x = "Crime ", y = "Food.insec") +
  theme_minimal()
ggsave("outliers_plot_updated.png")


# Validation 1: Scatter plot of Unemployment vs. HDI
ggplot(data, aes(x = Crime, y = log(Victims.Counts), color = Anomaly_Score, shape = Outlier)) +
  geom_point(size = 3) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_text(aes(label = ifelse(Outlier == "Outlier", Country, "")), vjust = -1, size = 3) +
  labs(title = "Isolation Forest: Outliers in Crime vs. Victims.Counts",
       x = "Crime", y = "Victims.Counts") +
  theme_minimal()
ggsave("outliers_plot_Crime_Victims.Counts.png")
# Validation 2: Histogram of anomaly scores
ggplot(data, aes(x = Anomaly_Score)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.8) +
  geom_vline(xintercept = 0.55
             , color = "black", linetype = "dashed") +
  labs(title = "Distribution of Anomaly Scores", x = "Anomaly Score", y = "Count") +
  theme_minimal()
ggsave("scores_histogram.png")

# Validation 3: Statistical summary of outliers vs. normal
summary_outliers <- outliers %>% summarise_all(mean, na.rm = TRUE)
summary_normal <- data %>% filter(Outlier == "Normal") %>% select(all_of(x_vars)) %>% summarise_all(mean, na.rm = TRUE)
print("Outlier Means:")
print(summary_outliers)
print("Normal Means:")
print(summary_normal)

# Validation 4: Sensitivity analysis with different threshold
data$Outlier_0.7 <- ifelse(scores > 0.7, "Outlier", "Normal")
outliers_0.7 <- data %>% 
  filter(Outlier_0.7 == "Outlier") %>% 
  select(Country, Anomaly_Score, all_of(x_vars))
print("Outliers with Threshold 0.7:")
print(outliers_0.7)

# Validation 5: Subsampling for robustness
iso_model_sub <- isolation.forest(
  data_scaled,
  ntrees = 100,
  sample_size = 107, # Subsample 60% of data
  nthreads = 1,
  scoring_metric = "depth"
)
scores_sub <- predict(iso_model_sub, data_scaled)
data$Subsample_Score <- scores_sub
data$Subsample_Outlier <- ifelse(scores_sub > 0.59, "Outlier", "Normal")
outliers_sub <- data %>% 
  filter(Subsample_Outlier == "Outlier") %>% 
  dplyr::select(Country, Subsample_Score, dplyr::all_of(x_vars))
print("Outliers with Subsampling (sample_size = 64):")
print(outliers_sub)



