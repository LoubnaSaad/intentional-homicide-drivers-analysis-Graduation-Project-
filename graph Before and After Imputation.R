
library(readr)
df_before  <-read_csv("Data Before Any Imputation.csv")
df_After <-read_csv("phase1_missing2.csv")
colMeans(is.na(df_before))

df_before<-df_before[df_before$Country!="Egypt",]
df_After<- df_After[df_After$Country!="Egypt",]
colMeans(is.na(df_before))

# Unemployment

compare1 <- data.frame(
  value = c(df_before$Unemployment, df_After$Unemployment),
  source = c(
    rep("Original (with NA)", nrow(df_before)),
    rep("Imputed", nrow(df_After))
  )
)

ggplot(compare1, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original (with NA)" = "red", "Imputed" = "firebrick4"))+
  labs(title = "Unemployment Before Imputation vs Unemployment After Imputation",
       x = "Unemployment", y = "Density") +
  theme_minimal()

########################################

# Temperature

compare2 <- data.frame(
  value = c(df_before$Annual.Mean  , df_After$Annual.Mean),
  source = c(
    rep("Original (with NA)", nrow(df_before)),
    rep("Imputed", nrow(df_After))
  )
)
ggplot(compare2, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original (with NA)" = "red", "Imputed" = "firebrick4"))+
  labs(title = "Temperature Before Imputation vs Temperature After Imputation",
       x = "Temperature", y = "Density") +
  theme_minimal()

########################################

# Health Expenditure

compare3 <- data.frame(
  value = c(df_before$health.exp  , df_After$health.exp),
  source = c(
    rep("Original (with NA)", nrow(df_before)),
    rep("Imputed", nrow(df_After))
  )
)
ggplot(compare3, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original (with NA)" = "red", "Imputed" = "firebrick4"))+
  labs(title = "Health Expenditure Before Imputation vs Health Expenditure After Imputation",
       x = "Health Expenditure", y = "Density") +
  theme_minimal()



########################################

# Internet Rate

compare4 <- data.frame(
  value = c(df_before$internet.rate  , df_After$internet.rate),
  source = c(
    rep("Original (with NA)", nrow(df_before)),
    rep("Imputed", nrow(df_After))
  )
)
ggplot(compare4, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original (with NA)" = "red", "Imputed" = "firebrick4"))+
  labs(title = "Internet Rate Before Imputation vs Internet Rate After Imputation",
       x = "Internet Rate", y = "Density") +
  theme_minimal()

########################################

# Food insecurity

compare5 <- data.frame(
  value = c(df_before$Food.in , df_After$Food.insequire),
  source = c(
    rep("Original (with NA)", nrow(df_before)),
    rep("Imputed", nrow(df_After))
  )
)
ggplot(compare5, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original (with NA)" = "red", "Imputed" = "firebrick4"))+
  labs(title = "Food insecurity Before Imputation vs Food insecurity After Imputation",
       x = "Food insecurity", y = "Density") +
  theme_minimal()

########################################

# Crime

compare6 <- data.frame(
  value = c(df_before$Crime , df_After$Crime),
  source = c(
    rep("Original (with NA)", nrow(df_before)),
    rep("Imputed", nrow(df_After))
  )
)
ggplot(compare6, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original (with NA)" = "red", "Imputed" = "firebrick4"))+
  labs(title = "Crime Before Imputation vs Crime After Imputation",
       x = "Crime", y = "Density") +
  theme_minimal()

########################################

# Corruption Score

compare7 <- data.frame(
  value = c(df_before$CPI.score , df_After$CPI.score),
  source = c(
    rep("Original (with NA)", nrow(df_before)),
    rep("Imputed", nrow(df_After))
  )
)
ggplot(compare7, aes(x = value, fill = source)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Original (with NA)" = "red", "Imputed" = "firebrick4"))+
  labs(title = "Corruption Score Before Imputation vs Corruption Score After Imputation",
       x = "Corruption Score", y = "Density") +
  theme_minimal()

