###### Graduation Project       


# Library 

library(readr)
library(mice)
library(psych)
library("GGally")
library(missForest) 
library(wbacon)
library(ggplot2)
library(dplyr)
library(purrr)
library(ggplot2)
library(psych)
library(BaylorEdPsych)
library(MASS)      
library(sandwich)   
library(lmtest) 
library(AER)
#  1---- Clean Data ----

df  <- read.csv("dftota22l.;;;;;;;;;;;;;;;csv.csv")

df$Country<-as.factor(df$Country)
colnames(df)
levels(df$Country)
str(df)
colMeans(is.na(df))
df_new<-df[,-c(13,14,15,16,17,18,19)]
# Missing                      
colMeans(is.na(df_new))
LittleMCAR(df)
# missingness indicator for a variable
df_new$missing_crime <- is.na(df_new$Crime)
colnames(df_new)
vv<-glm(missing_crime ~ GDP + Unemployment + internet.rate, data = df_new, family = "binomial")
summary(vv)
colnames(df_new[,c(-1,-2,-9,-10,-17)])
imputed_data <- mice(df_new[,c(-1,-2,-9,-10,-12,-17)], method = "cart", m = 5, maxit = 100, seed = 123)

plot(imputed_data)
densityplot(imputed_data) 

pooled_check <- with(imputed_data, lm(Unemployment ~ Crime+ Food.insequire+Annual.Mean+ health.exp+Corruption.Rate + internet.rate ))
summary(pool(pooled_check))
long_data <- complete(imputed_data, action = "long", include = TRUE)

imputed_only <- filter(long_data, .imp != 0)

final_dataset <- imputed_only %>%
  group_by(.id) %>%
  summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))



compare_df <- data.frame(
  value = c(df_new$health.exp  , final_dataset$health.exp  ),
  source = c(
    rep("Original (with NA)", nrow(df_new)),
    rep("Imputed", nrow(final_dataset))
  )
)

ggplot(compare_df, aes(x = value, fill = source)) +
  geom_density(alpha = 0.25) +
  labs(title = "Density Plot: Unemployment   Before vs After Imputation",
       x = "Unemployment  ", y = "Density") +
  theme_minimal()


non_imputed_part <- df_new %>%
  mutate(.id = row_number()) %>%
  select(c(.id, 1, 2, 9, 10, 12, 17))


non_imputed_part <- df_new %>%
  mutate(.id = row_number()) %>%
  select(.id, column_name1, column_name2, ...)  # replace with actual names
names(df_new)[c(1, 2, 9, 10, 12, 17)]



non_imputed_part <- df_new %>%
  mutate(.id = row_number()) %>%
  select(.id, Country, Country.Code, Victims.rate, Victims.Counts, Gini.coefficient)






write.csv(final_dataset, "phase1_missing2.csv")
write.csv()

final_dataset<-read.csv("phase1_missing2.csv")
mean_value <- mean(df$Victims.Counts,na.rm=T)
variance_value <- var(df$Victims.Counts,na.rm=T)
dispersion_ratio <- variance_value / mean_value
str(df)
print(dispersion_ratio)
colnames(final_dataset)
predicators<-final_dataset[,c(-1,-11)]
nb_model <- glm.nb(df$Victims.Counts ~ Annual.Mean+Unemployment+health.exp+internet.rate+
                     GDP+ schooling.years+Food.insequire+Corruption.Rate+Crime   , data = final_dataset)
    
#########models

df_no<-read.csv("data with imputed values.csv")
df_no$Population..2021.<-gsub(",","",df_no$Population..2021.)

df_no$Population..2021.<-as.numeric(df_no$Population..2021.)
colnames(df_no)
#1
str(df_no)
poisson_model <- glm(counts.of.Victims ~offset(log(df_no$Population..2021.))
                     +internet.rate + health.exp+Annual.Mean + schooling.years+Unemployment+GDP+Food.insequire+Corruption.Rate+Crime, family = poisson(link = "log"), data = df_no)




summary(poisson_model)
# overdispersion value 
dispersion_ratio <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion_ratio
#test
dispersiontest(poisson_model)
vcov_matrix <- vcov(model)
coef_variances <- diag(vcov_matrix)
mu_hat <- predict(model, type = "response")
dispersion <- deviance(model) / df.residual(model)
adjusted_variances <- dispersion * mu_hat
plot(mu_hat, adjusted_variances,
     xlab = "Predicted Mean (mu_hat)",
     ylab = "Estimated Variance",
     main = "Predicted Mean vs Variance",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2) # y = x line
#2
nb_model <- glm.nb(counts.of.Victims ~offset(log(df_no$Population..2021.))
                   +internet.rate + health.exp+Annual.Mean + schooling.years+Unemployment+GDP+Food.insequire+Corruption.Rate+Crime,data = df_no)
summary(nb_model)
# sandwich
robust_se <- sqrt(diag(vcovHC(poisson_model, type = "HC3"))) 
robust_se2 <- vcovHC(poisson_model, type = "HC0") 

summary(robust_se)



coeftest(nb_model, vcov = robust_se)
  #comparing 
AIC(poisson_model, nb_model)

# Load the 'sandwich' package for robust standard errors
library(sandwich)

# Calculate the robust covariance matrix using vcovHC
robust_cov_matrix <- vcovHC(poisson_model, type = "HC3")

# Run coeftest() with the robust covariance matrix
library(lmtest)
robust_test_results <- coeftest(poisson_model, vcov = robust_cov_matrix)

# Print the results
print(robust_test_results)
str(nb_model)


library(sandwich)
library(lmtest)

robust_se <- vcovHC(nb_model, type = "HC0")
coeftest(nb_model, vcov = robust_se)








