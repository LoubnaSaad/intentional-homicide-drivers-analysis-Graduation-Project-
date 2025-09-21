# Beyond the Crime Scene: Key Drivers of Intentional Homicide

## Overview
Analyzed homicide counts across 108 countries using robust negative binomial regression in R, achieving 90% model fit accuracy. 
Handled overdispersion and outliers (sigma: 0.4–0.6) with advanced diagnostics, reducing data noise by 20%.
Interpreted model outputs for actionable policy insights, creating detailed reports.
Using models that aren’t published yet on R and create our godness of fit measures for M-estimation 

2021 cross-sectional analysis (107 countries) modeling victim counts/rates ~ socioeconomic factors. MAR imputation (mice CART), outliers (isolation forest kept genuine like Brazil), selection (Lasso), models (Poisson overdispersed, NB sensitive, robust NB stable with Tukey/IRLS). Sig: Crime ↑6.4%, unemployment² ↑0.17%, corruption ↓1.3%.

## Skills
- Prep: mice CART (density preserved), isolation forest (threshold 0.55-0.7, plots).
- EDA: ggpairs/cor, scatters/densities/bubbles (e.g., HDI-internet , unemployment-food-victims interactions).
- Modeling: nb.glm.rob (tunings 4/3-10/2), transformations (squared unemployment/food), offset log(pop).
- Tests: Wald/saddle/esaddle/wt.rob for sig/joint.
- Diags: Pearson/deviance chi-sq, pseudo R² (0.89), residuals/fitted plots (weighted, labeled outliers).

## Key Findings
- Model: log(rate) ~ unemployment² + crime + corruption + food² + temp.
- Overdispersion: 0.53 (robust).
- Policy: Economic stability/anti-corruption reduce homicides.

## Files 
- `eda_bivariate_analysis.R`: EDA with ggpairs, cor tests, scatters, densities, bubble plots.
- `outliers_detection_isolation_forest.R`: Outlier detection using isolation forest, with validation plots and summaries.
- `missing_data_imputation.R`: Missing data handling with MCAR test, mice CART imputation, pooled models, overdispersion checks.
- `graph Before and After Imputation.R`: Density plots comparing variables before/after imputation.
- `hypothesis_tests_robust_nb.R`: Hypothesis tests (Wald, saddlepoint, esaddle) for variable significance in robust NB models.
- `goodness_of_fit_diags.R`: Goodness-of-fit metrics like Pearson/deviance chi-sq, pseudo R², AIC/BIC.
- `robust_nb_model_setup.R`: Model parameters, joint tests, tilted exponential tilting, weighted residuals plots.
- `robust_nb_fits_nonscaled.R`: Robust NB fits (non-scaled/scaled), deviance calculations, pseudo R², basic residuals plots.
- `model_fits_and_plots.R`: Specific model tunings (e.g., 4/3, 10/2), advanced residuals/fitted plots with labels and log scales.

## Report
- [Full PDF](https://drive.google.com/file/d/1uQoTuSWt61VcrpG57D5HyhS4adTeuX8m/view?usp=drive_link)

## Data Sources
UNODC, World Bank, Numbeo, Transparency Int'l. 
