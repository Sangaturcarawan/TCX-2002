#Tutorial 3 (needed for tutorial 4)

set.seed(123)

# 36 months: 2022-01 to 2024-12
months <- seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by = "month")
n <- length(months)

# Simple features
Holidays <- ifelse(format(months, "%m") %in% c("01","02","06","11","12"), 1, 0)  # SG retail-heavy months
Marketing_Spend <- 30000 + 400*(1:n) + 8000*Holidays + rnorm(n, 0, 3000)
Tourist_Arrivals <- 600 + 20*(1:n) + 80*sin(2*pi*(1:n)/12) + rnorm(n, 0, 40)     # in thousands

# Generate Sales with a simple linear relationship + noise
Sales <- 150000 + 1.8*Marketing_Spend + 220*Tourist_Arrivals + 40000*Holidays + rnorm(n, 0, 20000)

df <- data.frame(
  Month = months,
  Sales,
  Marketing_Spend,
  Tourist_Arrivals,
  Holidays = factor(Holidays) # as factor for easy interpretation
)

head(df)


train <- subset(df, Month < as.Date("2024-01-01"))
test  <- subset(df, format(Month, "%Y") == "2024")
nrow(train); nrow(test)


# Simple LR: Sales ~ Marketing_Spend
m_lr <- lm(Sales ~ Marketing_Spend, data = train)
summary(m_lr)

# Multiple LR: add Tourist_Arrivals and Holidays
m_mlr <- lm(Sales ~ Marketing_Spend + Tourist_Arrivals + Holidays, data = train)
summary(m_mlr)


rmse <- function(a,b) sqrt(mean((a-b)^2))

pred_train_lr  <- predict(m_lr,  train)
pred_train_mlr <- predict(m_mlr, train)
pred_test_lr   <- predict(m_lr,  test)
pred_test_mlr  <- predict(m_mlr, test)

cat("Train RMSE (LR): ", rmse(train$Sales, pred_train_lr),  "\n")
cat("Train RMSE (MLR):", rmse(train$Sales, pred_train_mlr), "\n")
cat("Test RMSE (LR):  ", rmse(test$Sales,  pred_test_lr),   "\n")
cat("Test RMSE (MLR): ", rmse(test$Sales,  pred_test_mlr),  "\n")

num <- subset(train, select = c(Sales, Marketing_Spend, Tourist_Arrivals))
round(cor(num), 2)

# install.packages("car") # run once if needed
library(car)
vif(m_mlr)
# Rule of thumb: VIF > 5 (or 10) may indicate problematic multicollinearity.

par(mfrow=c(2,2))
plot(m_mlr)   # Residuals vs Fitted, QQ plot, Scale-Location, Residuals vs Leverage
par(mfrow=c(1,1))











# Lesson 6 – Predictive Analytics II - MLR, Model complexity, Generalization, and Bias-Variance Tradeoff

# 1. Explore the data
# Scenario: Use the same data from Tutorial 3.
# Create scatterplot for “Sales vs Marketing Spend” & “Sales vs Tourists”.


pacman::p_load(tidyverse, corrplot, modelr, relaimpo, gridExtra)

# Sales vs Marketing Spend
p1 <- ggplot(df, aes(x = Marketing_Spend, y = Sales)) +
  geom_point(color = "red") +
  ggtitle("Sales vs. Marketing Spend") + 
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"), 
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")
  )

p2 <- ggplot(df, aes(x= Tourist_Arrivals, y = Sales)) +
  geom_point(color = "blue") +
  ggtitle("Sales vs. Tourist Arrivals") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")
  )

p1
p2


p3 <- ggplot(df, aes(x=Tourist_Arrivals, y=Sales)) +
  geom_point(color="darkblue")

p3

p4 <- ggplot(df, aes(x=Marketing_Spend, y=Sales)) +
  geom_point(color="red")

p4

# Combine the two plots side by side
grid.arrange(p1, p2, ncol = 2)

grid.arrange(p1, p2)

grid.arrange(p1, p2, nrow = 2)

grid.arrange(p3,p4,ncol=2)

# correlation between independent vars
cor(df[,c('Tourist_Arrivals', "Marketing_Spend")])

cor(df[,c('Tourist_Arrivals', 'Marketing_Spend')])

# plotting correlations
corrplot(
  cor(df[, sapply(df, is.numeric)], use="complete.obs"),
  method = "number", 
  type='lower'
)

corrplot(
  cor(df[,sapply(df, is.numeric)], use = "complete.obs"),
  method = "number"
)
names(df)

# What are your comments on the correlation plot?
# there might be multicollinearity between marketing and tourist (predictors) due to correlation
# may need to separate them

# 2. Simple Linear Regression, Multiple Linear Regression & RMSE

# build SLR
model1 <- lm(Sales ~ Marketing_Spend, data = df)
summary(model1)

m1 <- lm(Sales ~ Marketing_Spend, data = df)
summary(m1)

# R Code: Complete Assumption Check
model <- lm(Sales ~ Marketing_Spend, data = df)

# All-in-one diagnostic plots
par(mfrow = c(2, 2)) #splits plot to 4, mfrow is matrix fill by row
plot(model)
par(mfrow = c(1, 1)) #reset

# Interpretation
# Plot 1: Residuals vs Fitted - checks linearity & homoscedasticity, dots evenly spread out, residuals constant = homoscedastic
# Plot 2: Q-Q plot - checks normality: closer to line = normal
# Plot 3: Scale-Location - checks homoscedasticity
# Plot 4: Residuals vs Leverage - identifies outliers (outside cook's distance region)
# Formal tests

library(lmtest)
bptest(model) # Breusch-Pagan test for homoscedasticity (constant variance, constant residuals / errors ) H0, H1 is heteroscedastic
# p value > 0.05 fail to reject H0 => homoscedastic
#< 0.05 reject H0 => heteroscedastic

shapiro.test(residuals(model)) # Shapiro-Wilk test for normality
#w measures how close residuals are to normal
# p-value > 0.05 fail to reject H0 so it is normal

dwtest(model) # Durbin-Watson test for independence
#H0: residual are independent = no autocorrelation
#H1: current residuals are dependent on previous residuals, residuals are positively autocorrelated
#if H1, and data is time based, consider adding lag terms or time series regression models
#if not time based, check for data ordering or possible omitted variables causing autocorrelation
# DW ~ 2 residuals are independent = no autocorrelation
# < 2 positive autocorrelation
# > 2 negative autocorrelation
# range between 0 to 4
#correlation = measures relationship between two different variables, usually unordered
#autocorrelation = measures relationship between a variable (or residual) and itself
# at a previous time/sequence


# More than one predictor
# build MLR
model2 <- lm(Sales ~ Marketing_Spend + Tourist_Arrivals, data = df)
summary(model2)
#coefficients: b0, b1, b2... estimate intercept is b0 and the value of DV if IV are 0, base value, or y intercept
# b1, b2 is estimate 2 row onwards, measures gradient, 1 increase in IV = that amount of increase in DV

#Pr(>|t|) is the p value for each coefficient
# H0 beta = 0 (coefficient has no effect) insignificant
#H1 beta != 0 (coefficient has effect), statistically significant
#p value < 0.05  reject H0 = coefficient is statistically significant
#f statistic p value < 0.05 means model as a whole is statistically significant
#meaning reject null hypothesis for the model as a whole
# R squared shows percentage of DV explained by predictor(s)
#adjusted r squared is after accounting for weird shit
# high percentage means that a large proportion of variation in CV is explained by model
# this means it is a good fit
#residual standard error shows average size of prediction errors

#adding a predictor makes the model a better fit



# extracting some more info from the model object
# 1. Add predicted values to original data frame - one way
df <- df %>%
  add_predictions(model2)

# 2. add residuals to original data frame - one way
df <- df %>%
  add_residuals(model2)

df

# Plot residues; random residue with no pattern is better!
ggplot(df, aes(pred, resid)) +
  geom_point() +
  geom_ref_line(h = 0)

#homscedastic

# 3. RSqr. and Adj-RSqr.
summary(model2)$adj.r.squared
summary(model2)$r.squared
#percentage of variation or variance of DV that models / IV / predictors explains
#adjusted penalises extra predictors added, making a more realistic measure of model fit
#adjusted will be lower, more realistic, meaning the model explains a bit less of the variation



# Diagnostic plots for MLR
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))


# Residuals vs Fitted → check linearity & homoscedasticity
# 
# Normal Q-Q → check if residuals are normally distributed
# 
# Scale-Location (Spread vs Fitted) → check homoscedasticity
# 
# Residuals vs Leverage (Cook’s distance) → detect influential points/outliers

# Diagnostic plots guide:
# 
# 1. Residuals vs Fitted
#    - Want: random scatter around 0 (no pattern)
#    - Bad: curve → missing nonlinear term
#           funnel shape → heteroscedasticity
#           extreme points → outliers
#
# 2. Normal Q-Q
#    - Want: points along diagonal line
#    - Bad: strong curve at tails → non-normal residuals
#
# 3. Scale-Location (Spread vs Fitted)
#    - Want: horizontal band with equal spread
#    - Bad: funnel shape → heteroscedasticity
#           trend/pattern → variance not constant
#
# 4. Residuals vs Leverage (Cook’s Distance)
#    - Want: most points central, none too influential
#    - Bad: points far right → high leverage
#           points outside Cook’s distance lines → influential outliers
#
# Summary:
# 1: Linearity + homoscedasticity
# 2: Normality
# 3: Homoscedasticity again
# 4: Influential outliers



# Formal tests
bptest(model2) # Breusch-Pagan test for homoscedasticity
shapiro.test(residuals(model2)) # Shapiro-Wilk test for normality
dwtest(model2) # Durbin-Watson test for independence

# --- Model Assumption Tests for model2 ---

# 1. Breusch-Pagan test (homoscedasticity)
#    H0: Residuals have constant variance (homoscedastic)
#    H1: Residuals do not have constant variance (heteroscedastic)
#    Result: BP = 2.01, p = 0.3664 > 0.05
#    → Fail to reject H0 → residuals are homoscedastic (good).

# 2. Shapiro-Wilk test (normality of residuals)
#    H0: Residuals are normally distributed
#    H1: Residuals are not normal
#    Result: W = 0.91, p = 0.00657 < 0.05
#    → Reject H0 → residuals deviate from normality (not ideal).
#    Note: Large samples can still work fine (normality is less critical
#    if n > 30 and other assumptions hold). Check Q-Q plot for severity.

# 3. Durbin-Watson test (independence of residuals)
#    H0: Residuals are independent (no autocorrelation)
#    H1: Residuals are autocorrelated
#    Result: DW = 1.45, p = 0.0277 < 0.05
#    → Reject H0 → evidence of positive autocorrelation in residuals.
#    (Might happen with time-series data. Consider time-related predictors
#    or using time-series models like ARIMA if needed.)

# --- Summary ---
# ✔ Homoscedasticity holds
# ✘ Normality is violated (residuals not perfectly normal)
# ✘ Independence is violated (residual autocorrelation present)
#
# Next steps: 
# - Inspect residual plots for patterns.
# - If time-based → consider lag variables or time-series modeling.
# - If normality deviation is mild → may still proceed, as regression is
#   fairly robust to non-normal residuals (especially with n > 30).

# 1. Breusch-Pagan test
#    BP = test statistic for the Breusch-Pagan test
#    - It measures how much the variance of residuals depends on the predictors.
#    - Larger BP values → more evidence of heteroscedasticity.
#    - You compare BP against a chi-squared distribution with df = #predictors.

# 2. Shapiro-Wilk test
#    W = test statistic for the Shapiro-Wilk test
#    - It measures how close the residuals are to a normal distribution.
#    - W ranges from 0 to 1.
#      * W close to 1 → residuals are close to normal.
#      * W much smaller → residuals deviate from normality.

# 3. Durbin-Watson test
#    DW = test statistic for the Durbin-Watson test
#    - It measures autocorrelation in residuals (specifically lag-1 autocorrelation).
#    - DW ranges from 0 to 4:
#         ~2 → no autocorrelation (independent residuals, good)
#         <2 → positive autocorrelation (residuals are related, not independent)
#         >2 → negative autocorrelation








#personal




# Load required libraries
library(ggplot2)
library(gridExtra)
library(corrplot)
library(dplyr)
library(modelr)
library(lmtest)

# 1. Explore the data
# Scenario: Use the same data from Tutorial 3

# Scatterplot for Sales vs Marketing Spend
p1 <- ggplot(df, aes(x = Marketing_Spend, y = Sales)) +
  geom_point(color = "red") +
  ggtitle("Sales vs. Marketing Spend") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")
  )

# Scatterplot for Sales vs Tourist Arrivals
p2 <- ggplot(df, aes(x = Tourist_Arrivals, y = Sales)) +
  geom_point(color = "blue") +
  ggtitle("Sales vs. Tourist Arrivals") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")
  )

# Combine the two plots side by side
grid.arrange(p1, p2, ncol = 2)

# Correlation between independent variables
cor(df[, c('Tourist_Arrivals', "Marketing_Spend")])

# Correlation plot of all numeric variables
corrplot(
  cor(df[, sapply(df, is.numeric)], use = "complete.obs"),
  method = "number",
  type = 'lower'
)

# Comments: You should check if independent variables are highly correlated,
# because strong correlation may indicate multicollinearity.

# 2. Simple Linear Regression (SLR)
model1 <- lm(Sales ~ Marketing_Spend, data = df)
summary(model1)

# 3. Assumption check for SLR
par(mfrow = c(2, 2))  # 2x2 plotting layout
plot(model1)           # Diagnostic plots: Residuals vs Fitted, Q-Q, Scale-Location, Residuals vs Leverage
par(mfrow = c(1, 1))  # Reset plotting layout

# Formal statistical tests
bptest(model1)                     # Breusch-Pagan test for homoscedasticity
shapiro.test(residuals(model1))    # Shapiro-Wilk test for normality of residuals
dwtest(model1)                     # Durbin-Watson test for independence

# 4. Multiple Linear Regression (MLR)
model2 <- lm(Sales ~ Marketing_Spend + Tourist_Arrivals, data = df)
summary(model2)

# Extract predicted values and residuals and add to original data frame
df <- df %>%
  add_predictions(model2, var = "pred") %>%
  add_residuals(model2, var = "resid")

# Plot residuals vs predicted values
ggplot(df, aes(x = pred, y = resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Predicted Values") +
  theme_minimal()

# 5. R-squared and Adjusted R-squared
summary(model2)$r.squared
summary(model2)$adj.r.squared

# 6. Diagnostic plots for MLR
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# 7. Formal statistical tests for MLR
bptest(model2)                     # Breusch-Pagan test for homoscedasticity
shapiro.test(residuals(model2))    # Shapiro-Wilk test for normality
dwtest(model2)                     # Durbin-Watson test for independence


