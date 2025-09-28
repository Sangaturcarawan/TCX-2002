# TCX2002 Tutorial 3 - Predictive Analytics I: Linear Regression
# Lesson 5 – Predictive Analytics I – Introduction to Machine Learning and LR

# 1. Understanding Predictors and Outcome
# Scenario: You're predicting monthly sales for a small F&B outlet using a few intuitive predictors.

# Setup Data
set.seed(123)

# 36 months: 2022-01 to 2024-12
months <- seq(as.Date("2022-01-01"), as.Date("2024-12-01"), by = "month")
months
n <- length(months)
n

# Simple features
# Holidays: 1 for SG retail-heavy months (Jan, Feb, Jun, Nov, Dec), else 0
Holidays <- ifelse(format(months, "%m") %in% c("01","02","06","11","12"), 1, 0)

Holidays
class(Holidays)


# Marketing spend: increases over time + higher during holidays + random noise
Marketing_Spend <- 30000 + 400*(1:n) + 8000*Holidays + rnorm(n, 0, 3000)

# Tourist arrivals (in thousands): trend + seasonality + noise
Tourist_Arrivals <- 600 + 20*(1:n) + 80*sin(2*pi*(1:n)/12) + rnorm(n, 0, 40)

# Generate Sales with a simple linear relationship + noise
Sales <- 150000 + 1.8*Marketing_Spend + 220*Tourist_Arrivals + 40000*Holidays + rnorm(n, 0, 20000)

# Create data frame
df <- data.frame(
  Month = months,
  Sales = Sales,
  Marketing_Spend = Marketing_Spend,
  Tourist_Arrivals = Tourist_Arrivals,
  Holidays = factor(Holidays) # as factor for easy interpretation
)

df

# Question: What are the predictors and outcome (predicted response)?
# Outcome: Sales
# Predictors: Marketing_Spend, Tourist_Arrivals, Holidays

# 2. Train-Test Split
# Use 2022–2023 as train and 2024 as test
train <- subset(df, Month < as.Date("2024-01-01"))
test <- subset(df, format(Month, "%Y") == "2024")
train

# Check number of rows
nrow(train); nrow(test)

# 3. Correlation and Multicollinearity Check

# Check Correlation among numeric variables
num <- subset(train, select = c(Sales, Marketing_Spend, Tourist_Arrivals))
num
df
pearson_correlation <- round(cor(num), 2)
class(pearson_correlation)
typeof(pearson_correlation)
pearson_correlation

#cor() pearson correlation
# -1 perfect negative correlation
# 0 no linear relationship
# 1 perfect positive correlation

install.packages("car")
# VIF to check multicollinearity (needs car package)
# install.packages("car") # run once if needed
library(car)

vif(m_mlr)  # Rule of thumb: VIF > 10 may indicate problematic multicollinearity


# 4. Simple Linear Regression, Multiple Linear Regression & RMSE

# Simple LR: Sales ~ Marketing_Spend
m_lr <- lm(Sales ~ Marketing_Spend, data = train)
summary(m_lr)

# Multiple LR: add Tourist_Arrivals and Holidays
m_mlr <- lm(Sales ~ Marketing_Spend + Tourist_Arrivals + Holidays, data = train)
summary(m_mlr)

# RMSE function
rmse <- function(a,b) sqrt(mean((a-b)^2))
rmse
# Predictions
pred_train_lr <- predict(m_lr, train)
pred_train_mlr <- predict(m_mlr, train)
pred_test_lr <- predict(m_lr, test)
pred_test_mlr <- predict(m_mlr, test)

# Calculate RMSE
cat("Train RMSE (LR): ", rmse(train$Sales, pred_train_lr), "\n")
cat("Train RMSE (MLR):", rmse(train$Sales, pred_train_mlr), "\n")
cat("Test RMSE (LR): ", rmse(test$Sales, pred_test_lr), "\n")
cat("Test RMSE (MLR): ", rmse(test$Sales, pred_test_mlr), "\n")


num <- subset(train, select = c(Sales, Marketing_Spend, Tourist_Arrivals))
round(cor(num), 2)

# install.packages("car") # run once if needed
library(car)
vif(m_mlr)
# Rule of thumb: VIF > 5 (or 10) may indicate problematic multicollinearity.

par(mfrow=c(2,2))
plot(m_mlr)   # Residuals vs Fitted, QQ plot, Scale-Location, Residuals vs Leverage
par(mfrow=c(1,1))

