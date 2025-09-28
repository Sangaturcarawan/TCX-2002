### L5 - Tutorial 3 ###

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


# ======================================================================

### L6 - Tutorial 4 (continued) ###

pacman::p_load(corrplot, modelr, tidyverse, gridExtra, relaimpo)


# Basic scatter plot with base R
p1 <- ggplot(df, aes(x = Marketing_Spend, y = Sales)) +
  geom_point(color = "red") +
  ggtitle("Sales vs. Marketing Spend") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")
  )

p1


# Basic scatter plot with base R
p2 <- ggplot(df, aes(x = Tourist_Arrivals, y = Sales)) +
  geom_point(color = "blue") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "lightgray", linetype = "dotted"),
    panel.grid.minor = element_line(color = "lightgray", linetype = "dotted")
  )

p2


# Combine the two plots side by side
grid.arrange(p1, p2, ncol = 2)


#####################################################
# correlation between independent vars
cor(df[,c('Tourist_Arrivals', "Marketing_Spend")])

#####################################################
# plotting correlations 
corrplot(cor(df[, sapply(df, is.numeric)],
             use="complete.obs"), 
         method = "number", 
         type='lower')

names(df)


# build model-1
model1 = lm(Sales ~ Marketing_Spend, data = df)

# model summary

summary(model1)


# R Code: Complete Assumption Check 
model <- lm(Sales ~ Marketing_Spend, data = df) 
# All-in-one diagnostic plots 
par(mfrow = c(2, 2)) 
plot(model) 
par(mfrow = c(1, 1)) 
# Interpretation 
# Plot 1: Residuals vs Fitted - checks linearity & homoscedasticity 
# Plot 2: Q-Q plot - checks normality 
# Plot 3: Scale-Location - checks homoscedasticity 
# Plot 4: Residuals vs Leverage - identifies outliers 
# Formal tests 
library(lmtest) 
bptest(model) # Breusch-Pagan test for homoscedasticity 
shapiro.test(residuals(model)) # Shapiro-Wilk test for normality 
dwtest(model) # Durbin-Watson test for independence


#####################################################
# More than one predictors
# build model-2
model2 = lm(Sales ~ Marketing_Spend + Tourist_Arrivals, data = df)

#####################################################
# model summary
summary(model2)

## extracting some more info from the model object
## 1. Add predicted values to original data frame
# one way 
df = df%>%
  add_predictions(model2)

# another way to just view
# model$fitted.values

## 2. add residuals to original data frame
# one way 
df = df%>%
  add_residuals(model2)
# another way to just view
# model$residuals

#Plot residues; random residue with no pattern is better!
ggplot(df, aes(pred, resid)) +
  geom_point()+geom_ref_line(h = 0)

# 3. RSqr. and Adj-RSqr.
summary(model2)$adj.r.squared

summary(model2)$r.squared

par(mfrow = c(2, 2)) 
plot(model2) 
par(mfrow = c(1, 1))
bptest(model2) # Breusch-Pagan test for homoscedasticity 
shapiro.test(residuals(model)) # Shapiro-Wilk test for normality 
dwtest(model2) # Durbin-Watson test for independence
####### END #########



