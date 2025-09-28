# National University of Singapore
# TCX2002 Introduction to Business Analytics
# Tutorial 4 – Predictive Analytics II
# Lesson 6 – MLR, Model complexity, Generalization, and Bias-Variance Tradeoff

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
