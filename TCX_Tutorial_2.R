# National University of Singapore
# TCX2002 Introduction to Business Analytics
# Tutorial 2
# Lesson 4 - Preparing and Comparing Data: t-tests, ANOVA, and Hypothesis Testing

library(tidyverse)
library(dplyr)
library(lubridate)
library(moments)
library(car)


# 1. One-Sample t-test
# Scenario: A coffee chain claims that its average daily sales is $500, in the past month.
# You are asked to verify this.
# Dataset:
# A vector of 30 daily sales figures:
sales <- c(595, 447, 477, 423, 570, 547, 541, 490, 593, 557, 533, 482, 516, 590,
           476, 499, 419, 509, 430, 479, 502, 511, 496, 471, 554, 596, 485, 534, 414, 443)
# 1. 3. State the null and alternative hypotheses.

mean_sales <- mean(sales)
mean_sales

# Null Hypothesis: The true mean daily sales is $500
# H0: mu = 500
# Alternative Hypothesis: The true mean daily sales is not $500
# H1: mu != 500

# 2. Use t.test(sales, mu = 500) to test the claim.
# What is the p-value? Do you reject or fail to reject the null hypothesis?

?t.test
t.test(sales, mu=500)
length(sales)

# Since p-value >0.05, fail to reject the null hypothesis. = don't reject
# p < 0.05 reject, more than enough evidence to reject


# 2. Paired Sample t-test
# Scenario: A 10-day marketing campaign was launched. You want to test whether customer
# online visits to the online shop increased during the campaign vs the 10 days before.

before <- c(125, 153, 148, 113, 139, 123, 106, 146, 119, 172)
after <- c(140, 165, 154, 122, 148, 194, 164, 152, 200, 197)

# 1. Run a paired t-test using t.test(before, after, paired = TRUE).
# H0 no increase: mu after - mu before = 0
# H1 increase in visits: mu after - before > 0

t.test(before, after, paired=TRUE)
?t.test
shapiro.test(before)
shapiro.test(after)
#W = test statistic, = 1 means perfectly normal
#p value probability of observing your data if it were normal
#p value > 0.05 fail to reject null h (data is normally distributed)


# 2. Interpret the confidence interval and p-value.
# 3. What business decisions might be made from this result?

# 3. ANOVA: Comparing More Than Two Groups
# Scenario: A manager wants to compare average satisfaction scores (about of 5 star rating)
# across 3 branches: North, South, and Central, during the past 4 weeks of summer holidays.

branch <- factor(c(rep("North",4), rep("South",4), rep("Central",4)))
score <- c(3.2, 3.5, 3.7, 3.2, 4.0, 4.2, 3.9, 3.6, 3.7, 3.6, 3.4, 3.9)
branch
score
# 1. Run aov(score ~ branch) and check summary().

aovmodel <- aov(score ~ branch)
aovmodel
summary(aovmodel)

boxplot(score ~ branch, 
        main = "Scores by Branch", 
        xlab = "Branch", 
        ylab = "Score", 
        col = c("lightblue", "lightgreen", "lightpink"))
points(tapply(score, branch, mean), col="red", pch=19)

# 2. 3. What is the F-statistic and p-value?
# What do the results imply for how customers perceive service across branches?

# 4. Comparing weight loss in diet plans
# Scenario: A nutritionist wants to compare the average weight loss (in kg) for three
# different diet plans (A, B, and C) over a month. She randomly assigns 5 people to
# each plan. Their weight losses are:

diet <- factor(rep(c("A", "B", "C"), each = 5))

loss <- c(2.1, 2.5, 2.0, 2.9, 2.4, # Diet A
          3.0, 3.2, 2.8, 3.5, 3.1, # Diet B
          1.8, 2.0, 2.2, 1.9, 2.1) # Diet C

modelaov <- aov(loss ~ diet)
modelaov
summary(modelaov)

# Boxplot of weight loss by diet
boxplot(loss ~ diet,
        main = "Weight Loss by Diet Plan",
        xlab = "Diet Plan",
        ylab = "Weight Loss (kg)",
        col = c("lightblue", "lightgreen", "lightpink"))

# Add mean points in red
points(tapply(loss, diet, mean), col="red", pch=19)


# 1. 2. Is there a significant difference in mean weight loss among the three diets?
# Report the F-statistic and p-value.
# 3. What is your conclusion

# 5. Risk management software in financial trading
# Scenario: A financial analyst wants to determine if a new risk management software has
# reduced the number of trading errors in a brokerage firm. She records the number of errors
# made by 10 traders in the month before and after the software was implemented:

before <- c(8, 6, 7, 9, 10, 5, 8, 7, 6, 9)
after <- c(5, 4, 5, 6, 8, 3, 6, 5, 4, 6)

# 1. 2. Has the risk management software significantly reduced trading errors
# Provide the p-value and interpret the result.

t.test(before, after, paired=TRUE, alternative="greater")
# alternative hypothesis is greater meaning mean before - mean after > 0
#on average, error decreased by mean difference
