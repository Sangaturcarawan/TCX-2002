# Load required libraries
library(tidyverse)  # Data manipulation and visualization
library(dplyr)      # Data wrangling
library(lubridate)  # Date handling
library(moments)    # Skewness calculation
library(car)        # Levene's test for ANOVA

### Question 1 ###

sales <- c(595, 447, 477, 423, 570, 547, 541, 490, 593, 557, 533, 482, 516, 590, 476, 499, 
           419, 509, 430, 479, 502, 511, 496, 471, 554, 596, 485, 534, 414, 443) 

mean(sales)

t.test(sales, mu = 500)
?t.test
# Since p-value >0.05, fail to reject the null hypothesis.

# Null Hypothesis: μ = 500
# Alternative Hypothesis: μ ≠ 500

# The company’s claim that the average sales of $500 seems reasonable on this sample. 
# No statistically significant evidence against it.


### Question 2 ###

before <- c(125, 153, 148, 113, 139, 123, 106, 146, 119, 172) 
after <- c(140, 165, 154, 122, 148, 194, 164, 152, 200, 197)

t.test(before, after, paired = TRUE)


# Normality for each group
# Shapiro–Wilk per group => ensures t-test's normality assumption is reasonable.
shapiro.test(before)
shapiro.test(after)

#Conclusion:
# There is statistically significant evidence to suggest that customer visits increased 
# during the campaign compared to before.

#Business Decisions:
# Effectiveness: The marketing campaign was effective in increasing online shop visits. 
# Repeat/Scale: Consider repeating or scaling up similar campaigns.
# Deeper Analysis: Investigate which campaign elements contributed most and optimize further.
# Budget Justification: Data supports further investment in marketing.
# Long-Term Tracking: Continue to monitor if the impact is sustained or temporary.


### Question 3 ###

branch <- factor(c(rep("North",4), rep("South",4), rep("Central",4))) 
score <- c(3.2, 3.5, 3.7, 3.2, 4.0, 4.2, 3.9, 3.6, 3.7, 3.6, 3.4, 3.9) 

#Visualize
boxplot(score ~ branch, 
        main = "Boxplot of Scores by Branch", 
        xlab = "Branch", 
        ylab = "Score", 
        col = c("lightblue", "lightgreen", "lightpink"))

fit <- aov(score ~ branch)
summary(fit)

# Inference:
# F = 4.99; the differences between group means are about 4.99 times larger than what we’d expect from random within-group variability.
# p = 0.0348 (< 0.05)
# Strong evidence that not all regions have the same mean sales.
# We reject H0 (μ_North = μ_South = μ_Central)


# What do the results imply for how customers perceive service across branches? 

diet <- factor(rep(c("A", "B", "C"), each = 5))
loss <- c(2.1, 2.5, 2.0, 2.9, 2.4,   # Diet A
          3.0, 3.2, 2.8, 3.5, 3.1,   # Diet B
          1.8, 2.0, 2.2, 1.9, 2.1)   # Diet C

anova_result <- aov(loss ~ diet)
summary(anova_result)
