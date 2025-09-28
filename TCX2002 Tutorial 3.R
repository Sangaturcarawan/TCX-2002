# National University of Singapore
# TCX2002 Introduction to Business Analytics

# Tutorial 1

# Lesson 3 - Understanding Distributions, Sampling, and Estimation

# 1. Probability Distribution,
# 2. The Normal Curve;
# 3. Population vs Sample,
# 4. Sampling & Estimation (simple random, systematic, stratified, and convenience sampling),
# 5. Sampling error vs bias,
# 6. Point estimation
# 7. STD Error (SE) & Confidence Intervals

rm(list = ls())
library(ggplot2)

set.seed(123) #produces a reproducible set of random values 
#if you want two vectors to have the same boolean values for examples
# you need to run set.seed(same_number) before each variable assignment or vector c()

# 1. Simulating Randomness and Outcome Space
# Lab Questions:
#   1. Simulate 100 coin tosses in R using sample(c("Heads", "Tails"), 100, replace = TRUE).
choices <- c("heads", "tails")
tosses <- sample(choices, 100, replace=TRUE)
tosses

#   2. Count how many times "Heads" appears. What if you increase to 1000 coin tosses or more?
tosses_heads <- sum(tosses == "heads")
tosses_heads
tosses_1000 <- sample(choices, 1000, replace=TRUE) #generates a vector based on a shorter vector that has things you wanna repeat multiple times
#replace=TRUE means that the choices can be repeated, if you put replace=FALSE 
# and the number to repeat is less that the number of choices, then you will get an error

table(tosses)
prop.table(table(tosses))
table(tosses)["heads"]

table(tosses_1000) # gives the number of each type of thing
prop.table(table(tosses_1000)) #gives proportion of each result
table(tosses_1000)["heads"]

barplot(table(tosses), main="Coin Toss Results - 100", col=c("red","pink"))
barplot(table(tosses_1000), main="Coin Tosses - 1000", col=c("blue","violet"))
# table(), main= title, col=c(color char vector)

plot(table(tosses)) #this is the bare version of barplot

class(barplot(table(tosses_1000), main="Coin Tosses - 1000", col=c("blue","violet")))
#matrix array

class(plot(table(tosses)))

length(tosses)

mean(tosses=="heads")
tosses=="heads" #generates a logical vector and assigns 
#TRUE for heads and FALSE for everything else
# note it is a logical expression or logical vector
#it is a conditional logical vector generator applied to another vector


sum(tosses=="heads") / length(tosses) #equivalent to mean() for logical vectors only
is.vector(tosses=="heads")
length(tosses=="heads") #counts total number of items in logical vector extracted from char vector
class(tosses=="heads") #logical

cumsum(tosses=="heads") #cumulative sum

round(prop.table(table(tosses=="heads")),3)

#ggplot2
ggplot(data.frame(tosses), aes(x=tosses, fill=tosses)) + geom_bar() #bare minimum
ggplot(data.frame(tosses), aes(x=tosses, fill=tosses)) + geom_bar() + labs(title="Coin tosses -")
ggplot(data.frame(tosses), aes(x=tosses, fill=tosses)) + geom_bar() + labs(title="Coin Tosses - 100") + scale_fill_manual(values = c("blue", "red"))

#   3. Is the outcome approximately 50-50? What does this tell you about randomness?
# almost but not so, it fluctuates above and below it and approaches it as sample size increase
# random outcomes fluctuate
# law of large numbers, greater sample size, proportion of outcome approaches true probability


# 2. Exploring Sampling Techniques
# Scenario: Your company wants to survey customers.
# Lab Questions:
  # 1. Create a dummy data set with 500 customers and assign them to regions (North, South, East, West)

rm(list = ls())

set.seed(42)

regions <- c("North",
             "South",
             "East",
             "West")
regions

customer_id <- 1:500
customer_id

customer_region = sample(regions,
                500,
                replace=TRUE)
customer_region

customers_df <- data.frame(
  customer_id = customer_id,
  region = customer_region)
customers_df
head(customers_df)
head(customers_df, 10)

customers_df$region

  # 2. Perform:
    # o Simple random sampling
nrow(customers_df) # number of rows in a data.frame()
1:nrow(customers_df) # sequence operator from 1 till the total number of rows
sample(1:nrow(customers_df),50) # generates a vector of 50 random row numbers without replacement

srs_idx <- sample(1:nrow(customers_df),50) #sample() picks without replacement by default
#default is replacement=FALSE
srs_smp <- customers_df[srs_idx, ]
srs_smp
class(srs_idx)
typeof(srs_idx)
class(srs_smp)
typeof(srs_smp)

#using dplyr
library(dplyr)
simple_sample <- customers_df %>% # feeds data.frame into first argument of next function
  sample_n(size = 100)

simple_sample
table(simple_sample$region) #frequency distribution

#using base R
srs <- customers[sample(1:500, 100), ] #default replace=FALSE
table(srs$region)

#alternative official solutions
set.seed(42)
customers <- data.frame(
  customer_id=1:500,
  region=sample(c("North","South","East","West"),size=500,replace=TRUE)
)
head(customers)
table(customers)
table(customers$region)

library(dplyr)
simple_sample <- customers %>%
  sample_n(size = 100)

simple_sample
table(simple_sample$region)




    # o Stratified sampling (by region)
set.seed(42)
split_by_region <- split(customers_df, customers_df$region) #creates a list of df
split_by_region

#base r way is too long and complicated, just use dplyr

library(dplyr)

strat_smp <- customers_df %>%
  group_by(region) %>%
  sample_n(25) %>%
  ungroup() #not necessary here unless you wanna do further stuff on it

table(strat_smp$region)
strat_smp

#alternative
strat_smp <- customers_df %>%
  group_by(region) %>%
  sample_frac(0.20)
  
table(strat_smp$region)    

#alternative and newer way to do it

strat_smp <- customers_df %>%
  group_by(region) %>%
  slice_sample(prop=0.2) %>%
  ungroup()


table(strat_smp$region)

# o Systematic sampling (every 10th customer)

N <- nrow(customers_df)
n <- 100
k <- N/n
k
start <- sample(1:k, 1) #pick a random starting index
sys_idx <- seq(start, N, by = k)
sys_smp <- customers_df[sys_idx,]
sys_smp



###- Systematic sampling - (Dr Prakash slides)
N <- nrow(customers)   # population size (500)
n <- 100                # desired sample size
k <- N / n             # interval (10 here)
start <- sample(1:k, 1)  # random start between 1 and k
sys_idx <- seq(start, N, by = k)

systematic_sample <- customers[sys_idx, ]

# show samples divided by region
table(systematic_sample$Region)


  # 3. Compare the results: Are all regions equally represented in each technique?

original_distribution <- table(customers_df$region)
simple_distribution <- table(simple_sample$region)
stratified_distribution <- table(strat_smp$region)
systematic_distribution <- table(sys_smp$region)

original_distribution
simple_distribution
stratified_distribution
systematic_distribution

comparison <- data.frame(
  Original = original_distribution,
  Simple = simple_distribution,
  Stratified = stratified_distribution,
  Systematic = systematic_distribution
)

comparison
colnames(comparison) <- c("Region", "Original", "Region", "SimpleRandom", "Region", "Stratified", "Region", "Systematic")
comparison
print(comparison)


# 3. Confidence Intervals and Interpretation
# Scenario: A manager wants to know the average time to resolve customer complaints.

# Lab Questions:
#   1. Use a dataset with complaint resolution times. 
# We use a distribution that is skewed to the right (more common in real data)

set.seed(123) 
complaint_times <- rgamma(100, shape = 2, scale = 5)

complaint_times
head(complaint_times)
mean(complaint_times)
sd(complaint_times)
summary(complaint_times)

plot(complaint_times)
barplot(complaint_times)
hist(complaint_times)
pie(complaint_times)

hist(complaint_times, main = "Complaint Resolution Times", xlab = "Time (hours)")
hist(complaint_times, breaks = 30, main = "Complaint Resolution Times", xlab = "Time (hours)")


# 2. Calculate the mean and standard deviation.

mean_time <- mean(complaint_times)
sd_time <- sd(complaint_times)
cat("Mean resolution time:", round(mean_time, 2), "hours\n")
cat("Standard deviation:", round(sd_time, 2), "hours\n")



# 3. Compute a 95% confidence interval for average resolution time.
mean_time
sd_time
n <- length(complaint_times)
n

std_error <- sd_time/sqrt(n)
std_error
conf_lvl <- 0.95
alpha_sig_lvl <- 1 - alpha_sig_lvl
qnorm(0.975)
qnorm(0.95)
z-value <- qnorm(1 - alpha_sig_lvl/2)
margin_error <- z_value * std_error

confidence_int_lower <- mean(complaint_times) - qnorm(0.975) * sd(complaint_times) / sqrt(length(complaint_times))
confidence_int_lower

confidence_ints <- mean(complaint_times) + c(-1,1) * qnorm(0.975) * sd(complaint_times) / sqrt(length(complaint_times))
confidence_ints


# Scenario: An e-commerce site tracks conversion rates. In 1,200 visitors:
#   • 84 made purchases • Sample conversion rate: p̂= 84/1,200 = 7%
# Lab Questions:
#   1. Check if conditions are met for normal approximation 
x <- 84
n <- 1200
p_hat <- x/n
p_hat

np <- n * p_hat
nq <- n * (1-p_hat)
normal_ok <- (np >= 10) && (nq >= 10)
normal_ok

# 2. Calculate standard error for the proportion
std_err <- sqrt(p_hat * (1-p_hat) / n)

# 3. Build 95% confidence interval for true conversion rate 
z_value <- qnorm(0.975)
intervals <- p_hat + c(-1,1) * z_value * std_err
round(intervals,2)
intervals


# 4. How would you communicate this to the marketing team?

# “Based on a sample of 1,200 visitors, 84 made a purchase, giving a 7% conversion rate.
# Using statistical analysis, we are 95% confident that the true conversion rate is between 5% and 9%.
# This means that if we repeated similar campaigns, the actual conversion rate is likely to fall in this range.”
