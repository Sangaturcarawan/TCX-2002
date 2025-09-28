######################
##### Question 1 #####
######################
library(dplyr)

# Set a seed for reproducibility
set.seed(123) 

# Generate the sample
coin_flips <- sample(c("Heads", "Tails"), 100, replace = TRUE) 

# Display the number of Heads and Tails
table(coin_flips)

# Just the count of "Heads"
heads_count_100 = table(coin_flips)["Heads"]
heads_count_100

# Generate the sample of 1000
coin_flips_1000 <- sample(c("Heads", "Tails"), 1000, replace = TRUE)

table(coin_flips_1000)


######################
##### Question 2 #####
######################

# Set a seed for reproducibility
set.seed(42)

# Create a data frame with 500 customers
customers <- data.frame(
  CustomerID = 1:500,
  Region = sample(c("North", "South", "East", "West"), size = 500, replace = TRUE)
)

# Display the first few rows of the dataset
head(customers)

# Display the distribution of regions in the original dataset
table(customers$Region)

###---------- SRS: Simple random sampling ----------###
simple_sample <- customers %>%
  sample_n(size = 100)

# Display the distribution of regions in the simple random sample
table(simple_sample$Region)

# -- another way to do SRS -- #Claudia
srs <- customers[sample(1:500, 100), ]
srs_dist <- table(srs$Region)
srs_dist
# --

###---------- Stratified sampling by region ----------###
stratified_sample <- customers %>%
  group_by(Region) %>%
  sample_frac(0.20)

# Display the distribution of regions in the stratified sample
table(stratified_sample$Region)

# anther way to do Stratified sampling by region (making it proportional - additional)
strat <- customers %>%
  group_by(Region) %>%
  slice_sample(prop = 0.2) %>%
  ungroup()
strat_dist <- table(strat$Region)
strat_dist

###---------- Systematic sampling (every 10th customer) ----------###
# # Ensure the data is sorted by CustomerID
#customers_sorted <- customers %>%
#  arrange(CustomerID)
# Calculate the sampling interval
#interval <- 10
## Get the indices for the systematic sample
# systematic_indices <- seq(from = 1, to = nrow(customers_sorted), by = interval)
## Select the customers based on the indices
# systematic_sample <- customers_sorted[systematic_indices, ]
## Display the distribution of regions in the systematic sample
# table(systematic_sample$Region)

###- Systematic sampling - (Dr Prakash slides)
N <- nrow(customers)   # population size (500)
n <- 100                # desired sample size
k <- N / n             # interval (10 here)
start <- sample(1:k, 1)  # random start between 1 and k
sys_idx <- seq(start, N, by = k)

systematic_sample <- customers[sys_idx, ]

# show samples divided by region
table(systematic_sample$Region)


### Compare ###
# Original / simple / stratified / systemic distribution
original_distribution <- table(customers$Region)
simple_distribution <- table(simple_sample$Region)
stratified_distribution <- table(stratified_sample$Region)
systematic_distribution <- table(systematic_sample$Region)

# Combine results into a single data frame for comparison
comparison <- data.frame(
  Original = original_distribution,
  SimpleRandom = simple_distribution,
  Stratified = stratified_distribution,
  Systematic = systematic_distribution
)

# Rename the columns for clarity
colnames(comparison) <- c("Region", "Original", "Region", "SimpleRandom", "Region", "Stratified", "Region", "Systematic")

print(comparison) # Display the comparison table

##### ----- Additional Notes on Sampling ----- #####
# Simple Random Sampling (SRS)
# - Every individual has the same chance of being picked. When your population is fairly homogeneous and you don’t need to worry about subgroups being under- or over-represented
# - Example: You have 500 customers spread across regions, and you want a general customer satisfaction survey. Just pick 50 at random... everyone is equally likely

# Stratified Sampling
# - Divide the population into groups (strata) and sample proportionally (or equally) within each.
# - Use case: When you want to ensure representation of all subgroups
# - Example: You want to compare complaint handling in North, South, East, West. If you sample randomly, small regions might be underrepresented. With stratified sampling, you guarantee each region is included fairly (e.g. 25% from each).

# Systematic Sampling
# - What it is: Select every k-th element from a list, after a random start
# - Use case: When your data are ordered (like a customer list or time sequence), and you want a quick, evenly spread sample
# - Example: You have a line of 500 customers in a call log, and you take every 10th one to check service quality. Much faster than random selection

# Conclusion: 
# Use SRS when the population is uniform and you just want fairness.
# Use Stratified when you care about comparing groups and want guaranteed representation.
# Use Systematic when you need a quick, evenly spaced sample — but watch out for hidden patterns.
######################
##### Question 3 #####
######################

# Set a seed for reproducibility
set.seed(123)

# Create a dummy dataset of 100 complaint resolution times (in hours)
# We use a distribution that is skewed to the right (more common in real data)
complaint_times <- rgamma(100, shape = 2, scale = 5)

# Display the first few entries of the dataset & simple visual
head(complaint_times)

plot(complaint_times)
barplot(complaint_times)
hist(complaint_times) # Best
pie(complaint_times)

# Visualize the distribution of the data
hist(complaint_times, main = "Complaint Resolution Times", xlab = "Time (hours)")
hist(complaint_times, breaks = 30, main = "Complaint Resolution Times", xlab = "Time (hours)")

### Calculate the mean (average) resolution time ###
mean_time <- mean(complaint_times)

# Calculate the standard deviation
sd_time <- sd(complaint_times)

# Print the results
cat("Mean resolution time:", round(mean_time, 2), "hours\n")
cat("Standard deviation:", round(sd_time, 2), "hours\n")



### Compute a 95% Confidence Interval ###
# Sample size (100)
n <- length(complaint_times)

# Z-score for a 95% confidence interval
z_score <- 1.96

# Calculate the standard error of the mean
standard_error <- sd_time / sqrt(n)

# Calculate the margin of error
margin_of_error <- z_score * standard_error

# Calculate the confidence interval bounds
lower_bound <- mean_time - margin_of_error
upper_bound <- mean_time + margin_of_error

# Print the confidence interval
cat("95% Confidence Interval of mean resolution time: [", round(lower_bound, 2), ", ", round(upper_bound, 2), "] hours\n")


##############################
## Question 3b - Ecommerce ### (Exercise 4B.2)
##############################
n <- 1200
p_hat <- 84 / n                  # 84/1200=7% conversion
SE <- sqrt(p_hat * (1 - p_hat) / n)# SE for a sample proportion 
z_star <- 1.96  # 95% CI

# 1. Check if conditions are met for normal approximation 
cat("To use the normal approximation method a minimum of 10 successes and 10 failures in each group are necessary.")
np <- n * p_hat
nq <- n * (1 - p_hat)
normal_ok <- (np >= 10) && (nq >= 10)
cat("Are the normalapprox conditions met? =", normal_ok, "\n")

# 2 & 3
lower <- p_hat - z_star * SE
upper <- p_hat + z_star * SE

cat("Sample proportion =", p_hat, "\n")
cat("Standard error =", SE, "\n")
cat("95% Confidence interval = [", lower, ",", upper, "]\n")

# 95% Confidence interval = [ 0.0555637 , 0.0844363 ]


# 4. How to communicate to the marketing team
  
#E.g. Based on a sample of 1,200 visitors to the site, about 7% made 
#    purchases. Statistically, we are 95% confident that the true 
#    conversion rate of all visitors is between X% and Y% (where X and 
#    Y are the lower and upper bounds from the calculation). This means 
#    if we repeated this process many times, 95% of the intervals would 
#    contain the true conversion rate. This information can help us set 
#    realistic expectations and goals for marketing campaigns."
