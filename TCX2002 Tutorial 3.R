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
srs <- customers[sample(1:500, 100), ]

    # o Stratified sampling (by region)
set.seed(42)
split_by_region <- split(customers_df, customers_df$region)


    # o Systematic sampling (every 10th customer)

  # 3. Compare the results: Are all regions equally represented in each technique?
