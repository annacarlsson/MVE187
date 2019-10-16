#################################################################
# EXERCISES
# Updated: 2019-10-16
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE187/Exercises/")
library(ggplot2)
library(gplots)
library(LearnBayes)

palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

##### Problems #####

### Problem A4.1

# Definition of function
binomial.conf.interval = function(y,n){
  z = qnorm(.95)
  phat = y / n
  se = sqrt(phat * (1 - phat) / n)
  return(c(phat - z * se, phat + z * se))
}

# Compute vector of confidence intervals
n <- 20
y <- rbinom(n=n, size=n, prob=0.5)
ci_b <- binomial.conf.interval(y,n)
ci_b <- matrix(ci_b, nrow = n, byrow = FALSE)

# Compute the proportion where true p is in CI
nbr_contains_b <- ci_b[ci_b[,1] < 0.5 & ci_b[,2] > 0.5,]
prop_b <- dim(nbr_contains_b)[1]/n

# Now repeat, but let p = 0.05
n <- 20
y <- rbinom(n=n, size=n, prob=0.05)
ci_c <- binomial.conf.interval(y,n)
ci_c <- matrix(ci_c, nrow = n, byrow = FALSE)

nbr_contains_c <- ci_c[ci_c[,1] < 0.05 & ci_c[,2] > 0.05,]
prop_c <- dim(nbr_contains_c)[1]/n

