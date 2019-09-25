#################################################################
# ASSIGNMENT 2
# Updated: 2019-09-24
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE187/Assignments/Assignment_2")
library(ggplot2)
library(gplots)

palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(23563)

##### A) Probability of power delivery at any time #####
prob <- pgamma(24, shape = 0.8, scale = 6, log = FALSE) - pgamma(4, shape = 0.8, scale = 6, log = FALSE)

##### B) Simulate expected value and CI for expected value

# Define P(v)
P <- function(v){
  n <- length(v)
  P_temp <- rep(0, n)
  for (i in 1:n){
    if (v[i] >= 4 && v[i] < 15 ){
      P_temp[i] <- cos(v[i]*pi/11+7*pi/11) + 1
    }
    if (v[i] >= 15 && v[i] < 25){
      P_temp[i] <- 7/4 + v[i]/30 - v[i]^2/900
    }
  }
  return(P_temp)
}

# Compute approximate expected value
N <- 100
sample <- rgamma(N, shape = 0.8, scale = 6)
P_sample <- P(sample)
E_approx <- 1/N * sum(P_sample)

# Compute approximate 95% CI
s <- sqrt(1 / (N-1)  * sum((P_sample - E_approx)^2))

ci_lower <- E_approx - 1.96 * s / sqrt(N)
ci_upper <- E_approx + 1.96 * s / sqrt(N)

##### C) Numerical integration #####


