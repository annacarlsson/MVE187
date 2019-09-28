#################################################################
# ASSIGNMENT 2
# Updated: 2019-09-28
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE187/Assignments/Assignment_2")
library(ggplot2)
library(gplots)

palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(23563)

##### 1A) Probability of power delivery at any time #####
prob <- pgamma(24, shape = 0.8, scale = 6, log = FALSE) - pgamma(4, shape = 0.8, scale = 6, log = FALSE)

##### 1B) Simulate expected value and CI for expected value

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
N <- 10000
sample <- rgamma(N, shape = 0.8, scale = 6)
P_sample <- P(sample)
E_approx <- 1/N * sum(P_sample)

# Compute approximate 95% CI
s <- sqrt(1 / (N-1)  * sum((P_sample - E_approx)^2))

ci_lower <- E_approx - 1.96 * s / sqrt(N)
ci_upper <- E_approx + 1.96 * s / sqrt(N)

##### 1C) Numerical integration #####

# Define integrand
integrand <- function(v){
  p <- P(v)
  gamma <- dgamma(v, shape = 0.8, scale = 6)
  return(p*gamma)
}

# Compute integral numerically
E <- integrate(integrand, 4, 25)

##### 1D) Use importance sampling to improve accuracy #####

# Plot each function and consider which proposal density to use
v <- seq(1,25,length.out = 10000)
p_v <- P(v)
pi_v <- dgamma(v, shape = 0.8, scale = 6)
instr_distr <- dnorm(v, mean = 11, sd = 4.5, log = FALSE)

ggplot() +
  geom_line(
    aes(x = v, y = p_v), colour = palette[2], size = 1, show.legend = FALSE) +
  geom_line(
    aes(x = v, y = pi_v), colour = palette[3], size = 1, show.legend = FALSE) +
  labs(title = "Distributions of P(v) and v",
       x = "v",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8)))

ggplot() +
  geom_line(
    aes(x = v, y = p_v*pi_v), colour = palette[1], size = 1, show.legend = FALSE) +
  geom_line(
    aes(x = v, y = instr_distr), colour = palette[4], size = 1, show.legend = FALSE) +
  labs(title = "Scaled product of densities of P(v) and v",
       x = "v",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8)))

##### 1E) Use proposal density to reestimate mean #####

# Define importance sampling function
P_importance <- function(v){
  p_v <- P(v)
  pi_v <- dgamma(v, shape = 0.8, scale = 6)
  g_v <- dnorm(v, mean = 11, sd = 4.5, log = FALSE)
  return(p_v * pi_v / g_v)
}

N <- 10000
importance_sample <- rnorm(N, mean = 11, sd = 4.5)
P_importance_sample <- P_importance(importance_sample)
E_importance_approx <- 1/N * sum(P_importance_sample)

##### 1F) Estimate variance of power production #####
s_importance <- sqrt(1 / (N-1)  * sum((P_importance_sample - E_importance_approx)^2))

ci_importance_lower <- E_importance_approx - 1.96 * s_importance / sqrt(N)
ci_importance_upper <- E_importance_approx + 1.96 * s_importance / sqrt(N)





