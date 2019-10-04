#################################################################
# ASSIGNMENT 2
# Updated: 2019-10-04
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE187/Assignments/Assignment_2")
library(ggplot2)
library(gplots)
library(emdbook)

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
N <- 20000
sample <- rgamma(N, shape = 0.8, scale = 6)
P_sample <- P(sample)
E_approx <- 1/N * sum(P_sample)

# Compute approximate 95% CI
s <- sqrt(1 / (N-1)  * sum((P_sample - E_approx)^2))

ci_lower <- E_approx - 1.96 * s / sqrt(N)
ci_upper <- E_approx + 1.96 * s / sqrt(N)

# Create plot for approximate E as function of sample size
sim_function <- function(N){
  output <- rep(0,length(N))
  index <- 1
  for (i in N){
    print(i)
    sample_temp <- rgamma(i, shape = 0.8, scale = 6)
    P_sample_temp <- P(sample_temp)
    E_approx_temp <- 1/i * sum(P_sample_temp)
    output[index] <- E_approx_temp
    index <- index + 1
  }
  return(output)
}

N_seq <- seq(1,20000,100)
E_seq <- sim_function(N_seq)

ggplot() +
  geom_line(
    aes(x = N_seq, y = E_seq), colour = palette[2], size = 1, show.legend = FALSE) + 
  labs(title = "Simulated mean as function of sample size",
       x = "Sample size",
       y = "Simulated mean") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill=alpha('white', 0.9)))

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
    aes(x = v, y = p_v, colour = "P(v)"), size = 1, show.legend = TRUE) +
  geom_line(
    aes(x = v, y = pi_v,  colour = "pi(v)"), size = 1, show.legend = TRUE) +
  labs(title = "Distributions of P and v",
       x = "v",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)) + 
  scale_colour_manual("", 
                      breaks = c("P(v)", "pi(v)"),
                      values = c("P(v)"=palette[2], "pi(v)"=palette[3]))

ggplot() +
  geom_line(
    aes(x = v, y = p_v*pi_v, colour = "P(v) * pi(v)"), size = 1, show.legend = TRUE) +
  geom_line(
    aes(x = v, y = instr_distr, colour = "Proposal density"), size = 1, show.legend = TRUE) +
  labs(title = "Product of densities of P(v) and v and proposal density",
       x = "v",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10)) +
  scale_colour_manual("", 
                      breaks = c("P(v) * pi(v)", "Proposal density"),
                      values = c("P(v) * pi(v)"=palette[2], "Proposal density"=palette[3]))

##### 1E) Use proposal density to reestimate mean #####

# Define importance sampling function
P_importance <- function(v){
  p_v <- P(v)
  pi_v <- dgamma(v, shape = 0.8, scale = 6)
  g_v <- dnorm(v, mean = 11, sd = 4.5, log = FALSE)
  return(p_v * pi_v / g_v)
}

N <- 20000
importance_sample <- rnorm(N, mean = 11, sd = 4.5)
P_importance_sample <- P_importance(importance_sample)
E_importance_approx <- 1/N * sum(P_importance_sample)

# 95% confidence interval
s_importance <- sqrt(1 / (N-1)  * sum((P_importance_sample - E_importance_approx)^2))

ci_importance_lower <- E_importance_approx - 1.96 * s_importance / sqrt(N)
ci_importance_upper <- E_importance_approx + 1.96 * s_importance / sqrt(N)

##### 1F) Estimate variance of power production #####
estimated_variance <- 1 / N  * sum((P_importance_sample - E_importance_approx)^2)

##### 2A) Maximum likelihood estimate of lambda #####
Y_i <- c(162, 267, 271, 185, 111, 61, 27, 8, 3, 1)
I <- c(0:9)

# MLE of lambda
lambda_est <- sum(Y_i*I) / sum(Y_i)

# Compute values from distribution
est_vals <- n * dpois(I, lambda_est, log = FALSE)

# Plot distribution with estimated values and actual values
ggplot() +
  geom_line(
    aes(x = I, y = est_vals), colour = palette[2], size = 1, show.legend = FALSE) +
  geom_point(
    aes(x = I, y = Y_i), colour = palette[3], size = 1, show.legend = FALSE) +
  geom_point(
    aes(x = I, y = est_vals), colour = palette[2], size = 1, show.legend = FALSE) +
  labs(title = "Estimated values and actual values",
       x = "i",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8)))

##### 2F) Gibb's sampling of posterior of parameters #####

# Initialize parameters
N <- 100000
p <- rep(0.5, N)
lambda_1 <- rep(2, N)
lambda_2 <- rep(2, N)
ndata <- length(Y_i)

p_bin <- rep(0, ndata)
Z_i <- rep(0, ndata)

for (i in 2:N) {
  
  for (j in 1:10){
    p_bin[j] <- p[i-1] * dpois(j-1, lambda_1[i-1]) / ((p[i-1] * dpois(j-1, lambda_1[i-1]) + (1-p[i-1]) * dpois(j-1, lambda_2[i-1])))
    Z_i[j] <- rbinom(1, Y_i[j], p_bin[j])
  }

  p[i] <- rbeta(1, (1 + ndata * mean(Z_i)), 1 + 1 + ndata * mean(Y_i - Z_i))
  
  for (j in 1:10){
    p_bin[j] <- p[i] * dpois(j-1, lambda_1[i-1]) / ((p[i] * dpois(j-1, lambda_1[i-1]) + (1-p[i]) * dpois(j-1, lambda_2[i-1])))
    Z_i[j] <- rbinom(1, Y_i[j], p_bin[j])
  }
  
  lambda_1[i] <- rgamma(1, (1 + sum(I * Z_i)), (1 + ndata * mean(Z_i)))
  
  for (j in 1:10){
    p_bin[j] <- p[i] * dpois(j-1, lambda_1[i]) / ((p[i] * dpois(j-1, lambda_1[i]) + (1-p[i]) * dpois(j-1, lambda_2[i-1])))
    Z_i[j] <- rbinom(1, Y_i[j], p_bin[j])
  }
  
  lambda_2[i] <- rgamma(1, (1 + sum(I * (Y_i - Z_i))), (1 + ndata * mean(Y_i - Z_i)))
}

lambda_1_mean <- mean(lambda_1)
lambda_2_mean <- mean(lambda_2)
p_mean <- mean(p)

extended_model <- p_mean * dpois(I, lambda_1_mean) + (1 - p_mean) * dpois(I, lambda_2_mean)

# Plot histograms for each of the parameters
ggplot() +
  geom_histogram(
    aes(x = p), colour = palette[3], fill = palette[3], bins = 50) + 
  labs(title = "Histogram of p",
       x = "p",
       y = "Counts") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8)))

ggplot() +
  geom_histogram(
    aes(x = lambda_1), colour = palette[3], fill = palette[3], bins = 50) + 
  labs(title = "Histogram of lambda_1",
       x = "lambda_1",
       y = "Counts") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8)))

ggplot() +
  geom_histogram(
    aes(x = lambda_2), colour = palette[3], fill = palette[3], bins = 50) + 
  labs(title = "Histogram of lambda_2",
       x = "lambda_2",
       y = "Counts") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8)))

# Plot the improved density with MLE
ggplot() +
  geom_line(
    aes(x = I, y = est_vals, colour = "Poisson MLE"), size = 1, show.legend = TRUE) +
  geom_line(
    aes(x = I, y = n * extended_model, colour = "Extended model"), size = 1, show.legend = TRUE) +
  geom_line(
    aes(x = I, y = Y_i, colour = "Actual counts"), size = 1, show.legend = TRUE) +
  labs(title = "MLE density and mixture density",
       x = "i",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8))) +
  scale_colour_manual("", 
                      breaks = c("Actual counts", "Poisson MLE", "Extended model"),
                      values = c("Actual counts"=palette[3], "Poisson MLE"=palette[2], "Extended model" =palette[4]))

