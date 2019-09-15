#################################################################
# ASSIGNMENT 1
# Updated: 2019-09-12
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE187/Assignments/Assignment_1")
library(ggplot2)
library(gplots)
library(ggpubr)

palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(23563)

#### A) Function for plotting normal mixtures ####

# Definition of function
normal_mixture_density <- function(thetas, means, stds, weights){
  n <- length(weights)
  N <- length(thetas)
  nonweighted_densities <- matrix(data = NA, nrow = N, ncol = n)
  for (i in 1:n){
    nonweighted_densities[,i] <- dnorm(thetas, means[i], stds[i], log = FALSE)
  }
  mixture_density <- nonweighted_densities %*% diag(weights)
  return (rowSums(mixture_density))
}

# Parameters
thetas <- seq(1.51, 1.53, length=1000)
weights <- c(0.33, 0.57, 0.10)
means <- c(1.5163, 1.5197, 1.5203)
stds <- c(0.001, 0.001, 0.005)

mixture_density <- normal_mixture_density(thetas, means, stds, weights)

# Densities for each Gaussian, for comparison in plot
density_1 <- dnorm(thetas, means[1], stds[1], log = FALSE)
density_2 <- dnorm(thetas, means[2], stds[2], log = FALSE)
density_3 <- dnorm(thetas, means[3], stds[3], log = FALSE)

# Plot densities
ggplot() +
  geom_line(
    aes(x = thetas, y = mixture_density), colour = palette[3], size = 1, show.legend = FALSE) + 
  geom_line(
    aes(x = thetas, y = density_1), colour = palette[1], size = 1, alpha = 0.25, show.legend = FALSE) + 
  geom_line(
    aes(x = thetas, y = density_2), colour = palette[2], size = 1, alpha = 0.25, show.legend = FALSE) + 
  geom_line(
    aes(x = thetas, y = density_3), colour = palette[4], size = 1, alpha = 0.25, show.legend = FALSE) + 
  labs(title = "Density for IR (Gaussian mixture)",
     x = "Theta",
     y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill=alpha('white', 0.9)))

#### B) Prior predictive for mixture density ####
# Parameters
x <- seq(1.51, 1.53, length=1000)
weights <- c(0.33, 0.57, 0.10)
means <- c(1.5163, 1.5197, 1.5203)
stds <- c(sqrt(0.001^2 + 0.001^2), sqrt(0.001^2 + 0.001^2), sqrt(0.001^2 + 0.005^2))

mixture_prior_density <- normal_mixture_density(x, means, stds, weights)

# Prior predictive densities for each Gaussian
prior_density_1 <- dnorm(x, means[1], stds[1], log = FALSE)
prior_density_2 <- dnorm(x, means[2], stds[2], log = FALSE)
prior_density_3 <- dnorm(x, means[3], stds[3], log = FALSE)

# Plot densities
ggplot() +
  geom_line(
    aes(x = x, y = mixture_prior_density), colour = palette[3], size = 1, show.legend = FALSE) + 
  geom_line(
    aes(x = x, y = prior_density_1), colour = palette[1], size = 1, alpha = 0.25, show.legend = FALSE) + 
  geom_line(
    aes(x = x, y = prior_density_2), colour = palette[2], size = 1, alpha = 0.25, show.legend = FALSE) + 
  geom_line(
    aes(x = x, y = prior_density_3), colour = palette[4], size = 1, alpha = 0.25, show.legend = FALSE) + 
  
  labs(title = "Prior predictive density for the measurement x (Gaussian mixture)",
       x = "x",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.85, 0.8),
    legend.background = element_rect(fill=alpha('white', 0.9)))

##### C) Posterior of theta

# Define function for posterior
posterior_theta <- function(thetas, x, sigma, sigmas_mix, means, weights){
  predictive_prior_stds <- sqrt(sigmas_mix^2+sigma^2)
  
  likelihood <- dnorm(x, mean = thetas, sd = sigma, log = FALSE)
  prior <- normal_mixture_density(thetas, means, sigmas_mix, weights)
  predictive_prior <- normal_mixture_density(x, means, predictive_prior_stds, weights)
  
  posterior <- likelihood * prior / predictive_prior
  return(posterior)
}

# Define parameters
thetas <- seq(1.51, 1.53, length=1000)
x <- 1.52083
sigma <- 0.001
sigmas <- c(0.001, 0.001, 0.005)
means <- c(1.5163, 1.5197, 1.5203)
weights <- c(0.33, 0.57, 0.10)

# Compute densities (also, the component densities for comparison as functions of theta)
posterior <- posterior_theta(thetas, x, sigma, sigmas, means, weights)
likelihood <- dnorm(x, thetas, sigma)
prior <- normal_mixture_density(thetas, means, sigmas, weights)
predictive_prior <- normal_mixture_density(x, means, sqrt(sigmas^2+sigma^2), weights)

ggplot() +
  geom_line(
    aes(x = thetas, y = posterior, colour = "Posterior"), size = 1, show.legend = TRUE) +
  geom_line(
    aes(x = thetas, y = likelihood, colour = "Likelihood"), size = 1, alpha = 0.25, show.legend = TRUE) +
  geom_line(
    aes(x = thetas, y = prior, colour = "Prior"), size = 1, alpha = 0.25, show.legend = TRUE) +
  geom_line(
    aes(x = thetas, y = predictive_prior, colour = "Predictive prior"), size = 1, alpha = 0.25, show.legend = TRUE) +
  labs(title = "Posterior distribution of theta given x = 1.52083",
       x = "Theta",
       y = "Density") + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    legend.position = c(0.89, 0.85),
    legend.background = element_rect(fill=alpha('white', 0.8))) + 
  #scale_colour_manual(values = palette[c(1,2,3,4)], labels = c('With correlated','Without correlated', '', ''), guide = 'legend',name='Variables')
  scale_colour_manual("", 
                      breaks = c("Posterior", "Likelihood", "Prior", "Predictive prior"),
                      values = c("Posterior"=palette[3], "Likelihood"=palette[1], 
                               "Prior"=palette[2], "Predictive prior"=palette[4]))

