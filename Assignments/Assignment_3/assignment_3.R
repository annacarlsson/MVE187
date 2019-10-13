#################################################################
# ASSIGNMENT 3
# Updated: 2019-10-13
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE187/Assignments/Assignment_3")
library(ggplot2)
library(gplots)

palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(23563)

##### Implementation of EM algorithm #####
tol <- 1e-6

Y_i <- c(162, 267, 271, 185, 111, 61, 27, 8, 3, 1)
I <- c(0:9)

# Initialize vectors
p <- 0
lambda_1 <- 0
lambda_2 <- 0

# Set initial values
p <- c(p, 0.01)
lambda_1 <- c(lambda_1, 0.001)
lambda_2 <- c(lambda_2, 0.001)

ndata <- length(Y_i)
p_prim <- rep(0, ndata)

# Run algorithm
k <- 3
converged <- FALSE
while (converged == FALSE) {
  
  # E step (computing p_prim)
  for (j in 1:10){
    p_prim[j] <- p[k-1] * dpois(j-1, lambda_1[k-1]) / ((p[k-1] * dpois(j-1, lambda_1[k-1]) + (1 - p[k-1]) * dpois(j-1, lambda_2[k-1])))
  }
  
  # M step
  p[k] <- sum(Y_i * p_prim) / sum(Y_i)
  lambda_1[k] <- sum(Y_i * I * p_prim) / (1 + sum(Y_i * p_prim))
  lambda_2[k] <- sum(Y_i * I * (1 - p_prim)) / (1 + sum(Y_i * (1 - p_prim)))               
  
  # Check for convergence
  if (abs(p[k] - p[k-1])<tol && abs(lambda_1[k] - lambda_1[k-1])<tol && abs(lambda_2[k] - lambda_2[k-1])<tol){
    converged <- TRUE
  }
  
  k <- k + 1 
}

##### Plots #####

# MLE of lambda
lambda_est <- sum(Y_i*I) / sum(Y_i)
n <- sum(Y_i)
est_vals <- n * dpois(I, lambda_est, log = FALSE)

# Counts from model fitted in assignment 3
end <- length(p)
extended_model <- p[end] * dpois(I, lambda_1[end]) + (1 - p[end]) * dpois(I, lambda_2[end])

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

