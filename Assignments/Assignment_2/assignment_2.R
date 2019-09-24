#################################################################
# ASSIGNMENT 2
# Updated: 2019-09-24
# Author: Anna Carlsson
#################################################################

##### Set working directory and load packages #####
setwd("/Users/anna/Dokument/GitHub/MVE187/Assignments/Assignment_1")
library(ggplot2)
library(gplots)

palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set.seed(23563)

##### A) Probability of power delivery at any time #####
prob <- pgamma(24, shape = 0.8, scale = 6, log = FALSE) - pgamma(4, shape = 0.8, scale = 6, log = FALSE)
