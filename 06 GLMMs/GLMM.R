library(tidyverse)
library(lme4)
library(performance)
library(parameters)

SFON_data <- haven::read_sav("SFON.sav")
SFON_data$Task <- factor(SFON_data$Task, labels = c("Bird", "Truck"))
# This data was collected from 46 children aged 3-5. 
# They were given two versions (Truck/Bird) of the same task: In each version,
# they played with a toy together with an experimenter. During each of "game",
# they're behavior was codded as either showing some attention to the
# quantitative properties of the game (counting, saying numbers, etc.) or not.
# Additionally, the children ability to discriminate between quantities was
# measured.
head(SFON_data)
#      ID - Child ID.
#    Task - Which of the tasks (Bird or Truck)
#     Age - Child's age in years
# weberFr - quantitative discrimination ability (smaller numbers are better)
#  Attend - Did the child attend to the quantitative properties of the game?

# Data from here:
# https://psyarxiv.com/mt3n9/


# We are interested in:
# Do children with better quantitative discrimination (smaller weberFr) tend to
# attend to quantitative information with a higher probability than those with
# worse quantitative discrimination?
# We must also control for Age.


# Think about that data:
# - Which effects do we have?
# - What level are each of these effect in?
# - How is the data nested?


# We can summarise the data by child
ggplot(SFON_data, aes(weberFr, Attend)) + 
  stat_summary(aes(group = ID), geom = "point")
# Looks like a negative trend - how can we model this?

# Learn more aboug GLMs here:
# https://github.com/mattansb/Practical-Applications-in-R-for-Psychologists/tree/master/08%20generalized%20linear%20models


# Random Intercepts Model -----------------------------------------------------

# Fitting a Binomial (logistic) random intercepts model.
m0 <- glmer(Attend ~ (1 | ID),
            family = binomial("logit"),
            data = SFON_data)
# - What is the ICC?
icc(m0)
# Note that logistic models don't have "error variance". Instead. level 1 variance is 
# fixed at (pi^2)/3
insight::get_variance_residual(m0)

# - On average, do children tend to attend more or less?
model_parameters(m0)
# Coefficient is negative, hard to tell...
model_parameters(m0, exponentiate = TRUE)
# An Odds of 0.21 is ~1:4.5, or p = 0.17


# Exercise ---------------------------------------------------------------

# 1. Fit a conditional model with a fixed effect for Age.
# - How much variation in the random intercept is explained by Age?


# 2. Fit a conditional model with a fixed effect for weberFr (on top of Age).
# - Does it have a better fit than the previous model?
# - What what is the slope of weberFr? Is it in the expected direction?
# - How much additional variation in the random intercept is explained by weberFr?
  



