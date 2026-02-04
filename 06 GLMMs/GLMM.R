library(tidyverse)

library(lme4)

library(performance)
library(parameters)
library(marginaleffects)

# load r2_pseudo function
source(
  "https://github.com/mattansb/Hierarchical-Linear-Models-foR-Psychologists/raw/refs/heads/main/helpers.R"
)


# The data --------------------------------------------------------------

# Data from https://doi.org/10.31234/osf.io/mt3n9
SFON_data <- haven::read_sav("SFON.sav") |>
  mutate(
    ID = factor(ID),
    Task = as_factor(Task)
  )

# This data was collected from 46 children aged 3-5.
nlevels(SFON_data$ID)
# They were given two validated versions (Truck/Bird) of the same task (Task):
# In each version, they played with a toy together with an experimenter. For
# each of "game", their behavior was coded as either showing some attention to
# the quantitative properties of the game (counting, saying numbers, etc.) or
# not (Attend). Additionally, the children ability to discriminate between
# quantities was measured (weberFr).

head(SFON_data, n = 16)


## Understanding the data --------------------

# The outcome is "Attend"
# What is the random grouping variable? Why?
# What level are the other variables on? Person (2)? Measurement (1)?

SFON_data |>
  filter(ID == "102")


# Out question:
# Do children with better quantitative discrimination (smaller weberFr) attend
# quantitative information with a higher probability than those with worse
# quantitative discrimination (controlling for Age)?

# We can summarize the data by child
ggplot(SFON_data, aes(weberFr, Attend)) +
  stat_summary(aes(group = ID), geom = "point")
# Looks like a negative trend - how can we model this?

# Learn more about GLMs here:
# https://github.com/mattansb/Practical-Applications-in-R-for-Psychologists/blob/master/11%20generalized%20linear%20models

# Random Intercepts Model -----------------------------------------------------

# Fitting a Binomial (logistic) random intercepts model.
mod_rndm.intr <- glmer(
  Attend ~ (1 | ID),
  family = binomial(link = "logit"),
  data = SFON_data
)


# - On average, do children tend to attend more or less?
model_parameters(mod_rndm.intr)
# Coefficient is negative, hard to tell...
model_parameters(mod_rndm.intr, exponentiate = TRUE)
# An Odds of 0.21 is ~1:4.5, or p = 0.17
plogis(-1.54)

# However, this is a _biased_ estimate. Why?
# Because inv(mean(logit)) != mean(inv(logit))
mean(SFON_data$Attend)

# We can get the unbiased estimate with {marginaleffects}:
# "generalized" means:
avg_predictions(
  mod_rndm.intr,
  newdata = datagrid(),
  re.form = NA
)
# vs. actual means:
avg_predictions(
  mod_rndm.intr,
  # We *want* the random effects
  newdata = datagrid(ID = unique),
  re.form = NULL
)

# Does this matter? Depends on what you're doing. The biased estimates are
# sometimes called *generalized means*, and those are sometimes fine. See:
# https://rvlenth.github.io/emmeans/articles/transformations.html?q=bias#bias-adj

# - What is the ICC?
icc(mod_rndm.intr)
# Note that logistic models don't have "error variance". Instead. level 1
# variance is fixed at (pi^2)/3
insight::get_variance_residual(mod_rndm.intr)


# Age model -------------------------------------------------------------

mod_age <- glmer(
  Attend ~ Age + (1 | ID),
  family = binomial(link = "logit"),
  data = SFON_data
)

anova(mod_age, mod_rndm.intr) # We can see the Age is a significant predictor.

# We can compute pseudo R2 - Age is a level 2 variable, so it explained level 2
# variance:
VarCorr(mod_rndm.intr)
VarCorr(mod_age)

r2_pseudo(mod_age, mod_rndm.intr)[1, ]
# Age explains 19% of the variance between children in their tendency to attend
# quantitative properties.

model_parameters(mod_age, exponentiate = TRUE)
# We can report the effect of age on the OR scale:
# Age had a positive effect on attending, with the odds of attending increasing
# of each year of age by a factor of OR = 65.37, 95% CI [4.43, 965.05], z =
# 3.04, p = .002.

# Or se can get average marginal slopes.
plot_predictions(mod_age, condition = "Age", re.form = NULL, rug = TRUE) +
  scale_y_continuous(
    expression(Pr(Attend)),
    limits = c(0, 1),
    labels = scales::label_percent(),
    oob = scales::oob_squish
  ) +
  theme_bw()
# The average marginal slopes are the average of the linear slopes across all
# values of Age.
avg_slopes(mod_age, variables = "Age", re.form = NULL)
# Age had a positive effect on attending, with the probability of attending
# increasing on average for each year of age by 45 percentage points, 95% CI
# [19, 72], z = 3.36, p < .001.

# Or we can get the average odds ratio:
avg_comparisons(
  mod_age,
  variables = "Age",
  comparison = "lnor",
  transform = "exp"
)
# Age had a positive effect on attending, with the odds of attending
# increasing on average for each year of age by a factor of 11.2, 95% CI
# [1.89, 66.4], z = 2.66, p = .008.

# Why doesn't this match the model_parameters() output?
# Because ORs are cursed with non-collapsibility:
# https://doi.org/10.1093/aje/kwaa267

# Exercise ---------------------------------------------------------------

# Fit a conditional model with a fixed effect for weberFr (on top of Age).
# - Does it have a better fit than the previous model?
# - How much *additional* variation in the random intercept is explained by
#   weberFr? (compute pseudo-R2.)
# - What what is the effect of weberFr? Is it in the expected direction?
#   - Use {marginaleffects} to plot the effect.
#   - Compute both OR and AME (average marginal effects)
# - Should we include "Task" as a random grouping variable? Why might we want
#   to, and why might we not?
