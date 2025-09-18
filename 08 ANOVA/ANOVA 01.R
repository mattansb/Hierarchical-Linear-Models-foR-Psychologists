library(dplyr)
library(ggplot2)

library(lmerTest)

library(datawizard)
library(emmeans)
emm_options(lmer.df = "S")

# The data --------------------------------------------------------------------

data("obk.long", package = "afex")
?afex::obk.long

head(obk.long)

## Understanding the data --------------------------

# The outcome is "value".
# What is the random grouping variable? Why?
# What level are the other variables on? Person (2)? Measurement (1)?

obk.long |>
  filter(id == "1")


# We want to examine the effect of treatment and its interaction with phase and
# hour, controlling for pre-treatment covariates: age and gender.

## Preparing the data for an ANOVA ------------------------------

# To produce valid type-3 ANOVA tables with proper F-tests (or Chi-square tests)
# we need to fit the model with all of our predictors mean-centered.
# - For continuous predictors we can do this by subtracting the grand mean.
obk.long$age_c <- center(obk.long$age)
obk.long$hour_c <- center(obk.long$hour)
# - For categorical predictors, centering means have 0 mean "grand average".
#   We can do this by using effects coding dummy variables.
contrasts(obk.long$treatment)
contrasts(obk.long$treatment) <- contr.sum
contrasts(obk.long$treatment)
# When both contrasts are 0, this is equivalent to talking about the mean of the
# 3 groups. Unfortunately this makes the regression coefficients harder to
# interpret, but we can use followup contrasts and simple effects instead (see
# below). Learn more here:
# https://shouldbewriting.netlify.app/posts/2021-05-25-everything-about-anova/
#
# Let's do this for gender and phase as well
contrasts(obk.long$gender) <- contr.sum
contrasts(obk.long$phase) <- contr.sum


# The maximal model -------------------------------------------------------

mod_maximal <- lmer(
  value ~ (treatment + hour_c) * phase + gender + age_c + (phase + hour_c | id),
  data = obk.long
)
# Note: we didn't include _all_ the interactions as fixed effects!
# Why aren't we including the phase * hour_c interaction as a random slope?

## ANOVA table -------------------------------------------------------------

# We can obtain an Type-3 ANOVA table using the {car} package:
AOV_maximal <- car::Anova(mod_maximal, type = 3, test.statistic = "F")
AOV_maximal

# For GLMMs, we can only set test.statistic = "Chisq" (default). This will give
# an Analysis of Deviance Table, but everyone still seems to call it an ANOVA
# table or an ANOVA-like table.

# We can generate from this table _*approximate*_ effect sizes with the
# {effectsize} package:
effectsize::eta_squared(AOV_maximal)


## Follow-up ---------------------------------------------------------------
# Contrasts, simple effects, etc. can all be carried out with {emmeans}, just
# like with a standard ANOVA. See
# https://github.com/mattansb/Analysis-of-Factorial-Designs-foR-Psychologists

# Marginal means:
em.int <- emmeans(mod_maximal, ~ treatment + phase)
em.int


# Simple effects:
joint_tests(mod_maximal, by = "treatment")

# Simple contrasts:
w.treatmet <- tibble(
  "ctrl vs {A,B}" = c(-2, 1, 1) / 2,
  "A vs B" = c(0, -1, 1)
)

contrast(em.int, method = w.treatmet, by = "phase")
contrast(em.int, interaction = list(treatment = w.treatmet, phase = "consec"))

pd <- position_dodge(0.2)
as.data.frame(em.int) |>
  ggplot(aes(
    phase,
    emmean,
    color = treatment,
    shape = treatment,
    group = treatment
  )) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.1,
    color = "black",
    position = pd
  ) +
  geom_point(size = 3, position = pd) +
  geom_line(position = pd) +
  scale_color_manual(
    "Group",
    labels = c("Control", "A", "B"),
    values = c("grey80", "red", "firebrick")
  ) +
  scale_shape("Group", labels = c("Control", "A", "B")) +
  scale_x_discrete(
    NULL,
    limits = c("pre", "post", "fup"),
    labels = c("Pre", "Post", "Follow-Up")
  ) +
  labs(y = "Outcome") +
  theme_classic()
# etc...

# Exercise ----------------------------------------------------------------

# Going back to the example from crossed random grouping variables.
# 1. Fit the maximal model with an effect for condition, sex, and their
#    interaction. Make sure data is prepared for and ANOVA table.
# 2. Produce an ANOVA table.
