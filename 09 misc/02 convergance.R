library(dplyr)

library(lmerTest)

# Convergence  ---------------------------

# Let's use the same example from our single-trial RT analysis.
data(stroop, package = "afex")
?afex::stroop

stroop_1 <- stroop |>
  filter(
    pno %in% paste0("s1_", 1:10), # use only 10 subjects for ease of example
    acc == 1 # only correct responses
  ) |>
  # set contrasts to sum coding (instead of default treatment coding):
  mutate(
    condition = C(condition, contr = contr.sum),
    congruency = C(congruency, contr = contr.sum)
  )


# This model will not converge:
mod <- glmer(
  rt ~ condition * congruency + (condition * congruency | pno),
  family = inverse.gaussian(link = "identity"),
  data = stroop_1
)

# We already saw that this warning can be ignored:
performance::check_convergence(mod)

# But let's try and make it go away *without* reducing the random effects
# structure (i.e., without going from a maximal model to a non-maximal model).

## 1. Turn off some convergence checks ----------------

# lme4 _loooooooves_ to check all kinds of convergence criteria, some of which
# are completely unrelated to what interests us (e.g., whether the Hessian is
# positive definite, whether the gradient is small, whether the optimizer
# converged, etc.)
# We can turn off some of these checks with:
lmerControl(calc.derivs = FALSE)
glmerControl(calc.derivs = FALSE)


glmer(
  rt ~ condition * congruency + (condition * congruency | pno),
  family = inverse.gaussian(link = "identity"),
  data = stroop_1,

  control = glmerControl(calc.derivs = FALSE)
)
# It worked!

## 2. Use a different optimizer ----------------

# lme4 also has a bunch of optimizers to choose from. A good choice is the
# "bobyqa" optimizer, which can be set with:
lmerControl("bobyqa")
glmerControl("bobyqa")

mod2 <- glmer(
  rt ~ condition * congruency + (condition * congruency | pno),
  family = inverse.gaussian(link = "identity"),
  data = stroop_1,

  control = glmerControl("bobyqa")
)
# It worked again!

# See more info about using and comparing optimizers:
?allFit()

## 3. Reparameterize the model ----------------

# This can mean:
# - re-scaling numerical predictors
# - changing contrasts for factors
# - using cell-mean coding (0 + f1:f2) or nesting (f1 / f2) terms

glmer(
  rt ~ 0 + condition:congruency + (0 + condition:congruency | pno),
  family = inverse.gaussian(link = "identity"),
  data = stroop_1
)
# This doesn't work...

# It can also mean using complex random intercepts (CRI), per Scandola & Tidoni
# (2024) https://doi.org/10.1177/25152459231214454
#
# This means going from this (x is a numeric predictor and f is a factor):
y ~ x * f + (1 + x * f | g)
# becomes:
y ~ x * f + (1 | g1) + (1 + x | g1:f)

mod_cri <- glmer(
  rt ~ condition *
    congruency +
    (1 | pno) +
    (1 | pno:condition) +
    (1 | pno:congruency) +
    (1 | pno:condition:congruency),
  family = inverse.gaussian(link = "identity"),
  data = stroop_1
)
# This uses a different number of degrees of freedom, so if you're comparing
# models make sure they are all using CRI or all NOT using CRI.

parameters::compare_parameters(
  bobyqa = mod2,
  CRI = mod_cri,
  select = "{estimate} ({se})"
) |>
  print(digits = 3)
# We can see that these give very similar estimates and standard errors as other
# methods.

# CRI has a disadvantage of making interpretation of random effects more
# difficult, but it has the advantage of being less computationally expensive
# and so also much (much) faster.

## 4. Bayes! ----------------

# Bayes is much more robust to these convergence issues (and also has many many
# more advantages), so really this is a great time to learn Bayesian
# modelling...

# You can use {brms}, which is a bit more involved (but IMO totally worth it):
brms::brm(
  rt ~ condition * congruency + (condition * congruency | pno),
  family = inverse.gaussian(link = "identity"),
  data = stroop_1,
  # prior = ... # YOU WOULD NEED TO SPECIFY PRIORS FOR THIS MODEL!
)


# Or you can use {rstanarm}, which is a bit more plug-and-play (but also less
# flexible):
rstanarm::stan_glmer(
  rt ~ condition * congruency + (condition * congruency | pno),
  family = inverse.gaussian(link = "identity"),
  data = stroop_1,
  # prior = ... # YOU WOULD NEED TO SPECIFY PRIORS FOR THIS MODEL!
)
