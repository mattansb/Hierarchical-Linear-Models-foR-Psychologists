library(tidyverse)
library(datawizard)

library(lmerTest)

library(performance)
library(parameters)


# Setup -----------------------------------------------------------------

# Let's use the same example from 3-level model.

# This model will not converge *and* will give a warning about singular fit:
mod <- lmer(
  math ~ grade + (grade | childid:schoolid) + (grade | schoolid),
  data = mlmRev::egsingle
)

# Singular fits -------------------------------------------------------

# This model is indeed singular:
check_singularity(mod)

# This means we have a problem with the random effects structure.
# In this case, we can see it with the correlation between the random effects
# for childid being exactly 1.00:
VarCorr(mod)

# Is this okay?

# If we're interested in inferences on the random effects: no - estimates and
# standard errors of the random effects are not reliable:
model_parameters(mod, effects = "random", ci_random = TRUE) # ! (Waring: potentially long run time)

# If we're only interested in the fixed effects: maybe - fixed effects estimates
# are still reliable, but their standard errors are based on the estimated random
# effects and so might not be reliable.

# It is recommended to use other methods of inference that are not based on the simple
# standard errors (also known as Wald standard errors) such:
# - Compare models using likelihood ratio tests
# - Profile CIs (ignore SE, t, p - look only at CIs):
model_parameters(mod, effects = "fixed", ci_method = "profile") # ! (Waring: potentially long run time)
# - Non-parametric Bootstrapping

# Convergence issues -------------------------------------------------------

# We already saw that this warning can be ignored:
check_convergence(mod)

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


mod_no.derivs <- lmer(
  math ~ grade + (grade | childid:schoolid) + (grade | schoolid),
  data = mlmRev::egsingle,

  control = lmerControl(calc.derivs = FALSE)
)
# Didn't work :(

## 2. Use a different optimizer ----------------

# lme4 also has a bunch of optimizers to choose from. A good choice is the
# "bobyqa" optimizer, which can be set with:
lmerControl("bobyqa")
glmerControl("bobyqa")

mod_change.optim <- lmer(
  math ~ grade + (grade | childid:schoolid) + (grade | schoolid),
  data = mlmRev::egsingle,

  control = lmerControl("bobyqa")
)
# It worked!

# See more info about using and comparing optimizers:
?allFit()

## 3. Reparameterize the model ----------------

# This can mean:
# - re-scaling or centering numerical variables
# - changing contrasts for factors (treatment, cell-mean [below], ...)
# - ...

mod_standardized <- lmer(
  math ~ standardize(grade) +
    (standardize(grade) | childid:schoolid) +
    (standardize(grade) | schoolid),
  data = mlmRev::egsingle
)
# Didn't work :(

# We can also fit the model using complex random intercepts (CRI), per Scandola
# & Tidoni (2024) https://doi.org/10.1177/25152459231214454
#
# This means going from this (x is a numeric predictor and f is a factor):
y ~ x * f + (1 + x * f | g)
# becomes:
y ~ x * f + (1 | g1) + (1 + x | g1:f)

# Since out model has only one numeric predictor, we cannot demonstrate this,
# if we included "female":
math ~ grade + black + (grade | childid:schoolid) + (grade + black | schoolid)
# CRI would be:
math ~ grade +
  black +
  (grade | childid:schoolid) +
  (grade | schoolid) +
  (1 | schoolid:black)

# Models with CRIs uses a different number of degrees of freedom, so if you're
# comparing models make sure they are all using CRI or all NOT using CRI.
#
# CRIs also has a disadvantage of making interpretation of random effects more
# difficult, but it has the advantage of being less computationally expensive
# and so also much (much) faster.

## 4. Reduce the random effects structure ----------------

# We've worked really hard all semester to properly specify the maximal random
# effects structure a-la Barr et al. (2013), so we would NOT want to actually
# drop any random effects (slopes or intercepts), but what we can do is to drop
# random covariances (i.e., the correlations between random effects).

# We've seen that specifying independent random effects can be done with the ||
# (double bars) in our second lesson, but a more general form of this would be
# to use diag() which also supports factors:
mod_diag <- lmer(
  math ~ grade + diag(grade | childid:schoolid) + diag(grade | schoolid),
  data = mlmRev::egsingle
)
# It worked!

## Compare estimates and SEs across models ----------------

parameters::compare_parameters(
  mod,
  mod_no.derivs,
  mod_change.optim,
  mod_standardized,
  mod_diag,

  select = "{estimate} ({se})"
) |>
  print(digits = 4)
# We can see that all methods (even those that didn't eliminate convergence
# issues) give very similar estimates and standard errors.

## 5. Bayes! ----------------

# Bayes is much more robust to these convergence issues (and also has many many
# more advantages), so really this is a great time to learn Bayesian
# modelling...

# You can use {brms}, which is a bit more involved (but IMO totally worth it):
brms::brm(
  math ~ grade + (grade | childid:schoolid) + (grade | schoolid),
  data = mlmRev::egsingle
  # prior = ... # YOU WOULD NEED TO SPECIFY PRIORS FOR THIS MODEL!
)


# Or you can use {rstanarm}, which is a bit more plug-and-play (but also less
# flexible):
rstanarm::stan_glmer(
  math ~ grade + (grade | childid:schoolid) + (grade | schoolid),
  data = mlmRev::egsingle
  # prior = ... # YOU WOULD NEED TO SPECIFY PRIORS FOR THIS MODEL!
)
