
library(tidyverse)

library(lmerTest)
library(performance)

# Assumptions for (G)LMMs ---------------------------
# Same as for linear models, with two additions:

data("sleepstudy", package = "lme4")

mod <- lmer(Reaction ~ Days + (Days | Subject), 
            data = sleepstudy)

## +1. Homogeneity of level 1 variance ---------------
# For GLMMs, this is also called overdispersion:
# check_overdispersion(mod)

# Here, we will "test" with a visual inspection.
sleepstudy$resid <- residuals(mod)

ggplot(sleepstudy, aes(Subject, resid ^ 2)) + 
  geom_point() + 
  stat_summary(geom = "point", size = 3, color = "red") + 
  scale_y_continuous(name = expression((y[i] - hat(y)[i]) ^ 2), 
                     labels = scales::label_comma(),
                     transform = scales::transform_sqrt()) + 
  theme_bw()


# It looks like some subjects have more within-person (level 1) variance.


# Dealing violations:
# 1. Maybe we need a GLMM?
# 2. Going GAMLSS - modeling level-1 variance.
#   - with {brms} (Bayesian)
#   - with {gamlss} (Frequentist)

## +2. Random effects are normally distributed -------------

check_model(mod, check = "reqq")
# Produces QQ-plots for random effects. 
# We expect the dots to follow the green line (in this case, they do).


# Dealing violations:
# - Probably not so impactful... 

## Check all... ----------------------

check_model(mod)

