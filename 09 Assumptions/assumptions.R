
library(tidyverse)

library(lmerTest)

library(performance)
# library(DHARMa)


# Assumptions for (G)LMMs ---------------------------
# Same as for linear models, with two additions:

data("sleepstudy", package = "lme4")

mod <- lmer(Reaction ~ Days + (Days | Subject),
            data = sleepstudy)



## +1. Homogeneity of level 1 variance ---------------
# For GLMMs, this is also called overdispersion:
# check_overdispersion(mod)

# Here, we will "test" with a visual inspection.


# By prediction:
check_heteroscedasticity(mod) |> plot()
# Residuals don't seem to be related to the means they deviate from.



# By grouping variable:
sleepstudy$resid <- residuals(mod)

sleepstudy |>
  mutate(
    Subject = fct_reorder(Subject, resid ^ 2)
  ) |>
  ggplot(aes(Subject, resid ^ 2)) +
  geom_point() +
  stat_summary(geom = "point", size = 3, color = "red") +
  scale_y_continuous(name = expression((y[i] - hat(y)[i]) ^ 2),
                     labels = scales::label_comma()) +
  coord_trans(y = scales::transform_sqrt()) +
  theme_bw()
# It looks like some subjects have more within-person (level 1) variance (334,
# 308) than others...






# Dealing violations:
# 1. Maybe we need a GLMM?
# 2. Using robust standard errors:
#    See https://jepusto.github.io/clubSandwich/
# 2. Going GAMLSS - modeling level-1 variance.
#   - with {brms} (Bayesian)
#   - with {gamlss} (Frequentist)




## +2. Random effects are normally distributed -------------

check_model(mod, check = "reqq")
# Produces QQ-plots for random effects.
# We expect the dots to follow the green line (in this case, they do).


# Dealing violations:
# - Probably not so impactful (https://doi.org/10.1007/s11135-018-0802-x)...


## Check all... ----------------------

check_model(mod)
