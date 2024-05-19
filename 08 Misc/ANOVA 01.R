
library(dplyr)
library(lmerTest)
library(datawizard)

# Data --------------------------------------------------------------------

?afex::obk.long

data("obk.long", package = "afex")

head(obk.long)



# ANOVA table -------------------------------------------------------------

# To produce valid type-3 ANOVA tables with proper F-tests (or Chi-square tests)
# we need to fit the model with all of our predictors mean-centered.
# - For continuous predictors we can do this by subtracting the grand mean.
obk.long$age_c <- center(obk.long$age)
# - For categorical predictors we can do this by using effects coding dummy
#   variables.
contrasts(obk.long$treatment) <- contr.sum
contrasts(obk.long$gender) <- contr.sum
contrasts(obk.long$phase) <- contr.sum
# Unfortunately this makes the regression coefficients harder to interpret, but
# we can use followup contrasts and simple effects instead (see below).
# Learn more here:
# https://shouldbewriting.netlify.app/posts/2021-05-25-everything-about-anova/



mod <- lmer(value ~ treatment * (phase + gender + age_c)  + (phase | id),
            data = obk.long)


# We can obtain an Type-3 ANOVA table using the {car} package:
car::Anova(mod, type = 3, test.statistic = "F")
# Note: we didn't include _all_ the interactions.


# For GLMMs, we can only set test.statistic = "Chisq" (default). This will give
# an Analysis of Deviance Table, but everyone still seems to call it an ANOVA
# table or an ANOVA-like table.



## Using `mixed()` ---------------------------

# The {afex} package has a convenient function to produce the correct dummy
# coding and type 3 ANOVA table in one go:

library(afex)
a <- mixed(value ~ treatment * (phase + gender + age_c)  + (phase | id),
           data = obk.long, 
           # See ?mixed
           type = 3,
           method = "S",
           check_contrasts = TRUE)
# This can take (A LOT) longer to run, but tends to gives more accurate results
# than what we did above. (Read the docs about the various methods for LMMs and
# GLMMs.)

a




# Follow-up ---------------------------------------------------------------
# Contrasts, simple effects, etc. can all be carried out with {emmeans}, just
# like with a standard ANOVA. See
# https://github.com/mattansb/Analysis-of-Factorial-Designs-foR-Psychologists

library(emmeans)
emm_options(lmer.df = "S")

joint_tests(mod, by = "gender")

em.int <- emmeans(mod, ~ treatment + phase)
em.int

contrast(em.int, method = "consec", by = "treatment")

# etc...


## With link function ------------------------------

# When we have a link function, emmeans will (by default) extract estimates on
# the link-scale. If we want emmeans to back-transform the response, we need to
# set `regrid = "response"`.

emmeans(mod2, ~ treatment, regrid = "response")
# Etc...
