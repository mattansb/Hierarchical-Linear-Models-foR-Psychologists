
library(dplyr)
library(ggplot2)
library(patchwork)

library(lmerTest)

library(performance)
# library(statmod)
library(see)
library(emmeans)
emm_options(lmer.df = "S")



# The data --------------------------------------------------------------------

# Data is from a Stroop task with Ego depletion. See:
?afex::stroop

data(stroop, package = "afex")

stroop_1 <- stroop |> 
  filter(
    pno %in% paste0("s1_", 1:10),# use only 10 subjects for ease of example
    acc == 1
  )

head(stroop_1)


# The maximal model -----------------------------------------------------------

# According to Barr et al (2014) https://doi.org/10.1016%2Fj.jml.2012.11.001
# the random effects structure should be maximal as per the study design.
# Here that mean (condition * congruency | pno).

# According to Lo and Andrews (2015) https://doi.org/10.3389/fpsyg.2015.01171
# we should analyze RTs with a inverse-Gaussian likelihood, using an identity
# link function.
# Here that mean using `family = inverse.gaussian("identity")`
#
# This is by no means the only option - and there are many other methods
# available. See: https://lindeloev.github.io/shiny-rt/

mod_inv.gaus <- glmer(rt ~ condition * congruency + (condition * congruency | pno),
                      family = inverse.gaussian(link = "identity"),
                      control = glmerControl("nlminbwrap"),
                      data = stroop_1)


## Different likelihoods... --------------------


# Is the inverse-Gaussian a good choice here?
# Let's preform a predictive check with other families. See
?check_predictions


# Let's compare to the standard-Gaussian (normal) model:
mod_gauss <- lmer(rt ~ condition * congruency + (condition * congruency | pno),
                  data = stroop_1)

# Another popular alternative is the log-normal likelihood, which can be fit by transforming the RTs.
mod_lnorm <- lmer(log(rt) ~ condition * congruency + (condition * congruency | pno),
                  control = lmerControl("bobyqa"),
                  data = stroop_1)


(plot(check_predictions(mod_inv.gaus)) + ggtitle("Inverse Gaussian")) / 
  (plot(check_predictions(mod_gauss)) + ggtitle("Gaussian (Normal)")) + 
  (plot(check_predictions(mod_lnorm)) + ggtitle("log-Normal")) + 
  plot_layout(guides = "collect") &
  coord_cartesian(xlim = c(0, 2))
# We can see that the inverse-Gaussian model fits better than the Gaussian
# model. Not perfect, but better, and the log-normal model fits best.
# However, the log-normal uses a transformed response, making interpretation of
# effect and especially interaction difficult.





## ANOVA table -------------------------------------------------------------
# (Let's stick to the inverse-Gaussian model for now.)


# To produce type-3 ANOVA tables with proper F-tests (or Chi-square tests) we
# need to fit the model with all of our predictors mean-centered.
# - For continuous predictors we can do this by subtracting the grand mean.
# - For categorical predictors we can do this by using effects coding dummy
#   variables.
contrasts(stroop_1$condition) <- contr.sum
contrasts(stroop_1$congruency) <- contr.sum
# Unfortunately this makes the regression coefficients harder to interpret, but
# we can use followup contrasts and simple effects instead (see below).
# Learn more here:
# https://shouldbewriting.netlify.app/posts/2021-05-25-everything-about-anova/


mod_inv.gaus2 <- glmer(rt ~ condition * congruency + (condition * congruency | pno),
                       family = inverse.gaussian("identity"),
                       control = glmerControl("bobyqa"),
                       data = stroop_1)


# We can obtain an Type-3 ANOVA table using the {car} package:
car::Anova(mod_inv.gaus2, type = 3)
# This is actually an Analysis of Deviance Table (because we are using a
# GLM(M)), but everyone still seems to call it an ANOVA table or an ANOVA-like
# table.




## Follow-up ---------------------------------------------------------------
# Contrasts, simple effects, etc. can all be carried out with {emmeans}, just
# like with a standard ANOVA. See
# https://github.com/mattansb/Analysis-of-Factorial-Designs-foR-Psychologists



joint_tests(mod_inv.gaus2, by = "condition")

em.int <- emmeans(mod_inv.gaus2, ~ congruency)
em.int

contrast(em.int, method = "revpairwise")

# etc...



### With link function ------------------------------

# When we have a link function, emmeans will (by default) extract estimates on
# the link-scale. If we want emmeans to back-transform the response, we need to
# set `regrid = "response"`.

em.int2 <- emmeans(mod_lnorm, ~ congruency, regrid = "response")
em.int2

# Compare to:
em.int

contrast(em.int2, method = "revpairwise")



