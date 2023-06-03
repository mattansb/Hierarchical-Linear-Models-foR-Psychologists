library(dplyr)
library(lme4)


# Data --------------------------------------------------------------------

# Data is from a Stroop task with Ego depletion. See:
?afex::stroop

data(stroop, package = "afex")

stroop_1 <- stroop |> 
  filter(
    study == "1",
    pno %in% paste0("s1_", 1:10),# use only 10 subjects for ease of example
    acc == 1
  )

# Fit the model -----------------------------------------------------------

# According to Barr et al (2014) https://doi.org/10.1016%2Fj.jml.2012.11.001
# the random effects structure should be maximal as per the study design.
# Here that mean (condition * congruency | pno)

# According to Lo and Andrews (2015) https://doi.org/10.3389/fpsyg.2015.01171
# we should analyze RTs with a inverse-Gaussian likelihood, using an identity
# link function.
# Here that mean using `family = inverse.gaussian("identity")`
#
# This is by no means the only option - and there are many other methods
# available. See: https://lindeloev.github.io/shiny-rt/

m_inv.gaus <- glmer(rt ~ condition * congruency + (condition * congruency | pno),
                    family = inverse.gaussian("identity"),
                    data = stroop_1)


## Different likelihoods... --------------------


library(performance)
library(see)
library(ggplot2)
library(patchwork)


# Is the inverse-Gaussian a good choice here?
# Let's preform a predictive check with other families. See
?posterior_predictive_check


# Let's compare to the standard-Gaussian (normal) model:
m_gauss <- lmer(rt ~ condition * congruency + (condition * congruency | pno),
                data = stroop_1)

(plot(posterior_predictive_check(m_inv.gaus)) + ggtitle("Inverse Gaussian")) / 
  (plot(posterior_predictive_check(m_gauss)) + ggtitle("Gaussian (Normal)")) + 
  plot_layout(guides = "collect")

# We can see that the inverse-Gaussian model fits better than the Gaussian
# model. Not perfect, but better.



# Another popular alternative is the log-normal likelihood, which can be fit by transforming the RTs.
m_lnorm <- lmer(log(rt) ~ condition * congruency + (condition * congruency | pno),
                data = stroop_1)


(plot(posterior_predictive_check(m_inv.gaus)) + ggtitle("Inverse Gaussian")) / 
  (plot(posterior_predictive_check(m_gauss)) + ggtitle("Gaussian (Normal)")) + 
  (plot(posterior_predictive_check(m_lnorm)) + ggtitle("log-Normal")) + 
  plot_layout(guides = "collect")

# This seems to fit even better!










# ANOVA table -------------------------------------------------------------
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



m_inv.gaus2 <- glmer(rt ~ condition * congruency + (condition * congruency | pno),
                     family = inverse.gaussian("identity"),
                     data = stroop_1)


# We can obtain an Type-3 ANOVA table using the {car} package:
car::Anova(m_inv.gaus2, type = 3)



# This is actually an Analysis of Deviance Table (because we are using a
# glm(m)), but everyone still seems to call it an ANOVA table or an ANOVA-like
# table.



## Using `mixed()` ---------------------------

# The {afex} package has a conviniant function to produce the correct dummy coding and
# type 3 ANOVA table in one go:

library(afex)
a <- mixed(rt ~ condition * congruency + (condition * congruency | pno),
           family = inverse.gaussian("identity"),
           data = stroop_1, 
           # See ?mixed
           type = 3,
           method = "LRT",
           check_contrasts = TRUE)
# This takes longer to run, but gives more accurate results than what we did
# above.

a




# Follow-up ---------------------------------------------------------------

# We can run any followup simple effects or contrasts with emmeans:
library(emmeans)

joint_tests(m_inv.gaus, by = "condition") # Note that when df2 = Inf -> Chisq = F.ratio * df1

em.int <- emmeans(m_inv.gaus, ~ congruency)
em.int

contrast(em.int, method = "revpairwise")

# etc...


## With link function ------------------------------

# When we have a link function, we need to tell emmeans to back-transform the
# response with `regrid = "response"`

em.int2 <- emmeans(m_lnorm, ~ congruency, regrid = "response")
em.int2

# Compare to:
em.int

contrast(em.int2, method = "revpairwise")








# Exercise ----------------------------------------------------------------

# Fit a Gaussian and a log-normal models with effects coding and compare the
# resulting type-3 ANOVA tables to the one obtained with the inverse-Gaussian
# model.
# - Do the results differ? Do you conclusions change?
# - Do the estimated means differ?

