                     ### 2 - Model Comparison ###

library(dplyr)
library(ggplot2)

library(lmerTest)

library(performance)
library(parameters)


# Setup --------------------------------------------------------------------

# The data: 
# 101 older adults were measured once a day, over a period of six days Each day
# they preformed the same cognitive task The outcome - processing speed (average
# RT across trials) The purpose of the study: to assess individual differences
# in short-term learning; individual trajectories for the change in RT across
# the six occasions.

temp <- tempfile()
download.file("https://www.pilesofvariance.com/Chapter3b/SPSS/SPSS_Chapter3b.zip", temp)
dataset <- haven::read_sav(unz(temp, "SPSS_Chapter3b/SPSS_Chapter3b.sav"))
unlink(temp)


dataset <- dataset |> 
  mutate(
    PersonID = factor(PersonID) # Convert to factor
  )



head(dataset) # this is already in a stacked\long format - cool! 




## Understanding the data --------------------

# The outcome is "rt"
# What is the random grouping variable? Why?
# What level is "session" on? Person (2)? Measurement (1)?

dataset |> 
  filter(PersonID == "101")




## Spaghetti plot --------------------------------------


# Change in rt by session:
ggplot(dataset, aes(session, rt, color = PersonID))+
  geom_line(show.legend = FALSE) +   
  stat_smooth(method = "lm", se = FALSE, 
              color = "black", linewidth = 2) + 
  scale_x_continuous("Session", breaks = 1:6) + 
  scale_y_continuous("Reaction Time", labels = scales::label_comma()) + 
  theme_bw() + 
  labs(caption = glue::glue("Data from {N} people, over 6 sessions.",
                            N = nlevels(dataset$PersonID)))


# Which effect do you think you see?





# We are interested in the maximal model:
rt ~ 1 + session + (1 + session | PersonID)

# We *could* just fit that model, but we will build up to it, and see what we
# can learn along the way...



# Intercept models -----------------------------------------------------------

# The most simple model- BP empty model:
mod_empty <- lm(rt ~ 1,
                data = dataset)
summary(mod_empty)



# The random intercepts model adds... random intercepts to the empty model:
mod_rndm.intr <- lmer(rt ~ 1 + (1 | PersonID),
                      data = dataset)

model_parameters(mod_rndm.intr, ci_method = "satterthwaite") # or just ci_method = "s"
model_parameters(mod_rndm.intr, ci_method = "kenward") # or just ci_method = "kr"


# This model's equations (using Hoffman's notation):

# Level-1 equation: 
#     RT_ji = b_0j + eji
#
# Level-2 equation: 
#     b_0j = gamma_00 + U_0j
#
# Composite equation: 
#     RT_ji = (gamma_00 + U_0j) + eji
#   Reaction Time of person j during obs. i = sample's mean (fixed intercept) +
#   the individual's deviation from the mean intercept + error for person j in
#   obs. i.




# What is the best way to compare these BP and WP empty models?
# - Are they nested? Yes.
# - Were they fit using the same method? No - lm() uses OLS (equivalent to ML) 
#   while lmer() uses REML be default.
# - Should we refit lmer(REML = FALSE)? No - the models differ in their random 
#   effects, so we NEED REML!



# Solution:
ranova(mod_rndm.intr)
# Removes the random intercept (constrains the model) in the REML setting.

# Conclusion - the random intercept model results in better fit.

# Had we looked at the ICC before testing, we would have expected this result:
icc(mod_rndm.intr)

# Report: ICC=0.82, X2(1)=691.74, p<.001




# What about this?
r2(mod_rndm.intr)
# In a random intercepts only model, the conditional R2 is the same as the ICC!
#
# Why is the marginal R2=0?


# Fixed effect for session ----------------------------------------
# (session - level 1 continuous predictor)

# In terms of FIXED effects - here we will add session to the intercept.
# Session is a level 1 predictor, so we have few options when adding session:
# A) adding session only as fixed.
# B) adding session as fixed and random.
# C) adding session as fixed and random + the co-variation of random effects.


## Fit model (A) ========
# FIXED EFFECTS: intercept, session
# RANDOM EFFECTS: intercept 

# Level-1 equation: 
#     RT_ji =  b_0j + b_1j * session_ij + eji 
#
# Level-2 equations: 
#     b_0j = gamma_00 + U_0j   (same as before )
#     b_1j = gamma_10
#
# Composite: 
#     RT_ji =  (gamma_00 + U_0j) + gamma_10 * session_ij + eji 
#   RT of person j during obs. i= sample's mean RT + general session effect *
#   session value at obs. i  + the individual difference in person j's intercept
#   + error for person j in obs. i

mod_fixd.sssn <- lmer(rt ~ session + (1 | PersonID),
                      data = dataset)
# (rt ~ session is the same as rt ~ 1 + session)
model_parameters(mod_fixd.sssn, ci_method = "S")



# Q: The fixed effect for session is significant - how can we interpret this?





## Fit model (B)  ===============
# FIXED EFFECTS: intercept, session
# RANDOM EFFECTS: intercept, session (adding a random slope)

# Level-1 equation: 
#     RT_ji =  b_0j + b_1j * session_ij + eji 
#
# Level-2 equations: 
#     b_0j = gamma_00 + U_0j   (same as before )
#     b_1j = gamma_10 + U_1j
#
# Composite: 
#     RT_ji =  (gamma_00 + U_0j) + (gamma_10 + U_1j) * session_ij + eji 
#   Person j's RT in obs. i= sample's mean (fixed intercept) + (Session effect
#   based on the general + individual effects) * session value at obs. i + the
#   individual difference in person j mean + error for person j in time i.

# Do you see the person-by-session interaction?

mod_rndm.sssn <- lmer(rt ~ session + (1 | PersonID) + (0 + session | PersonID),
                      data = dataset)

# Or use || do "remove" random covariance
mod_rndm.sssn <- lmer(rt ~ session + (session || PersonID),
                      data = dataset)





## Fit model (C) ============
# FIXED EFFECTS: intercept, session
# RANDOM EFFECTS: intercept, session, and their COV

# Hoffman's notation can't show cov between random effects, so the model's
# equations will be identical to those of model (B). But we can write out the
# full notation - which I will do on the board...

mod_full <- lmer(rt ~ session + (session | PersonID),
                 data = dataset)



# let's see the full model's parameters:
model_parameters(mod_full, ci_method = "S")


## Models comparison ----------------------------------------------------------

# Now let's compare these models.
# REMEMBER - the default behavior of lmer() is REML = TRUE

# Compare the models:
sjPlot::tab_model(mod_rndm.intr, mod_fixd.sssn, mod_rndm.sssn, mod_full,
                  show.se = TRUE,
                  df.method= "satterthwaite")





### compare: mod_rndm.intr VS mod_fixd.sssn

# - Are they nested? 
#   Yes (mod_rndm.intr is nested within mod_fixd.sssn)
# - Are they fit using the same method? 
#   Yes. But they both use REML, but only differ in their fixed effects! 


# Solution:
anova(mod_rndm.intr, mod_fixd.sssn, refit = TRUE)
# refit = TRUE will refit the models to ML before comparing them!
# This is actually the default (we don't have to use this argument)
#
# We can see these also produce AIC and BIC values.


# If you want to check it through Bayesian lenses:
bayestestR::bayesfactor_models(mod_fixd.sssn, denominator = mod_rndm.intr,
                               estimator = "ML") 
# estimator = "ML" is also the default!





### compare: mod_fixd.sssn VS mod_rndm.sssn:

# - Are they nested?
#   Yes (mod_fixd.sssn is nested within mod_rndm.sssn)
# - Are they fit using the same method? 
#   Yes - REML, and they differ on random effects, so no need to refit

anova(mod_fixd.sssn, mod_rndm.sssn, refit = FALSE)

bayestestR::bayesfactor_models(mod_rndm.sssn, denominator = mod_fixd.sssn,
                               estimator = "REML") 

# Conclusion: the change over session is heterogeneous!






### compare: mod_rndm.sssn VS mod_full:

# - Are they nested? 
#   Yes (mod_rndm.sssn is nested within mod_full)
# - Are they fit using the same method? 
#   Yes - REML, and they differ on random effects, so no need to refit

anova(mod_rndm.sssn, mod_full, refit = FALSE)
# What does this mean?

model_parameters(mod_full, ci_method = "S", ci_random = TRUE)
# Look at the correlation


# To understand this correlation, let's look at a plot:
random_effects <- coef(mod_full)[["PersonID"]]
head(random_effects)
ggplot(random_effects, aes(`(Intercept)`, session)) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  theme_bw() + 
  scale_x_continuous(expression(Intercept[j]), labels = scales::label_comma()) + 
  labs(y = expression(Session[j]))
# What is the intercept here?



# What can we learn from these?
icc(mod_full)
r2(mod_full)
# The model explains very little variance in the population (fixed effects).
# It seems that A LOT of the variance in due to heterogeneity in the random
# effects, specifically in the random intercepts (as we saw with the
# unconditional ICC).



# Exercise --------------------------------------------------------------------

# Consider the following Data:
dataset2 <- read.csv("Example- Couples relational Satisfaction.csv")
head(dataset2)

# This Data includes 100 heterosexual romantic couples.
# The Variables:
# -> ID here is not per person, but - per couple!
# -> Two measurements of relationship satisfaction ('sat') - For males (M_sat) 
#    and for females (F_sat)
# -> Duration of the relationship in years
# * M_sat, F_sat and Duration variables were centered and this is why we have 
#   both pos and neg values. (Only 100 obs. from the total sample appear here.
#   Centering was preformed on a bigger sample, this is why the variables mean
#   are not zero. Though, it isn't important to our use)

# Note 1- don't forget to prepare the data in a stacked\long format!
#         Hint- think of Gender as the a binary variable (like time 0
#         and time 1 in the first tutorial with the "2 occasions data")
# Note 2- please use lm()\ lmer() and not other functions, 
#         (even if it is possible for these models) 



# Don't forget to interpret your results as you go!

# 1) Identify the design: the random grouping variable, levels, DV, and
#    predictors and their levels.
# 2) build 2 models:
# Model A: empty model for predicting satisfaction.
#          ?. What does the intercept estimate mean?
# Model B: Model A + random intercepts per COUPLE.
#          ?. Compute the ICC. What does it means?
#          (The first tutorial's script may help you with this part!)
# 3) Compare models A and B using the right indices (which)
# 4) Build 2 more models:
# Model C: Model B + fixed effect for gender + fixed effect for duration.
# Model D: Model C + an interaction between gender and duration.
# 5) Compare model C to B and model D to C- each time using the right indices.
# 6) gender is a level one predictor - can we add a random slope per couple?


