                     ### 2 - Model Comparison ###

library(ggplot2)
library(lmerTest)
library(performance)
library(parameters)



# Part 1 - Fit some models =====================================================
# First, fitting different models for the same data (so we will have something
# to compare!)


## Data: 
# 101 older adults were measured once a day, over a period of six days Each day
# they preformed the same cognitive task The outcome -  processing speed
# (average RT across trials) The purpose of the study: to assess individual
# differences in short-term learning; individual trajectories for the change in
# RT across the six occasions (see figure 3.4)

dataset <- read.csv("Example- Aging&Cognition -6 occasions.csv")


head(dataset) # this is already in a stacked\long  format - cool! 

# Spaghetti plots- change in rt by session:
ggplot(dataset, aes(x = session, y = rt, color = factor(PersonID)))+
  geom_line(show.legend = FALSE) +   
  geom_smooth(method = "lm", se = FALSE, 
              color = "black", linewidth = 1)


# Which effect you think you see?


# We are interested in the maximal model:
rt ~ 1 + session + (1 + session | PersonID)

# How do we build up to that?



## Intercept models -----------------------------------------------------------

## The most simple model- BP empty model:
model.lm.empty <- lm(rt ~ 1,
                     data = dataset)
summary(model.lm.empty)

## The WP empty model:
# The starting point in MLM will be to estimate the WP empty model in which:
# FIXED EFFECTS  - intercept
# RANDOM EFFECTS - intercept

model.r_intercepts <- lmer(rt ~ 1 + (1 | PersonID),
                           data = dataset)

model_parameters(model.r_intercepts, ci_method = "S")
# The WP empty model's equations (using Hoffman's notation):

# level-1 equation: RTji = b0i + eji 
#                   Reaction Time of person i during obs. j = person i's
#                   intercept (person's mean RT) + residual error for person i's
#                   during obs. j (in reference to person i's intercept)
# level-2 equation: b0i = gamma00 + U0i
#                   person i's intercept = fixed intercept for sample (general/
#                   grand mean RT) + the individual difference of person i's
#                   intercept from this grand mean
# Together....
# Composite equation: RTji = gamma00 + U0i + eji
#                    Reaction Time of person i during obs. j= sample's mean
#                    (fixed intercept)+ the individual difference in person i's
#                    intercept + error for person i in obs. j




# What is the best way to compare these BP and WP empty models?
# - Are they nested? Yes.
# - Are they fit using the same method? No - lm() uses OLS (equivalent to ML) 
#   while lmer() uses REML.
# - Should be refit lmer(REML = FALSE)? No - the models differ in their random 
#   effects, so we NEED REML!



# Solution:
ranova(model.r_intercepts)
# Removes the random intercept (constrains the model) in the REML setting.

# Conclusion - the random intercept model results in better fit!

# Had we looked at the ICC before testing, we would have expected this result:
icc(model.r_intercepts)

# Report: ICC=0.82, X2(1)=691.74, p<.001


sigma(model.lm.empty)^2 # the variance in rt
sigma(model.r_intercepts)^2 # the within-person variance is reduced!
model_parameters(model.r_intercepts, ci_method = "S")
# 82% of the original remaining variance was moved into the random effect
# variance sigma2{a}, resulting in much less within-person error!
# 448.20^2 + 211.90^2 = 244122.6

# What about this?
r2(model.r_intercepts)
# In a random intercepts only model, the conditional R2 is the same as the ICC!
# Why is the marginal R2=0?


## Building up the rt ~ session model ----------------------------------------
## (session - level 1 continuous predictor)

# In terms of FIXED effects - here we will add session to the intercept.
# Session is a level 1 predictor, so we have few options when adding session:
# A) adding session only as fixed.
# B) adding session as fixed and random.
# C) adding session as fixed and random + the co-variation of random effects.


### Fit model (A) ========
#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept 

# level-1 equation: RTji =  b0i + b1i(sessionij) + eji 
#                   person i's RT in obs. j = person's mean RT + session effect
#                   for person i * session value at obs. j + residual for person
#                   i's at obs. j

# level-2 equations: b0i = gamma00 + U0i
#                       same as before 
#                    b1i = gamma10
#                       session effect for for person i is the general effect in
#                       the sample!

# Composite: RTji = gamma00 + gamma10(sessionij) + U0i + eji
#            RT of person i during obs. j= sample's mean RT+ general session
#            effect * session value at obs. j  + the individual difference in
#            person i's intercept + error for person i in obs. j

# Note - it will might be more accurate to write sessionij as the value at obs.
# j for person i, but we kept it simple (also - number of session and their order
# was fixed across participants)

model.sess <- lmer(rt ~ session + (1 | PersonID),
                   data = dataset)
model_parameters(model.sess, ci_method = "S")
# (rt ~ session is the same as rt ~ 1 + session)

# Q: The fixed effect for session is significant - how can we interpret this?


### Fit model (B)  ===============

#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept, session (adding a random slope)

## The model's equations:

# level-1 equation:  RTji =  b0i + b1i(sessionij) + eji <- same as before
# level-2 equations: b0i = gamma00 + U0i <- same as before
#                    b1i = gamma10 + U1i
#                    [Session effect for person i = the general session effect
#                    in the sample + the individual deviation effect in session
#                    for person i]

# Composite: RTji = gamma00 + (gamma10 + U1i)(sessionij) + U0i + eji
#            [person i's RT in obs. j= sample's mean (fixed intercept)+ (Session
#            effect based on the general+ individual effects) * session value at
#            obs. j + the individual difference in person i mean + error for
#            person i in time t]


model.r_sess_no_cov <- lmer(rt ~ session + (session || PersonID),
                            # || for not modelling the random COV
                            data = dataset)

### Fit model (C) ============

#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept, session, and their COV

# Hoffman's notation can't show cov between random effects, so the model's
# equations will be identical to those of model (B). But we can write out the
# full notation - which I will do on the board...

model.maximal <- lmer(rt ~ session + (session | PersonID),
                      # | for letting the random effect covary
                      data = dataset)



# let's see the models' parameters:
model_parameters(model.sess, ci_method = "S")
model_parameters(model.r_sess_no_cov, ci_method = "S")
model_parameters(model.maximal, ci_method = "S")

sjPlot::tab_model(model.sess, model.r_sess_no_cov, model.maximal,
                  df.method= "satterthwaite")

?sjPlot::tab_model


## Models comparison ----------------------------------------------------------

# Now let's compare these models.
# REMEMBER - the default behavior of lmer() is REML = TRUE


### compare: model.r_intercepts VS model.sess:

# model.r_intercepts
#    FIXED EFFECTS: intercept
#    RANDOM EFFECTS: intercept 

# model.sess:
#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept 

# - Are they nested? 
#   Yes (model.r_intercepts is nested within model.sess)
# - Are they fit using the same method? 
#   Yes. But they both use REML, but only differ in their fixed effects! 


# Solution:
anova(model.r_intercepts, model.sess, refit = TRUE)
# refit = TRUE will refit the models to ML before comparing them!
# This is actually the default (we don't have to use this argument)

# If you want to check it through Bayesian lenses:
bayestestR::bayesfactor_models(model.sess, denominator = model.r_intercepts) 


### compare: model.sess VS model.r_sess_no_cov:

# model.sess:
#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept 

# model.r_sess_no_cov
#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept , session

# - Are they nested?
#   Yes (model.sess is nested within model.r_sess_no_cov)
# - Are they fit using the same method? 
#   Yes - REML, and they differ on random effects, so no need to refit

anova(model.sess, model.r_sess_no_cov, refit = FALSE)


#>>> conclusion- the random slope model is better!


### compare: model.r_sess_no_cov VS model.maximal:

# model.r_sess_no_cov:
#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept, session

# model.maximal:
#    FIXED EFFECTS: intercept, session
#    RANDOM EFFECTS: intercept, session and their COV

# - Are they nested? 
#   Yes (model.r_sess_no_cov is nested within model.maximal)
# - Are they fit using the same method? 
#   Yes - REML, and they differ on random effects, so no need to refit

anova(model.r_sess_no_cov, model.maximal, refit = FALSE)
# What does this mean?

model_parameters(model.maximal, ci_method = "S")
# Look at the correlation

random_effects <- coef(model.maximal)[["PersonID"]]
head(random_effects)
ggplot(random_effects, aes(`(Intercept)`, session)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed")
# What is the intercept here?


# What can we learn from these?
icc(model.maximal)
r2(model.maximal)
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

## PART 1: 

# First, build 2 models:
  # Model A- empty model for predicting satisfaction.
  #          What does the intercept estimate means?
  # Model B- the same as the previous model- just set the intercept as random.
  #          + compute the ICC. what does it means?
  #          (The first tutorial's script may help you with this part!)
# Second,
  # Compare models A and B using the right indices (which)




## PART 2: 

# First, build 3 models:
  # Model C- predicting satisfaction from gender (not controlling for
  #          couple\ID)- a simple BP model
  # Model D- predicting satisfaction from gender (while controlling for
  #          couple\ID)- this one is a WP model. Even though we don't have
  #          multiple obs. of the same person, we have two obs. for the same
  #          couple! use this logic.
  # Model E- predicting satisfaction from the duration*gender interaction (while
  #          controlling for couple\ID)

# Second,
  # Compare the 3 models using the right indices (which?)


