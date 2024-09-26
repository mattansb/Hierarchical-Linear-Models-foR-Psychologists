          
            ## Growth models ##

library(dplyr)
library(ggplot2)

library(lmerTest)

library(parameters)
library(performance)


# The data -------------------------------------------------------------------

# Example Ch. 6 in Hoffman- same 'Aging&Cognition' 6 occasions data: 101 older
# adults measured on six occasions during a two-week span. The outcome
# - was a measure of processing speed: the average response time (RT) across
# trials (in milliseconds).

temp <- tempfile()
download.file("https://www.pilesofvariance.com/Chapter6/SPSS/SPSS_Chapter6.zip", temp)
dataset <- haven::read_sav(unz(temp, "SPSS_Chapter6/SPSS_Chapter6.sav"))
unlink(temp)

head(dataset)

# Our goal will be to model the change in RT over time (We will be examining
# 'session' as continuous variable)


## Understanding the data --------------------

# The outcome is "RT"
# What is the random grouping variable? Why?
# What level is "session" on? Person (2)? Measurement (1)?

dataset |> 
  filter(PersonID == "101")



## Descriptive statistics  ------------------------------------
# for RT at each occasion:

dataset |> 
  group_by(session) |> 
  summarise(
    across(rt, .fns = list(length = length,
                           mean = mean,
                           sd = sd,
                           min = min,
                           max = max))
  )

# Over time, the means appear to decrease (i.e., RT improves across sessions), 
# but the rate of the decrease appears to slow down after the second session..


## Two different effects of Time ---------------------------------

# The researcher may want to ask herself:

# 1. What is the mean reaction time?
# 2. Do people differ in their RTs?
# 3. Do they get faster across sessions?
# 4. Do they differ in their improvement (or lack of)?
# Which question is answered by which type of effect?



# Two different effects of Time:
#
# (1) FIXED EFFECTS of time -
#     Is there change in the outcome over time on average?
#     (average changes across subjects)
#
# (2) RANDOM SLOPES of time - 
#     Does everybody change the same? (individual changes / heterogeneity)
#     In ANOVA terms, this is the subject*time interaction - individual
#     differences in the effect of predictor x (e.g. time) on the outcome.
#
# COOL VISUALIZATION: http://mfviz.com/hierarchical-models/



## Spaghetti Plots helps to consider the pattern of the data:

## Spaghetti plot: 
ggplot(dataset, aes(session, rt, color = factor(PersonID)))+
  geom_line(show.legend = FALSE) +   
  stat_smooth(se = FALSE, 
              color = "black", linewidth = 2) + 
  scale_x_continuous("Session", breaks = 1:6) + 
  scale_y_continuous("Reaction Time", labels = scales::label_comma()) + 
  theme_bw() + 
  labs(caption = glue::glue("Data from {N} people, over 6 sessions.",
                            N = nlevels(factor(dataset$PersonID))))
  
# Seems linear? Maybe not...

# Note: The black line describe the smoothed average pattern across time. We
# didn't forced it to be a linear line as we did the first time we saw this
# example...


# Now, looking at this plot, will it make sense to model:
# -> the intercept as fixed? 
# -> the intercept as random? 
# -> time as fixed?
# -> time as random? 



## Preparation for modeling --------------------------------------------------

## A. Considering the 0 time point:

# An important consideration prior to defining models of change is where time 0
# should be. For the sake of interpretability, it should be meaningful!
# Generally, the location of time 0 should exist in your observed data. The
# question to ask yourself: At what point in time do you want a snapshot of
# individual differences? This will be T0. We will choose to snapshot these
# differences in the first session

# Making time 0 point meaningful- the first occasion.
dataset <- dataset |> 
  mutate(
    PersonID = factor(PersonID),
    time = session - 1
  )










# Building the linear growth model -------------------------------------------

# We will build the models that were presented in chapter 6.\ we saw in the
# "Model comparison" tutorial.

# We will build our model sequentially, step-by-step, to better understand the
# various fixed and random effects. When we do this (which we don't have to),
# some things to keep in mind:
# 1. Fixed effects will always be added before their corresponding random slopes
#    since random slopes are like interactions / the fixed effects are what
#    those random effects would be deviating from.
# 2. Random effects/slopes will be tested (when possible). 
# 3. Variances of random effects (and their covariances with random effects
#    already in the model) will be added one at a time so that the unique
#    contribution of each can be evaluated with a -2LL test\AIC\BIC.



## Random intercepts model ---------------------------------------------------

# FIXED EFFECTS: intercept
# RANDOM EFFECTS: intercept

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


mod_rndm.intr <- lmer(rt ~ 1 + (1 | PersonID),
                      data = dataset)
model_parameters(mod_rndm.intr, ci_method = "S")

icc(mod_rndm.intr)
# 81.7% of the variance in the RT outcome is BP variance in mean RT.



## Fixed effect time -------------------------------------


# FIXED EFFECTS: intercept, time
# RANDOM EFFECTS: intercept

# Level-1 equation: 
#     RT_ji =  b_0j + b_1j * time_ij + eji 
#
# Level-2 equations: 
#     b_0j = gamma_00 + U_0j   (same as before )
#     b_1i = gamma_10
#
# Composite: 
#     RT_ji =  (gamma_00 + U_0j) + gamma_10 * time_ij + eji 
#   RT of person j during obs. i= sample's mean RT + general time effect *
#   time value at obs. i  + the individual difference in person j's intercept
#   + error for person j in obs. i
  
mod_fixef.time <- lmer(rt ~ time + (1 | PersonID), 
                       data = dataset)
model_parameters(mod_fixef.time, ci_method = "S")

# mean trajectory for RT that starts at fixed intercept of gamma00 = 1,899.6 
# at session 1 and decreases linearly over time by fixed linear slope of 
# gamma10 = -51.6 per session.
# It is also significant - what can we _infer_ here?

# Time is sig. and all indices also suggest it improves fit:
anova(mod_fixef.time, mod_rndm.intr)




## Random slopes for time ---------------------------------------


# FIXED EFFECTS: intercept, time
# RANDOM EFFECTS: intercept, time

# Level-1 equation: 
#     RT_ji =  b_0j + b_1j * time_ij + eji 
#
# Level-2 equations: 
#     b_0j = gamma_00 + U_0j   (same as before )
#     b_1i = gamma_10 + U_1j
#
# Composite: 
#     RT_ji =  (gamma_00 + U_0j) + (gamma_10 + U_1j) * time_ij + eji 
#   Person j's RT in obs. i= sample's mean (fixed intercept) + (time effect
#   based on the general + individual effects) * time value at obs. i + the
#   individual difference in person j mean + error for person j in time i.


# We can model the cov between random effects (as in the book):
mod_rndm.time <- lmer(rt ~ time + (time | PersonID),
                      data = dataset)
model_parameters(mod_rndm.time, ci_method = "S", ci_random = TRUE)
# There seems to be a negative correlation... How would we interpreter it?


# To test the significance of the addition of the random linear time slope 
# variance (and its covariance with the random intercept), we can do a deviance 
# difference test against the fixed linear time:
anova(mod_rndm.time, mod_fixef.time, refit = FALSE)
ranova(mod_rndm.time) # or...



# Quadratic models (that's new!) ----------------------

# A unique feature of polynomial models is that they are hierarchical such that
# the quadratic effect (time^2) is higher-order than the linear effect (time),
# and the intercept is the lowest-order term in the model.
# -> we will build our models accordingly.

# Polynomial models are common approach for modeling nonlinear change because of
# their relatively low data requirements, ease of estimation, and wide-spread
# availability. However, because they have limited theoretical utility, it is
# important to be aware of alternative models whose parameters might be more
# meaningful and therefore useful in describing change over time (for now,
# beyond our scope. see chapter 6).

# First option within the polynomial family - Quadratic modeling:
# A single bend trajectories in which the rate of change appears to change only
# once.



## Fixed quadratic, random linear time model ----------------------------------

# FIXED EFFECTS: intercept, time, time^2
# RANDOM EFFECTS: intercept, time

mod_fixed.poly2 <- lmer(rt ~ poly(time, 2, raw = TRUE) + (time | PersonID),
                        data = dataset)
# This syntax will give us the same:
#     rt ~ time + I(time ^ 2) + (time | PersonID)
model_parameters(mod_fixed.poly2, ci_method = "S")
# raw = TRUE is important - if set to FALSE (default) it centers the predictor
# and changes the interpretation of the coefs (but not the model fit).


# The fixed quadratic time slope Gamma20 is significant + 
# this model presents better fit:
anova(mod_fixed.poly2, mod_rndm.time)

  
# SOME NOTEs:
#
# The quadratic effect of time is often labeled as the rate of acceleration or
# deceleration. However, its' estimated coefficient is actually only half of the
# rate ->
# Acceleration\ deceleration- how the fixed linear effect of time changes per
# unit time, which is given by twice the fixed quadratic effect of time:
#   2*13.87=27.74
#
# After including a fixed quadratic effect of time, the fixed linear effect of
# time is now also conditional, such that it becomes the instantaneous linear
# rate of change specifically at time 0. Thus, at session 1 (time 0), RT
# decreases by gamma10 = -120.9 per session. The positive fixed quadratic time
# slope creates a decelerating negative trajectory, such that the negative
# linear rate of change will become less negative per session by 27.8.




# Again, the second degree is significant - how can we interpret this?


# So, the decline in RT slows down across sessions on average, but do people
# differ in their rates of deceleration...?



  
## Full random quadratic time model ------------------------------------------
# A random quadratic effect of time is a person-specific deviation from the
# fixed quadratic effect!


# FIXED EFFECTS: intercept, time, time^2
# RANDOM EFFECTS: intercept, time, time^2

mod_rndm.poly2 <- lmer(rt ~ poly(time, 2, raw = TRUE) +
                         (poly(time, 2, raw = TRUE) | PersonID),
                       data = dataset)
# We get a convergence warning! 
# This means the model failed to properly estimate its parameters - The model's
# estimates cannot be trusted!
?allFit


# Let's try and fix that by using a different internal optimizer function:
mod_rndm.poly2 <- lmer(rt ~ poly(time, 2, raw = TRUE) +
                         (poly(time, 2, raw = TRUE) | PersonID),
                       control = lmerControl(optimizer = "bobyqa"),
                       data = dataset)
# Yay!


model_parameters(mod_rndm.poly2, ci_method = "S")
VarCorr(mod_rndm.poly2)
anova(mod_rndm.poly2, mod_fixed.poly2, refit = FALSE)
# These models' results are the same as in the book - go over this tutorial with
# the book.




## Simple intercepts and slopes ------------------------------------------------

# To see the PREDICTED intercepts and PREDICTED slopes
# (not the one's observed in the data)

library(marginaleffects)

plot_predictions(mod_rndm.poly2, condition = "time",
                 re.form = NA) + 
  theme_classic() + 
  scale_y_continuous("Reaction Time", labels = scales::label_comma())


# Simple intercepts in each time point (with CI)
avg_predictions(mod_rndm.poly2, variables = "time",
                re.form = NA)

# Simple slopes (instantaneous linear rate of change) at each time point
avg_slopes(mod_rndm.poly2, variables = "time", by = "time",
           re.form = NA)


# The linear rate of change is significantly negative through session 4, but by
# session 5 it is nonsignificantly negative (and is nonsignificantly positive at
# session 6). Thus, the improvement predicted by the quadratic model appears to
# ?shut off? after session 4.





# Exercise ----------------------------------------------------------------

temp <- tempfile()
download.file("https://www.intensivelongitudinal.com/ch4/ch4R.zip", temp)
dataex <- read.csv(unz(temp, "ch4R/time.csv"))
unlink(temp)

head(dataset)

# this data is from Bolger and Laurenceau (2013), chapter 4 (csv is included in
# this R project). It's simulated data that is intended to represent a study of
# wives from 50 heterosexual married couples, randomly assigned to a 16-week
# marital therapy treatment condition (n = 25) or a 16-week wait-list condition
# (n = 25). In both groups participants completed web diary on a fixed day each
# week for 15 time points. One questionnaire in this survey measured intimacy.

head(dataex)

# There are 16 lines of data for each subject, resulting in 16 * 50 = 800 rows
# in all.
# There are 4 variables: id, time, intimacy, and treatment (0 for control and 1
# for intervention).

## Spaghetti Plots
ggplot(dataex, aes(time, intimacy, colour = factor(id))) + 
  facet_grid(cols = vars(treatment), 
             labeller = as_labeller(c("0" = "Control",
                                      "1" = "Treatment"))) + 
  geom_line() +
  stat_smooth(method = 'lm', se = FALSE,
              colour = "black", linewidth = 1) +
  guides(colour = "none") + 
  theme_bw() + 
  labs(y = "Intimacy", x = "Time")
# What do you see here? which effects?



# Your mission- Build and compare the following models:
# 1. empty model for predicting intimacy
#    - find the unconditional ICC
# 2. the random linear time model for predicting intimacy (time both as fixed
#    and random)
#    - check whether there's significant heterogeneity in the effect of time.
#    - What is the correlation between intimacy at the start of the study 
#      related to the change in intimacy over time?
#    - Re-code the data to obtain the correlation between the slope of time and
#      the outcome at the *end* of the study.
#    * Bonus: is the correlation is significantly different from 0?
# 3. Add (to the random linear time model) treatment as a predictor
#    - Compute the pseudo-R2
#    - Compute pseudo std. coefs
# 4. Add the time * treatment interaction
#    - Compute the pseudo-R2
#    - Use the {emmeans}/{marginaleffects} package for simple slope analysis
#      (compute the slope for each group).

# For each model - compare it to the previous model and interpret the results.
# Did the parameter you add improved the model?


