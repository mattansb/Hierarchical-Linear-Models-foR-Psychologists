## Growth models ##

# Load packages:
library(dplyr)
library(ggplot2)
library(lmerTest)
library(parameters)
library(performance)

# (The first part of this tutorial is suppose to be familiar from "Model
# Comparison" tutorial, we will repeat some of it in the notes...)

# The data -------------------------------------------------------------------------

# Example Ch. 6 in Hoffman- same 'Aging&Cognition'  6 occasions data:

# 101 older adults measured on six occasions during a two-week span. 
# The outcome - was a measure of processing speed: the average response
#               time (RT) across trials (in milliseconds).

data <- read.csv("Hoffman_Chap.6-Quadratic growth model.csv")
head(data)

# Our goal will be to model the change in RT over time
# (Note - we will look at 'session' as continuous variable)

## Inspect data 

## Descriptive statistics for RT at each occasion:


data |> 
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


## Two different effects of Time and Spaghetti Plots --------------------------------------------

# The researcher may want to ask herself:

#   1. What is the mean reaction time?
#   2. Do people differ in their RTs?
#   3. Do they get faster across sessions?
#   4. Do they differ in their improvement (or lack of)?
# Which question is answered by which type of effect?


# Two different effects of Time:

# (1) FIXED EFFECTS of time (model for the means)-
#     Is there change in the outcome over time on average? (mean changes across subjects)

# (2) RANDOM EFFECTS of time (model for the variance)- 
#     Does everybody change the same? (individual changes)
#     RANDOM EFFECTS are RANDOM SLOPES!
#     Note- this effect is different from the RANDOM INTERCEPT effect (which is also used
#     in regular repeated measures ANOVA):
#       RANDOM INTERCEPT- individual differences in the mean outcome.
#       RANDOM SLOPE of x (e.g. time)- individual differences in the effect of predictor x 
#                                      (e.g. time) on the outcome.

# COOL VISUALIZATION: http://mfviz.com/hierarchical-models/

## Spaghetti Plots helps to consider the pattern of the data:

## Spaghetti plot: 
ggplot(data, aes(session, rt)) + 
  geom_line(aes(colour = factor(PersonID)), 
            show.legend = FALSE) + 
  stat_smooth(se = FALSE, colour = "black", size = 1) + 
  labs(y = "reaction time")
  
# Seems linear? Maybe not...

# Note- The black line describe the average pattern across time.
#        We didn't forced it to be a linear line as we did the first time we saw
#        this example...
#        [try it with: geom_smooth(method="lm",se=FALSE, colour="black", size=1)]

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
data <- data |> 
  mutate(PersonID = factor(PersonID),
         time = session - 1)










# Building the linear growth model -----------------------------------------------------------------

# We will build the models that were presented in chapter 6.\ we saw in the "Model comparison" tutorial
# (see the png: "Models from Hoffman") 

# Reminder- 4 basic principles in MLM:
# 1. Fixed effects will always be added before their corresponding random effects,
#    given that the fixed effects are what those random effects would be deviating from.
# 2. Random effects will be tested (when possible) even if the corresponding fixed
#    effect was not significant. 
# 3. Fixed effects and random effects will be added in sequential models, rather than
#    adding both at the same time. 
# 4. Variances of random effects (and their covariances with random effects already
#    in the model) will be added one at a time so that the unique contribution of each
#    can be evaluated with a -2LL test\AIC\BIC.

# The level-1 model predicts outcome variation within persons (WP) over time-> measurement level
# The level-2 model predicts variation between persons (BP)-> id's level


## Empty means, random intercept model (empty WP model) -------------------------------------------------------

# FIXED EFFECTS: intercept
# RANDOM EFFECTS: intercept

# level-1 equation: OUTCOMEti = b0i + eti 
#                   [math score for person i in time t= mean intercept for person i +
#                                                       error for person i in time t]
# level-2 equation: b0i = gamma00 + U0i
#                   [mean intercept for person i= fixed (general) intercept for sample +
#                                                 the individual difference of person i's intersept from this grand mean]
# Composite: OUTCOMEti = gamma00 + U0i + eti
#            [math score for person i in time t= sample's mean (fixed intercept)+ 
#                                                the individual difference in person i intercept +
#                                                error for person i in time t]

M1 <- lmer(rt ~ 1 + (1 | PersonID), 
           data = data)
model_parameters(M1, ci_method = "S")

icc(M1)
# 81.7% of the variance in the RT outcome is BP variance in mean RT.


## fixed time and random intercept linear model -------------------------------------


# FIXED EFFECTS: intercept, time
# RANDOM EFFECTS: intercept

## The model's equations:

# level-1 equation: OUTCOMEti = b0i + b1i(TIMEti) + eti 
#                   [math score for person i in time t= mean intercept for person i +
#                                                       time effect for for person i * the t time point +
#                                                       error for person i in time t]
# level-2 equations: b0i = gamma00 + U0i <- same as before
#                    b1i = gamma10 
#                    [time effect for for person i = the generall time effect in the sample] 

# Composite: OUTCOMEti = gamma00 + gamma10(TIMEti) + U0i + eti
#            [math score for person i in time t= sample's grand mean (fixed intercept)+ 
#                                                general time effect * the t time point +
#                                                the individual difference in person i mean +
#                                                error for person i in time t]

# Note- it will be more accurate to write TIMEti as the t time point *for person i*.
#       But, time points are the same for all i's. so, I kept it simple and wrote "the t time point"...
  
M2a <- lmer(rt ~ time + (1 | PersonID), 
            data = data)
model_parameters(M2a, ci_method = "S")

# mean trajectory for RT that starts at fixed intercept of gamma00 = 1,899.6 
# at session 1 and decreases linearly over time by fixed linear slope of 
# gamma10 = -51.6 per session.

# Time is sig. and all indices also suggest it improves fit:
anova(M2a, M1)

## random linear time model --------------------------------------------------------------

# FIXED EFFECTS: intercept, time
# RANDOM EFFECTS: intercept, time

## The model's equations:

# level-1 equation:  OUTCOMEti = b0i + b1i(TIMEti) + eti <- same as before
# level-2 equations: b0i = gamma00 + U0i <- same as before
#                    b1i = gamma10 + U1i
#                    [time effect for for person i = the generall time effect in the sample
#                                                    + the individual effect of time for person i] 

# Composite: OUTCOMEti = gamma00 + (gamma10 + U1i)(TIMEti) + U0i + eti
#            [math score for person i in time t= sample's mean (fixed intercept)+ 
#                                                (time effect based on the general+ individual effects) * the t time point +
#                                                the individual difference in person i mean +
#                                                error for person i in time t]


# We can model the cov between random effects (as in the book):
M2b <- lmer(rt ~ time + (time | PersonID),
            data = data)
model_parameters(M2b, ci_method = "S")


# To test the significance of the addition of the random linear time slope 
# variance (and its covariance with the random intercept), we can do a deviance 
# difference test against the fixed linear time:
anova(M2b, M2a)




# Quadratic models (that's new!)+ convergence problems -------------------------------------------------------

# + A unique feature of polynomial models is that they are hierarchical
#   such that the quadratic effect (time^2) is higher-order than the linear effect 
#   (time), and the intercept is the lowest-order term in the model.
#     -> we will build our models accordingly.

# Polynomial models are common approach for modeling nonlinear change because of
# their relatively low data requirements, ease of estimation, and wide-spread availability.
# However, because they have limited theoretical utility, it is important to be aware of 
# alternative models whose parameters might be more meaningful and therefore useful in 
# describing change over time (for now, beyond our scope. see chapter 6).

# First option within the polynomial family-
# Quadratic modeling: a single bend trajectories in which the rate of change appears to
# change only once.

## fixed quadratic, random linear time model -----------------------------------------------

# FIXED EFFECTS: intercept, time, time^2
# RANDOM EFFECTS: intercept, time

M3a <- lmer(rt ~ poly(time, 2, raw = TRUE) + (time | PersonID),
            data = data)
# # This syntax will give us the same:
# M3a <- lmer(rt ~ time + I(time ^ 2) + (time | PersonID), data = data)
model_parameters(M3a, ci_method = "S")
# raw = TRUE is important - if set to FALSE (default) is centers the predictor
# and changes the interpretation of the coefs (but not the model fit).


# The fixed quadratic time slope Gamma20 is significant + 
# this model presents better fit:
anova(M3a, M2b)
  
# FEW NOTES:

# The quadratic effect of time is often labeled as the rate of acceleration or deceleration.
# However, its' estimated coefficient is actually only half of the rate.->
# Acceleration\ deceleration- how the fixed linear effect of time changes per unit time, 
# which is given by twice the fixed quadratic effect of time (2*13.866=27.732).

# Although the quadratic predictor TIME^2, is nonlinear transformation of time,
# it's still multiplied by a single fixed effect (slope) coefficient, like in any linear model:
# Predicted rt = intercept + b1*time + b2*time^2
# E.G. RT(t=3)= 1945.85 + (-120.9)*3 + 13.866*9 = 1707.944



#	After including a fixed quadratic effect of time, the fixed linear effect of time is now also
# conditional, such that it becomes the instantaneous linear rate of change specifically at time 0. 
  # Thus, at session 1 (time 0), RT decreases by gamma10 = -120.9 per session. 
  # The positive fixed quadratic time slope creates a decelerating negative trajectory, 
  # such that the negative linear rate of change will become less negative per session by 27.8.
  
# So - the decline in RT slows down across sessions on average,
#      but do people differ in their rates of deceleration?...
#      A random quadratic effect of time is a person-specific 
#      deviation from the fixed quadratic effect!
  
  
## random quadratic time model ------------------------------------------------------------
  
# FIXED EFFECTS: intercept, time, time^2
# RANDOM EFFECTS: intercept, time, time^2

M3b <- lmer(rt ~ poly(time, 2, raw = TRUE) +
              (poly(time, 2, raw = TRUE) | PersonID),
            data = data)
model_parameters(M3b, ci_method = "S")
VarCorr(M3b)
anova(M3b, M3a)

## Models 2b & 3b results are the same as in the book - go over this tutorial
## with the book.

# Model M3b seems better than M3a ****BUT**** we got a warning:
# Warning message:
# In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
# Model failed to converge with max|grad| = 0.0447192 (tol = 0.002, component 1)

# In this case of problems in convergence of the model, we want to be very careful.
# It might be that a model with this warning (and even reasonable) will provide better fit.
# But over-fitting is a big no-no!

# ----> that means we used too many parameters\ overfitted the data.
#       Actually, we jumped from 7 parameters in M3a to 10 parameters in M3b-  
#       We have added the (1) quadratic slope variance
#                         (2) quadratic slope cov with the linear slope
#                         (2) quadratic slope cov with the intercept

# I.E., this was not a truly sequential modelling.
#       we may inspect our random effects:
VarCorr(M3b) # it seems that quadratic slope cov with the linear slope are highly correlated!


# Lets' try modeling without covariance with quadratic time effect: 
M3bNew <- lmer(rt ~ poly(time, 2, raw = TRUE) +
                 (time | PersonID) + (0 + I(time ^ 2) | PersonID),
               data = data)
# Note the singularity warning. We will discuss this later in class.

# inspecting the random effects:
VarCorr(M3bNew)
# the quadratic slope variance is ~0! It seems there aren't any differences in
# the quadratic slope.
# We will talk about this in class.







# Simple intercepts and slopes ------------------------------------------------

# To see the PREDICTED intercepts and PREDICTED slopes
# (not the one's observed in the data)

library(emmeans)
library(ggeffects)

# Simple intercepts in each time point (with CI)
emmeans(M3b, ~ time,
        at = list(time = 0:5), # means at each time
        infer = TRUE) # get t-vals and CI

ggemmeans(M3b, c("time [all]")) |> plot()

# simple slopes (instantaneous linear rate of change) at each time point
emtrends(M3b, ~ time, var = 'time', max.degree = 1, 
         at = list(time = 0:5), # slopes at each time!
         infer = TRUE) # get t-vals and confint

  
# The linear rate of change is significantly negative through session 4, but by
# session 5 it is nonsignificantly negative (and is nonsignificantly positive at
# session 6). Thus, the improvement predicted by the quadratic model appears to
# ?shut off? after session 4.

