# All tutorial materials were originally developed by Yael Bar-Shachar (no relation).

                         ### (1) Intro to HLM ###

# For data preparation:
library(tidyverse)

# For running MLMs:
library(lmerTest) # Uses {lme4}
# Additional packages:
# - {nlme}: more flexible R matrix; limited to simple random structures
# - {glmmTMB}: more flexible; can be less stable
# - {brms}: MOST flexible; Bayesian

# For simple slope analysis:
library(emmeans)

# For getting ICC and R2:
library(performance)
# For tidy parameter tables with CIs
library(parameters)
library(merDeriv)


# Example 1: two-occasion data (Chp. 3- Hoffman) ==============================

# getting the DATA:
dataset <- read.csv("Example_1.csv")

head(dataset)
glimpse(dataset)

# preparing the DATA for MLM analysis:
# Converting data from wide to long\stacked format
dataset.long <- dataset |>
  pivot_longer(cols = c(T1, T2), 
               names_to = "Time", 
               values_to = "outcome") |>
  mutate(
    PersonID = factor(PersonID),
    Time = ifelse(Time == "T1", 0, 1),
    group = factor(group, levels = c(1, 2), labels = c("G1", "G2"))
  )
# Note, we centered time to the first assessment! -> "T1" is 0.
# We will talk more about centering in the future... For now, just think of the 
# making the predictors' 0 point *meaningful*. 
# We also converted group to a factor.
# (We could have also just converted these into factors)

head(dataset.long)


## Spaghetti plots -------------------------------------------------
# First, lets' just see the patterns in the data:

# Spaghetti plots- change in outcome by time:
p_spaghetti <- ggplot(dataset.long, aes(x = Time, y = outcome, color = factor(PersonID)))+
  # add a line for each ID but don't show use a legend
  geom_line(show.legend = FALSE) + 
  # add the pattern of the mean slope (linear pattern)
  stat_smooth(method = "lm", se = FALSE, 
              color = "black", linewidth = 1) + 
  scale_x_continuous(breaks = c(0, 1)) + 
  labs(x = "Time", y = "outcome") # use axis labels...
p_spaghetti

# split to groups:
p_spaghetti + 
  facet_wrap(~ group)


## Empty Model ---------------------------------------------------------------
# AKA intercept-only models

## Empty BP Model:
#-> simply using lm()
Model.empty.lm <- lm(outcome ~ 1,       # 1 is for including the intercept
                     data = dataset.long)
summary(Model.empty.lm)

# Tidy parameters:
model_parameters(Model.empty.lm)

# and yes, this intercept is just the outcome mean ...
mean(dataset.long$outcome)

## Empty  WP Model(=random intercept model):
#-> now we need lmer() for MLM (i.e. adding RANDOM effects, here- random intercept)
Model.empty.mlm <- lmer(outcome ~ 1 +
                          # the syntax start the same for FIXED effects
                          (1 | PersonID),
                        # BUT, we will add the RANDOM effects with Wilkinson's
                        # notation:
                        # (varying effect\s | random grouping variable)
                        REML = TRUE, 
                        # This is default (we can omit this argument), set to
                        # FALSE for ML fitting
                        data = dataset.long) 

summary(Model.empty.mlm) # Satterthwaite's method is the default
summary(Model.empty.mlm, ddf="Kenward-Roger") # we can change it to "KR"

# see tidy parameters:
model_parameters(Model.empty.mlm, ci_method = "Satterthwaite") # Or ci_method = "S"
model_parameters(Model.empty.mlm, ci_method = "KR")

# Check out that the output refers to:
# 1. Fixed effects (model of the means)
# 2. Random effects (model of the variances\SDs)


### Computing ICC ------------------------------

# We use the empty model for producing the intra-class correlation (ICC), which
# is actually:
#   Sigma{a} square / Total variance (Sigma{a} square + Sigma{e} square)

(3.50)^2 / ((3.50)^2 + (5.31)^2)

# OR...
# just use the icc() function from "performance" library.
icc(Model.empty.mlm)

# (For empty models, "Adjusted" and "Conditional" ICC are actually unconditional
# ICCs, For now- ignore Adjusted/ Conditional)



## Conditional model (=none empty) ---------------------------------------------------------

# Effects are conditioned on some predictor(s)

# We will add the group*time FIXED effects.
# That is, time effect, group effect, and their interaction. 

# Ignoring the idea that we measured each person twice - a BP model:
BPModel.con <- lm(outcome ~ Time * group ,
                  data = dataset.long)
model_parameters(BPModel.con)

# Interaction is not significant - there is much uncertainty about the estimate.

# The same model- accounting for the variance in the intercept (a WP model)
WPModel.con <- lmer(outcome ~ Time * group + (1 | PersonID),
                    data = dataset.long)
model_parameters(WPModel.con, ci_method = "S")
# The interaction's SE, CIs and p have changed - why?


# Pretty tables:
sjPlot::tab_model(BPModel.con, WPModel.con, 
                  show.se= TRUE,
                  df.method= "satterthwaite")

# NOTE: The intercept is the only random effect in the model. That and the
# nature of the design, means that this is not different from repeated measures
# ANOVA (besides of using REML and not OLS).

afex::aov_ez("PersonID", "outcome", dataset.long,
             between = "group", within = "Time")
# Notice that the df and p value for the interaction are identical!
# (They are NOT the same for Time and group - in the ANOVA they are main effect,
# in the LMM they are simple effects.)


### Simple slopes analysis --------------------------

# For significant interaction we would like to test simple slopes in our case we
# can see the slopes for Time=0 or group=0, but what about Time=1 or group=1?

emm_options(lmer.df = "satterthwaite") # setting the emm options to use 
                                       # satterthwaite df

# use emmeans's emtrends() function:
TrendsByGroup <- emtrends(WPModel.con, # the model
                          var = "Time", # the IV
                          ~ group , # the moderator
                          infer = TRUE)
TrendsByGroup

# see how Time effect for group 0 is equal to the effect of time
# in the main model:
model_parameters(WPModel.con, ci_method = "S")



### Random effects ----------------------------------

# lme4 (and lmerTest) have several new functions that allow us to extract the
# predicted group-wise estimated parameters = the random effects.
#
# ranef() give the deviation of each random parameter from the fixed parameter:
ranef(WPModel.con) # If we had random slopes, they would appear as additional columns

# coef() gived the actual group-wise effects:
coef(WPModel.con)
# Note that the effects for Time, group, and Time:group do NOT differ between
# subjects - these either were not modeled as random effects (e.g. Time) or are
# level 2 effects (so they cannot differ between subjects!)



