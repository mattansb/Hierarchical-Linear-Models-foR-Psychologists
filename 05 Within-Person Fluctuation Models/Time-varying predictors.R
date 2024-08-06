## Tutorial - Time-varing\longitudinal\dynamic\measured-level-1 predictors ##


## Load relevant packages:
library(tidyverse)
library(datawizard)


library(lmerTest)
library(performance)
library(parameters)

# The first example will serve to illustrate the role of time-varing predictors
# (e.g., mood) in longitudinal models for outcomes that show within-person
# FLUCTUATION.

# The second example will serve to illustrate the role of time-varing predictors
# (e.g., parents monitoring behaviors) in longitudinal GROWTH models (change
# over time), as in the last tutorial.



# Time-Variant Predictors in Models for Within-Person Fluctuation: ------------

data <- read.csv("Example_DailyMoodStressSymptoms_Hoffman_Chap.78.csv") 

# Data comes from the Cognition, Health, and Aging Project, in which 6
# assessments were collected over two weeks. Besides RT of a cognitive task, the
# study also collected a variety of other measures (of physical, cognitive, and
# emotional well-being). Because of concerns about initial reactivity to the
# assessments, only sessions 2 to 6 (i.e. 5 obs.) will be used.
# The outcome 'symptoms': the number of physical symptoms participants reported
# experiencing in the past 24 hours.

# The sample includes 525 lines of observations from 105 older adults:
head(data, n = 20)
# Can you tell which predictors are time-invariant?
# Can you tell which predictors are time-varying?



# Specifically we have in this data:
# 2 invariant level-2 variables: baseage, women
# 7 WP level-1 variables:
#   4 of them are some indexes of time (session , studyday dayofweek , weekend)
#   3 MEASURED time variant predictors (symptoms, mood , stressor)

# we will start our example for predicting daily physical symptoms by first
# examining a time-varying predictor of daily negative mood as computed as the
# mean of five items: sad, annoyed, worried, irritated, and depressed, each
# rated on a 5-point scale

## Data Prep ------------------------------------------------------

# Let's see how to properly center each type of variables.

# We will center our variables according to their level. 

# For example, centering \ coding of Time-Invariant Predictors  will be the same
# as we do in GLM: i.e, treating factors correctly (e.g. dummy coded) and
# centering continuous variables around the sample's mean.

# women (gender) is a binary variable:
# 0- man; 1- women
# (can be left as is, or centered such that 0 will be genders' mean).
data$women


data <- data |> 
  mutate(
    gender = factor(women, labels = c("man", "woman")),
    # age can be centered to the sample's mean (=grand mean)
    baseage_gmc = center(baseage)
  ) |> 
  drop_na(mood, symptoms)

data |> 
  select(baseage, baseage_gmc, gender, women) |> 
  head(n = 10)



### Centering MEASURED time-varying Predictors -----------------
# (that's new!) 

# symptoms, mood, and stressor, however, should be treated differently!

# We will use symptoms (= daily physical symptoms) as our outcome, and we won't
# center the outcome. But, if in another model, we would want to use symptoms as
# a predictor we might have to center it.


## Centering mood (our time varying predictor):
# Although time-varying predictors are measured at the time level (i.e., at each
# occasion), they usually contain systematic variation at higher levels of
# sampling as well (i.e., time-invariant variation).
# -> they contain both BP and WP information.
#    Both sources of variation must be represented explicitly (and centered
#    appropriately)!

## Two variables (instead of one) for each variation:
# 1- BP effect at level 2 - a trait \ chronic effect:
#    In the example of stress and negative mood, people with higher stress on
#    average (than other people) may report greater negative mood on average. 
# 2- WP effect at level 1 - a state \ acute effect:
#    At times when stress is higher than usual, negative mood may be greater
#    than usual as well.


#### Check heterogeneity in predictor -----
# [Not part of centering but part of the process] 

# How much of a time-varying predictor's variation is due to each source?  
# -> treat the time-varying predictor as an outcome and use an empty random
# model to quantify ICC:

lmer(mood ~ 1 + (1 | PersonID), data = data) |> 
  icc()
# Here, 35.5% of mood's variance was due to BP mean differences
# ICC size dictates which effects the time-varying predictor potentially show. 
# -> if there is significant BP variance then that BP variation can potentially
#    show a corresponding BP effect.
# -> Alternatively, this means it has only WP variation, and thus the 
#    time-varying predictor could only show a WP effect.

# We can also:
check_heterogeneity_bias(data, 
                         select = c("mood", "baseage"), 
                         by = "PersonID")
# Indeed, our time varying predictor has Possible heterogeneity bias 

# Visually:
data |> 
  mutate(
    PersonID = factor(PersonID) |> fct_reorder(mood, .fun = mean)
  ) |> 
  ggplot(aes(PersonID, mood)) +
  geom_point() +
  stat_summary(geom = "point", fill = "red", size = 2, shape = 21)


#### Computing the two new mood variables ---------------------

# mood_WP - the time-varying mood value at time t for person i.
# mood_PM  - the person mean of time-varying negative mood across days.
data |> 
  group_by(PersonID) |>
  mutate(
    mood_PM = mean(mood, na.rm = TRUE), # person mean
    mood_WP = mood - mood_PM # person mean centering!
  ) |> 
  select(PersonID, mood, mood_PM, mood_WP) |> 
  head(n = 15)


# We can also do this with the demean() function:
?demean 

data <- data |> 
  demean(select = "mood", 
         by = "PersonID", 
         
         suffix_groupmean = "_PM",
         suffix_demean = "_WP") |> 
  # add to the rest of the data
  bind_cols(data)

data |> 
  select(PersonID, mood, mood_PM, mood_WP) |> 
  head(data, n = 15)
# mood_WP is centered at a variable (not a constant): person's usual level of
# daily negative mood, as represented by each person's mean across occasions.


# We can also further grand-center the level 2 predictor if we wanted to:
data <- data |>
  mutate(mood_PM_c = center(mood_PM)) 



# All in all, these are our "mood variables"
data |> 
  select(PersonID, mood, mood_PM, mood_WP, mood_PM_c) |> 
  head(n = 15)
# Due to the variables centering - mood_WP will be uncorrelated with the mood_PM_c
# ->  because they operate at separate levels of the model, the effects of mood_WP
#     and mood_PM_c can be evaluated simultaneously. 


# Modeling - WP fluctuation effects - 2 main effects for mood-------------------

# we now continue the example by examining the effect of mood time-varying
# predictor. Note that time is not part of the model (not a growth model):
# Here, what we need to know is what the mood value was at each occasion, and
# not necessarily when that value was reported.

M0 <- lmer(symptoms ~ 1 + (1 | PersonID),
           data = data)
icc(M0)


## model 1a- adding the BP and WP effects of mood 

# FIXED: intercept, WPmood, BPmood
# RANDOM: intercept
  
# Level 1: symptomsti = b0i +b1i(mood_WPi)+eti
#                   the WP variable is in level 1!

# Level 2: 
# intercept: b0i = gamma00 + gamma01(mood_PM_ci)+ U0i
#                   the BP variable is in level 2! (Modeling the intercept)
# WPmood:    b1i = gamma10  -> For now- WPmood is not random

# Composite: symptomsti = gamma00 + U0i + gamma01(mood_PM_ci) + gamma10(mood_WPti)  + eti


M1a <- lmer(symptoms ~  mood_PM_c + mood_WP + # mood new vars as fixed effects
         (1 | PersonID),
       data = data)
model_parameters(M1a, ci_method = "S")

# The fixed intercept gamma00 = 1.30  is now the expected number of physical
# symptoms when all predictors have a value of 0: for a person with average mood
# equal to the average mood in the sample, and on  on a day when he is at his
# average negative mood.

## Negative mood effects:

# The sig. BP main effect of mood (gamma01 = 2.10 ) indicates that for every
# one-unit higher person ~mean mood, the symptoms reported across days is
# expected to be higher by 2.10 The n.s. WP main effect of mood (gamma10 =0.13 )
# indicates that for every one-unit more negative mood than usual (i.e.,relative
# to the person's mean), that specific day's symptoms are expected to be non
# significantly higher by 0.13

# To summarize, grumpier people report more symptoms across days, but being in a
# worse mood than usual doesn't predict reporting more physical symptoms that
# day.
# -> the BP and WP effects of the same variable appear to differ. 





## Effect sizes ----------------------------------------

### pseudo-beta ----------------------
standardize_parameters(M1a, method = "pseudo")



### pseudo-R2 ----------------------
# proportion reduction in each variance component using :
# (Variancefewer - Variancemore) / Variancefewer. 

VarCorr(M0)
# Baseline model: Residual sd = 0.78665  
#                 Intercept sd: 1.10872  
VarCorr(M1a)
# New model: Residual sd  = 0.78671      
#            Intercept sd = 0.95594     


# How much variance between people in symptoms is explained by mean mood?
1 - (0.95594 / 1.10872)^2
# 25%!


# How much variance within people in symptoms is explained by wp mood?
1 - (0.78671 / 0.78665)^2
# ~0%


### R2 nakagawa -----------------
r2_nakagawa(M1a)
#   Now 17% of the total outcome variance is now explained by fixed effects.
#   But still- 66% of total variance explained-
    # ->  Since we saw there was no additional explained variance by WP main effect
    #     17% of the variance is probably explained by BP effect, which explained some of the 
    #     already explained variance by the random intercept. 

### ICC -------------------

icc(M1a)
# Random intercepts explain 49% of the total variance
# or 59% of the variance not explained by the fixed effects.



    
    
## Adding the mood_WP var RANDOM EFFECT ----------------------

# FIXED: intercept, WPmood, BPmood
# RANDOM: intercept, WPmood

# That is, in level 2: b1i = gamma10 + U1i
# such that:
# Composite: symptomsti = gamma00 + gamma10(mood_PM_ci) + (gamma10 + U1i)(mood_WPti) + U0i + eti
    
# now every ID will have its own slope of WP changes in mood effect on symptoms.

M1b <- lmer(symptoms ~  mood_PM_c + mood_WP +
              (mood_WP | PersonID), # setting WP mood changes as random + COV with the intercept
            data = data)
model_parameters(M1b, ci_method = "S")
anova(M1a, M1b)
# Including the random effect didn't improved the model - it made the fit much
# worse.
# However, if we are interested in the generalization of the WP effect of mood -
# WE WILL INCLUDE IT!







# Exercise --------------------------------------------------------------------

data2 <- read.csv("ChangeInRiskyBehaviorAdolescence_Hoffman_Chap.7.csv") 

head(data2)
# Seven (approximately annual) assessments were collected from 200 adolescent girls 
# age 12 to 18 (1,400 total observations).

# The variables:
# The outcome variable: Risky (12-18)- a sum of 10 items indexing frequency of risky behavior 
#                       (e.g., smoking, drinking, shoplifting, skipping school).
# Time-invariant predictor: Attitude12- an index of their mothers' attitudes about
#                           smoking and drinking (measured only in the first time point).
# Time-variant predictors: Age (12-18)- the exact age in each time point.
#                          Monitor (12-18)- Now we will use it! Monitoring was measured as
#                          the mean across 10 items on a 1 to 5 scale.


## Data Prep (explained in the first tutorial):

# Creating a to long (stacked) format of the data:
data.long <- data2 |>
  pivot_longer(
    cols = starts_with(c("Age", "Risky", "Monitor")),
    names_pattern = "(.*)([0-9][0-9])",
    names_to = c(".value", "Age_rounded")
  ) |> 
  mutate(
    # we centered age to 18:
    YearsPre18 = Age - 18
  )
head(data.long)


## Your exercise:
# We are interested in the effect of age and monitoring on on Risky behavior.
# 1. For each predictor, examine if it should be treated as a time varrying
#   predictor.
#   - ICC / check of heterogeneity
#   - If you decided it is, split it into the two components.
# 2. Build your models step by step until you reach the maximal model.
#   - In each step, interpret your coefficients (fixed and random)
#   - Compute relevant pseudo R2 effect sizes.
#   - Compare models where you see fit.




# FOR MORE OF TIME VARYING PREDICTORS IN GROWTH MODELS - READ CHAPTER 9.
