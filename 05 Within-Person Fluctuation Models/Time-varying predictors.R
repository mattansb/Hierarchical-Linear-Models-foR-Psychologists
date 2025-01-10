 ## Tutorial - Time-varing\longitudinal\dynamic\measured-level-1 predictors ##


library(tidyverse)
library(datawizard)


library(lmerTest)

library(performance)
library(parameters)


# The data -------------------------------------------------------------------

temp <- tempfile()
download.file("https://www.pilesofvariance.com/Chapter8/SPSS/SPSS_Chapter8.zip", temp)
dataset <- haven::read_sav(unz(temp, "SPSS_Chapter8/SPSS_Chapter8.sav"))
unlink(temp)


# Data comes from the Cognition, Health, and Aging Project, in which 6
# assessments were collected over two weeks. Besides RT of a cognitive task, the
# study also collected a variety of other measures (of physical, cognitive, and
# emotional well-being). Because of concerns about initial reactivity to the
# assessments, only sessions 2 to 6 (i.e. 5 obs.) will be used.
# The outcome 'symptoms': the number of physical symptoms participants reported
# experiencing in the past 24 hours.

# The sample includes 525 lines of observations from 105 older adults:
head(dataset, n = 20)



## Understanding the data --------------------

# The outcome is "symptoms"
# What is the random grouping variable? Why?
# What level are the other variables on? Person (2)? Measurement (1)?
# - Can you tell which predictors are time-invariant?
# - Can you tell which predictors are time-varying?
#   How can you tell?

dataset |> 
  filter(PersonID == "105")


# We will start our example for predicting daily physical symptoms by first
# examining a time-varying predictor of daily negative mood as rated on a
# 5-point scale


## Data Prep ------------------------------------------------------

# Let's see how to properly center each type of variables.

# We will center our variables according to their level. 

# For example, centering \ coding of Time-Invariant Predictors  will be the same
# as we do in GLM: i.e, treating factors correctly (e.g. dummy coded) and
# centering continuous variables around the sample's mean.

# women (gender) is a binary variable:
# 0- man; 1- women
# (can be left as is, or centered such that 0 will be genders' mean).
dataset$women


dataset <- dataset |> 
  mutate(
    gender = factor(women, labels = c("man", "woman")),
    # age can be centered to the sample's mean (=grand mean)
    baseage_gmc = center(baseage)
  ) |> 
  drop_na(mood, symptoms)



## Dealing with MEASURED time-varying predictors --------------------------
# (that's new!) 

# Although time-varying predictors are measured at the time level (i.e., at each
# occasion), they usually contain systematic variation at higher levels of
# sampling as well (i.e., time-invariant variation).
# -> They contain both between- and within-person information.
#    Both sources of variation must be represented explicitly to avoid effect
#    "smushing"!
#
# Two variables (instead of one) for each variation:
# 1) Level 2 - a trait \ chronic effect:
#    In the example of stress and negative mood, people with higher stress on
#    average (than other people) may report greater negative mood on average. 
# 2) Level 1 - a state \ acute effect:
#    At times when stress is higher than usual, negative mood may be greater
#    than usual as well.


### Check heterogeneity in predictor -----

# How much of a time-varying predictor's variation is due to each source?  
# -> treat the time-varying predictor as an outcome and use an empty random
#    model to quantify ICC:
lmer(mood ~ 1 + (1 | PersonID), data = dataset) |> 
  icc()
# Here, 35.5% of mood's variance was due to between-person (mean) differences.
# ICC size dictates which effects the time-varying predictor potentially show. 
# -> If there is substantial between-person variance then that between-person 
#    variation can potentially show a corresponding between-person effect.
# -> Alternatively, this means it has only within-person variation, and thus the 
#    time-varying predictor could only show a within-person effect.


# Visually:
dataset |> 
  mutate(
    PersonID = factor(PersonID) |> fct_reorder(mood, .fun = mean)
  ) |> 
  ggplot(aes(PersonID, mood)) +
  geom_point() +
  stat_summary(geom = "point", color = "red", size = 3) + 
  scale_x_discrete("Person", labels = NULL) + 
  scale_y_continuous("Negative Mood", 
                     breaks = 1:5, minor_breaks = NULL,
                     limits = c(1, 5)) + 
  theme_bw()


### Splitting the variable ------------------------------------

# mood_WP - the time-varying mood value at time t for person i.
# mood_PM  - the person mean of time-varying negative mood across days.
dataset |> 
  group_by(PersonID) |>
  mutate(
    mood_PM = mean(mood, na.rm = TRUE), # person mean
    mood_WP = mood - mood_PM # person mean centering!
  ) |> 
  select(PersonID, mood, mood_PM, mood_WP) |> 
  head(n = 15)


# We can also do this with the demean() function:
?demean 

dataset <- dataset |> 
  demean(select = "mood", 
         by = "PersonID", 
         
         suffix_groupmean = "_PM",
         suffix_demean = "_WP") |> 
  # add to the rest of the data
  bind_cols(dataset)

dataset |> 
  select(PersonID, contains("mood")) |> 
  head(data, n = 15)
# mood_WP is centered at a variable (not a constant): person's usual level of
# daily negative mood, as represented by each person's mean across occasions.


# We can also further grand-center the level 2 predictor if we wanted to:
dataset <- dataset |>
  mutate(mood_PM_c = center(mood_PM)) 



# All in all, these are our "mood variables"
dataset |> 
  select(PersonID, contains("mood")) |> 
  head(n = 15)
# Due to the centering - mood_WP will be uncorrelated with the mood_PM_c
# ->  because they operate at separate levels of the model, the effects of 
#     mood_WP and mood_PM_c can be evaluated simultaneously.



# And that's it - we can now "forget" that those two variables were once one,
# and continute as usual!




# Modeling - WP fluctuation effects - 2 types of effect for mood--------------

# We now continue the example by examining the effect of mood time-varying
# predictor. Note that time is not part of the model (not a growth model): Here,
# what we need to know is what the mood value was at each occasion, and not
# necessarily when that value was reported.



## Random intercepts model --------------------------------

mod_rndm.intr <- lmer(symptoms ~ 1 + (1 | PersonID),
                      data = dataset)
icc(mod_rndm.intr)


## Mood effect(s) -----------------------------------------

# FIXED: intercept, WPmood, BPmood
# RANDOM: intercept
  
# Level 1: 
#     symptoms_ji = b_0j + b_1j * mood_WP_i + e_ij
#
# Level 2: 
#     b_0j = gamma_00 + gamma_01 * mood_PM_c_j + U_0j
#     b_1j = gamma_10
#
# Composite:
#     symptoms_ji = (gamma_00 + gamma_01 * mood_PM_c_j + U_0j) + 
#                   gamma_10 * mood_WP_i + e_ij


mod_mood <- lmer(symptoms ~  mood_PM_c + mood_WP + # mood new vars as fixed effects
                   (1 | PersonID),
                 data = dataset)

model_parameters(mod_mood, ci_method = "S")
# The fixed intercept (gamma_00 = 1.30) is now the expected number of physical
# symptoms when all predictors have a value of 0: for a person with average mood
# equal to the average mood in the sample, and on on a day when he is at his
# average negative mood.
#
# The between-person effect of mood (gamma_01 = 2.12) indicates that for every
# one-unit higher person ~mean mood, the symptoms reported across days is
# expected to be higher by 2.12.
#
# The within-person effect of mood (gamma_10 = 0.12) is not significant, and the
# effect might be anywhere between [-0.13, 0.37] - less than a third of a
# symptom in magnitude.
#
#
# To summarize, grumpier people report more symptoms across days, but being in a
# worse mood than usual doesn't predict reporting more physical symptoms that
# day.
# -> the BP and WP effects of the same variable appear to differ. 





### Effect sizes ----------------------------------------

#### pseudo-beta ----------------------
standardize_parameters(mod_mood, method = "pseudo")



#### pseudo-R2 ----------------------
# proportion reduction in each variance component using :
# (Variancefewer - Variancemore) / Variancefewer. 

VarCorr(mod_rndm.intr)
# Baseline model: Residual sd = 0.78665
#                Intercept sd = 1.10872
VarCorr(mod_mood)
# New model: Residual sd = 0.78671      
#           Intercept sd = 0.95594     


# How much variance between people in symptoms is explained by mean mood?
1 - (0.95594 / 1.10872)^2
# 25%!


# How much variance within people in symptoms is explained by wp mood?
1 - (0.78671 / 0.78665)^2
# ~0%



#### R2 nakagawa -----------------
r2_nakagawa(mod_mood)
# Now 17% of the total outcome variance is now explained by fixed effects.
# But still- 66% of total variance explained -
# ->  Since we saw there was no additional explained variance by WP main effect
#     17% of the variance is probably explained by BP effect, which explained
#     some of the already explained variance by the random intercept.


#### ICC -------------------

icc(mod_mood)
# Random intercepts explain 49% of the total variance
# or 59% of the variance not explained by the fixed effects.



    
## Mood effect random slopes -----------------------------------------

# FIXED: intercept, WPmood, BPmood
# RANDOM: intercept, WPmood

# That is, in level 2: 
#   b_1j = gamma_10 + U_1j
#
# Composite:
#     symptoms_ji = (gamma_00 + gamma_01 * mood_PM_c_j + U_0j) + 
#                   (gamma_10 + U_1j) * mood_WP_i + e_ij
    
# now every ID will have its own slope of WP changes in mood effect on symptoms.

mod_rndm.mood <- lmer(symptoms ~  mood_PM_c + mood_WP +
                        # setting WP mood changes as random + COV with the intercept
                        (mood_WP | PersonID), 
                      data = dataset)

model_parameters(mod_rndm.mood, ci_method = "S", ci_random = TRUE)
anova(mod_rndm.mood, mod_mood, refit = FALSE)
# Including the random effect didn't improved the model - it made the fit
# somewhat worse.
# However!!!!! If we are interested in the generalization of the WP effect of
# mood - WE MUST INCLUDE IT!











# Exercise --------------------------------------------------------------------


temp <- tempfile()
download.file("https://www.pilesofvariance.com/Chapter9/SPSS/SPSS_Chapter9.zip", temp)
dataset2 <- haven::read_sav(unz(temp, "SPSS_Chapter9/SPSS_Chapter9.sav"))
unlink(temp)

head(dataset2)
# Seven (approximately annual) assessments were collected from 200 adolescent
# girls age 12 to 18 (1,400 total observations).

# The variables:
# The outcome variable: 
#     Risky (12-18) - a sum of 10 items indexing frequency of risky behavior
#     (e.g., smoking, drinking, shoplifting, skipping school).
# Predictors:
#     - Attitude12 - an index of their mothers' attitudes about smoking and
#       drinking (measured only in the first time point).
#     - Age (12-18) - the exact age at each time point. 
#     - Monitor (12-18)- Monitoring was measured as the mean across 10 items on
#       a 1 to 5 scale.


# Creating a to long (stacked) format of the data:
dataset2.long <- dataset2 |>
  pivot_longer(
    cols = starts_with(c("Age", "Risky", "Monitor")),
    names_pattern = "(.*)([0-9][0-9])",
    names_to = c(".value", "Age_rounded"), 
    names_transform = list(Age_rounded = as.numeric)
  ) |> 
  mutate(
    PersonID = factor(PersonID),
    YearsPre18 = Age - 18 # we centered age to 18
  )
head(dataset2.long)


# We are interested in the effect of age and monitoring on on Risky behavior.
# 1. For each predictor, think if it should be treated as a time varying
#    predictor.
#   - ICC / check of heterogeneity
#   - If you decided it is, split it into the two components.
# 2. Build your models step by step until you reach the maximal model.
#   - In each step, interpret your coefficients (fixed and random)
#   - Compute relevant pseudo R2 effect sizes.
#   - Compare models where you see fit.


# FOR MORE OF TIME VARYING PREDICTORS IN GROWTH MODELS - READ CHAPTER 9.

