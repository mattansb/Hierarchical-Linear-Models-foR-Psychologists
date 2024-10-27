
            ##  Cross-level interactions and pseudo R square  ##


library(dplyr)
library(datawizard)
library(ggplot2)

library(lmerTest) # for MLMs

library(performance) # for icc
library(parameters)
library(marginaleffects) # for contrasts and more


# The Data ------------------------------------------------------------------
# This data is based on real data collected as part of the BIDS research
# project.

Nemo <- read.csv("Nemo.csv") # The child-Flanker task

head(Nemo) 
# ID - subject ID (child)
# RT - reaction time in the task
# Condition - the Flanker condition - See "Nemo Flanker.png"


Symptoms <- read.csv("Symptoms.csv")

head(Symptoms)
# ID      - the child's ID
# inatten - inattention symptoms (as per Mom)
# hyper   - hyper activity symptoms (as per Mom)
# ADHD    - total symptoms (inatten + hyper)


# Let's first merge the two data sets
ADHD_data <- Symptoms |> 
  right_join(Nemo, by = "ID")

head(ADHD_data)



## Understanding the data --------------------

# The outcome is "RT"
# What is the random grouping variable? Why?
# What level are each variable on? Person (2)? Measurement (1)?

ADHD_data |> 
  filter(ID == "14")

# We can see that the variables from the "Symptoms" dataset were level 2 data,
# while the variables from the "Nemo" dataset indicate level 1 data.



# Our research question:
# We are interested in how the number of inattention symptoms affect the
# Facilitation and Interference effects. That is, inattention X Condition
# effect.

# This is a cross-level interaction - Why?
# - Inattention is measured in level 2
# - Condition is measured in level 1

## Data prep ----------------------------------------------
# coding + centering

# Next we need to code Condition as a factor. We will use the Neutral condition
# as the reference group - that way we have parameters for:
# 1. The Facilitation effect: {Congruent - Neutral}
# 2. The Interference effect: {Incongruent - Neutral}
ADHD_data <- ADHD_data |> 
  mutate(
    ID = factor(ID),
    Condition = factor(Condition),
    Condition = relevel(Condition, ref = "Neutral")
  )

levels(ADHD_data$Condition)
contrasts(ADHD_data$Condition)

# We *could* use effects coding if we wanted the intercept to be the overall
# mean, but that would make interpretation of the coefficients harder.


# Since we are interested in how the number of inattention symptoms affect the
# Facilitation and Interference effects = moderation. So we will also center the
# inattention symptoms, to aid in interpretation.
# As we know, the scale of all model predictors should have a meaningful 0.
# --> If the original scale of the predictor doesn't include 0, or if 0 is of
# little value or interest, then a constant should be subtracted or added-> a
# procedure known as CENTERING.
ADHD_data <- ADHD_data |> 
  mutate(inatten_c = center(inatten))
head(ADHD_data)




# Modeling ----------------------------------------------------------------



## Random intercept model -----------------------------------

# The starting point is to estimate the WP empty model* in which:

# FIXED EFFECTS - intercept
# RANDOM EFFECTS - intercept

# The model's equations (Hoffman's notation):
# Level-1 equation: 
#     RT_ji = b_0j + e_ji 
# Level-2 equation: 
#     b_0j = gamma_00 + U_0j
#
# Composite equation: 
#     RT_ji = (gamma_00 + U_0j) + e_ji 
#   Reaction Time of child j during measurement i = sample's mean (fixed
#   intercept) + the individual difference in child j's intercept + error for
#   child j in measurement i


mod_rndm.intr <- lmer(RT ~ 1 + (1|ID), 
                      data = ADHD_data)


icc(mod_rndm.intr)
# ~10% of the variance in RT is due to person mean differences.

ranova(mod_rndm.intr) # if you need it...



## RT ~ Condition model ---------------------------------------------------
# (Level 1 predictor model)

# adding the effect of Condition:
# Does children's RT change in different condition?

# FIXED EFFECTS - intercept, condition dummy variables
# RANDOM EFFECTS - intercept, condition dummy variables, and their co-variances

# The model's equations:
#
# Level-1 equation:
#   RT_ij =  b_0j + b_1j * Cong_d_ij + b_2j * Incong_d_ij + e_ji 
#
# Level-2 equations:
#     b_0j = gamma_00 + U_0j [mean of neutral condition for child j]
#     b_1j = gamma_10 + U_1j [Cong effect for child j]
#     b_2j = gamma_20 + U_2j [Incong effect for child j]
#
# Composite:
#     RT_ij = (gamma_00 + U_0j) + (gamma_10 + U_1j) * Cong_d_ij + 
#             (gamma_20 + U_2j) * Incong_d_ij + e_ji
#   RT of child j during obs. i= sample's mean RT in neutral condition + child
#   j's deviation from this mean + (Cong effect based on the general +
#   individual effects) * Cong_dummy value at obs. i + (Incong effect based on
#   the general+ individual effects) * Cong_dummy value at obs. i + error for
#   child j in obs. i


mod_wthn.prsn <- lmer(RT ~ Condition + (Condition | ID), 
                      data = ADHD_data)

model_parameters(mod_wthn.prsn, ci_method = "S")
# We can see that there is a large [-414.06,  -23.03] interference effect.


fixef(mod_wthn.prsn) # the "gamma"s
ranef(mod_wthn.prsn) # the "u"s
VarCorr(mod_wthn.prsn) # This function shows us the random (co)variance components

# The interference effect (ConditionIncong) is related to the intercept
# (r=-0.72). This means that individuals who are slower in the neutral
# condition, tend to show larger (more negative) interference effects!

coefficients(mod_wthn.prsn)[["ID"]] |>
  ggplot(aes(`(Intercept)`, ConditionIncong)) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  theme_bw() + 
  scale_x_continuous(expression(Intercept[j]), labels = scales::label_comma()) + 
  scale_y_continuous(expression("Interference"[j]), labels = scales::label_comma())


# We can also see there is *a lot* of within-person variation that is not
# explained by the Flanker task.


# And indeed, <1% in RTs is explained by the Flanker condition.
r2_nakagawa(mod_wthn.prsn)
?r2_nakagawa
# The marginal r-squared considers only the variance of the fixed effects, while
# the conditional r-squared takes both the fixed and random effects into
# account.



## RT ~ Condition + Symptoms model ---------------------------------------------
# Adding level 2 predictor

# We will now add ADHD to the model. 
# For now, only as main effect (we will get to the moderation...)


# Can ADHD be random? 
# Not in this data!


# so...
# FIXED EFFECTS- intercept, condition dummy variables , symptoms
# RANDOM EFFECTS- intercept, condition dummy variables, and their co-variances

# The model's equations:
#
# Level-1 equation:
#   RT_ij =  b_0j + b_1j * Cong_d_ij + b_2j * Incong_d_ij + e_ji 
#
# Level-2 equations:
#     b_0j = gamma_00 + gamma01 * Symptoms_j + U_0j
#     b_1j = gamma_10 + U_1j
#     b_2j = gamma_20 + U_2j
#
# Composite:
#     RT_ij = (gamma_00 + gamma01 * Symptoms_j + U_0j) + 
#             (gamma_10 + U_1j) * Cong_d_ij + 
#             (gamma_20 + U_2j) * Incong_d_ij + e_ji
#   RT of child j during obs. i= sample's mean RT in neutral condition + ADHD
#   Symptoms effect on RT + child j's deviation from this mean (*after*
#   accounting for symptoms) + (Cong effect based on the general + individual
#   effects) * Cong_dummy value at obs. i + (Incong effect based on the general+
#   individual effects) * Cong_dummy value at obs. i + error for child j in obs.
#   i.


mod_adhd <- lmer(RT ~ inatten_c + Condition + (Condition | ID), 
                 data = ADHD_data)

anova(mod_adhd, mod_wthn.prsn, refit = TRUE) # fit isn't better

(mod_adhd_mp <- model_parameters(mod_adhd, ci_method = "S", effects = "fixed"))
# We can see that the effect for inatten is rather small (an increase in 1
# symptom amounts to [-14, +34] ms change in reaction times - in children).


# We can also quantify this addition to the model with Pseudo-R2!


### Pseudo R2 ---------------

# Pseudo R2 is calculated when adding FIXED effects!
# We won't check Pseudo R2 after adding random effects!


# We have added a level 2/ person-level/ BP variable->
# We are looking at the change in variance to the random intercepts.
VarCorr(mod_wthn.prsn)
VarCorr(mod_adhd)
# Now it is SD(Intercept) = 471.84
# Before it was SD(Intercept) = 477.99

(Psd_R2 <- 1 - (471.84 ^ 2) / (477.99 ^ 2))
# Therefore, 2.5% of the between-person variance in the neutral condition is
# accounted for by ADHD symptoms (at least in this data set).

# Compare to:
r2_nakagawa(mod_adhd)

# When explained variance was quite similar in the previous model:
r2_nakagawa(mod_wthn.prsn)


# Extra Note:
# It appears only ~0.1% of the total variance in RTs was related to person mean
# differences predictable by inattention, a small effect size at best. It makes
# sense when thinking about the fit indices that were not very definitive
# regarding the improvement in fit vs. number of predictors trade-off (and even
# supported the previous model).


# These are 3 approaches out of more. As this example illustrates the methods
# for calculating the explained variance will not converge on the same estimates
# and this is why it will be important to always describe exactly how any R2
# values were obtained in reporting your results.




### Pseudo beta ---------------

# We can also look at the pseudo-standardized coefficients.
# BP-standardized coef. = (BP-raw coef.) * (BPpredictorStd.Dev) / (BPoutcomeStd.Dev)
# WP-standardized coef. = (WP-raw coef.) * (WPpredictorStd.Dev) / (WPoutcomeStd.Dev)

# We first need the variances from the first model:
VarCorr(mod_rndm.intr)
sd_within <- 1178.48
sd_between <- 387.38


# There are our coefficients:
mod_adhd_mp
# e.g., the standardized effect of inatten:
9.60 * sd(Symptoms$inatten) / sd_between

# e.g., the standardized effect of the interference effect is harder (because it
# is a factor), but we can find it:
-216.86 * sd(ADHD_data$Condition == "Incong", na.rm = TRUE) / sd_within


# Or we can just get the whole thing directly:
model_parameters(mod_adhd, ci_method = "S", effects = "fixed",
                 standardize = "pseudo")

# Note that the level 2 effect of inatten (0.11) is stronger than the level 1
# effect (-0.09) - how can it be the weaker effect is the significant one??

# It is because of POWER - the level 1 predictor as all the repeated measures
# from all the subjects to support the effect. Whereas the level 2 predictor has
# only the single measures from each subject to support an effect.




## Cross level interaction  ------------------------

# Now we want to see how inatten moderated the Flanker effects!

# FIXED EFFECTS- intercept, condition dummy variables , symptoms, 
#                AND symptoms interactions with condition dummy variables
# RANDOM EFFECTS- intercept, condition dummy variables, and their co-variances


mod_cli <- lmer(RT ~ inatten_c + Condition + inatten_c:Condition + (Condition | ID), 
                data = ADHD_data)
# Or
mod_cli <- lmer(RT ~ inatten_c * Condition + (Condition | ID), 
                data = ADHD_data)



# The model's equations:
#
# Level-1 equation:
#   RT_ij =  b_0j + b_1j * Cong_d_ij + b_2j * Incong_d_ij + e_ji 
#
# Level-2 equations:
#     b_0j = gamma_00 + gamma01 * Symptoms_j + U_0j
#     b_1j = gamma_10 + gamma11 * Symptoms_j + U_1j
#     b_2j = gamma_20 + gamma21 * Symptoms_j + U_2j
#
# Composite:
#     RT_ij = (gamma_00 + gamma01 * Symptoms_j + U_0j) + 
#             (gamma_10 + gamma11 * Symptoms_j + U_1j) * Cong_d_ij + 
#             (gamma_20 + gamma21 * Symptoms_j + U_2j) * Incong_d_ij + e_ji
#   (gamma11 and gamma21 are the cross-level interaction estimates)

model_parameters(mod_cli, ci_method = "S", effects = "fixed") # dummy var interactions not sig

# comparing with model with no int.:

anova(mod_cli, mod_adhd)
# All arrow point at no interaction....




### Effect sizes --------------------

r2_nakagawa(mod_cli) # total explained variance

# Looking Pseudo R2:
VarCorr(mod_cli)
VarCorr(mod_adhd)

(PSD_R2_interference <- 1 - (600.27 / 600.45) ^ 2)
# Very little compared to the previous model...


# And Pseudo Std. Coef:
model_parameters(mod_cli, ci_method = "S", effects = "fixed",
                 standardize = "pseudo")





### Follow up analysis --------------

# Just like with linear models, we can conduct followup analyses here too. (Yes,
# interaction wasn't significant, so we'll explore it just for the sport...).
#
# We've seen how to do these with {emmeans}. We will now use
# {marginaleffects}... for sport.
# https://marginaleffects.com/




plot_predictions(mod_cli, condition = c("inatten_c", "Condition"),
                 re.form = NA, vcov = "satterthwaite") + 
  scale_color_brewer(breaks = c("Cong", "Neutral", "Incong"),
                     labels = c("Congruent", "Neutral", "Incongruent"),
                     type = "qual", 
                     aesthetics = c("fill", "color")) + 
  scale_y_continuous(labels = scales::label_comma()) + 
  labs(x = "Inattention Symptoms [centered]") + 
  theme_bw()



plot_predictions(mod_cli, condition = list("Condition", inatten_c = mean_sd),
                 re.form = NA, vcov = "satterthwaite") + 
  scale_color_brewer("Inattention Symptoms\n[centered]", type = "div",
                     labels = c("-SD", "Mean", "+SD")) + 
  scale_y_continuous(labels = scales::label_comma()) + 
  scale_x_discrete(limits = c("Cong", "Neutral", "Incong"),
                   labels = c("Congruent", "Neutral", "Incongruent")) + 
  theme_bw()






#### Simple slopes (Condition as moderator) ------------
avg_slopes(mod_cli, variables = "inatten_c", by = "Condition",
           # Set this to indicate we're interested in the fixed effects!
           re.form = NA, vcov = "satterthwaite") 
# The (n.s.) trends for the between person effects are:
# - In Neutral and Cong, subjects with *more* inattentive symptoms are slower.
# - In the Incong condition, they are slower.




#### Simple contrasts (inatten as moderator) -----------

avg_comparisons(mod_cli, variables = list("Condition" =  "reference"), by = "inatten_c",
                # We want to look as specific values of inattention:
                newdata = datagrid(inatten_c = mean_sd),
                # Set this to indicate we're interested in the fixed effects!
                re.form = NA, vcov = "satterthwaite")
# For low inatten, interference effect is weak.
# For mean inatten, interference effect is larger.
# For high inatten, interference effect is largest.
#
# (Reminder: the interaction = the difference between them, is not significant.)










