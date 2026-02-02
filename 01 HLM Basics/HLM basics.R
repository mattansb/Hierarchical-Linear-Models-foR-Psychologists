# All tutorial materials were originally developed by Yael Bar-Shachar (no relation).

### (1) Intro to HLM ###

# For data preparation:
library(tidyverse)

# For running HLMs:
library(lmerTest) # Uses {lme4}
# Additional packages:
# - {nlme}: more flexible R matrix; limited to simple random structures
# - {glmmTMB}: more flexible; can be less stable
# - {brms}: MOST flexible; Bayesian

# For simple slope analysis:
library(emmeans)
emm_options(lmer.df = "satterthwaite") # we'll talk about this next week

# For getting ICC and R2:
library(performance)
# For tidy parameter tables with CIs
library(parameters)
library(merDeriv)


# Two-occasion data (Ch. 3- Hoffman) -------------------------------------

# getting the DATA:
temp <- tempfile()
download.file(
  "https://www.pilesofvariance.com/Chapter3a/SPSS/SPSS_Chapter3a.zip",
  temp
)
dataset <- haven::read_sav(unz(temp, "SPSS_Chapter3a/SPSS_Chapter3a.sav"))
unlink(temp)


head(dataset)
# This data is in the wide format - each row is a person, and we have two
# columns for the outcome and a column for the group.
# To model this data in an HLM, we need to convert it to the long format.

dataset_long <- dataset |>
  # Converting data from wide to long\stacked format
  pivot_longer(
    cols = c(outcome1, outcome2),
    names_to = "Time",
    values_to = "outcome"
  ) |>
  # We'll cleanup the variable
  mutate(
    PersonID = factor(PersonID),
    group = factor(group, levels = c(1, 2), labels = c("G1", "G2")),
    Time = ifelse(Time == "outcome1", 0, 1)
  )
# Note, we "centered" time to the first assessment! -> "T1" is 0.
# We will talk more about centering in the future... For now, just think of the
# making the predictors' 0 point *meaningful*.

head(dataset_long)
glimpse(dataset_long)


## Understanding the data --------------------

# The outcome is "outcome" (duh)
# What is the random grouping variable? Why?
# What level are the predictors on? Person (2)? Measurement (1)?

dataset_long |>
  filter(PersonID == "1")


## Spaghetti plots -------------------------------------------------
# Lets' just see the patterns in the data:

# Spaghetti plots- change in outcome by time:
p_spaghetti <-
  ggplot(dataset_long, aes(x = Time, y = outcome, color = PersonID)) +
  # add a line for each ID but don't show use a legend
  geom_point() +
  geom_line() +
  # add the pattern of the mean slope (linear pattern)
  stat_smooth(method = "lm", se = FALSE, color = "black", linewidth = 2) +
  scale_x_continuous(breaks = c(0, 1)) +
  theme_bw() +
  guides(color = "none")

p_spaghetti


# Empty Model ---------------------------------------------------------------
# AKA intercept-only models

## Empty BP Model:
#-> simply using lm()
mod_empty <- lm(
  outcome ~ 1, # 1 is for including the intercept
  data = dataset_long
)
summary(mod_empty)


# Tidy parameters:
model_parameters(mod_empty)


# and yes, this intercept is just the outcome mean ...
mean(dataset_long$outcome)


# Random Intercept Model ----------------------------
# We need lmer() for HLM (i.e. adding random intercepts)

mod_rndm.intr <- lmer(
  outcome ~
    1 +
    # the syntax start the same for FIXED effects
    (1 | PersonID),
  # BUT, we will add the RANDOM effects with Wilkinson's
  # notation:
  # (varying effect\s | random grouping variable)
  REML = TRUE,
  # This is default (we can omit this argument), set to
  # FALSE for ML fitting
  data = dataset_long
)

summary(mod_rndm.intr)

# see tidy parameters:
model_parameters(mod_rndm.intr, ci_method = "Satterthwaite") # We'll discuss these next week

# Check out that the output refers to:
# 1. Fixed effects
# 2. Random effects

## Computing ICC ------------------------------

# We use the empty model for producing the intra-class correlation (ICC), which
# is actually:
#   Sigma{a} square / Total variance (Sigma{a} square + Sigma{e} square)

(3.50)^2 / ((3.50)^2 + (5.31)^2)


# OR...
# just use the icc() function from "performance" library.
icc(mod_rndm.intr)
# (For now we'll ignore the "Adjusted" / "Unadjusted" distinction. The model is
# an empty model - both are the unconditional ICC)

## Extracting model parts ---------------------------------

# This model's notation would be:
# outcome_ij = b_0j + e_ij
#       b_0j = gamma_00 + U_0

# We can extract each of these parts:
fixef(mod_rndm.intr) # gamma_00
ranef(mod_rndm.intr) # U_0
coef(mod_rndm.intr) # b_0j (check that these are equal to gamma_00 + U_0)
residuals(mod_rndm.intr) # e_ij


# Maximal model ---------------------------------------------------------
# We want an effect for Time, group, and their interaction.

p_spaghetti + facet_grid(cols = vars(group))


# What would be the random effects? Are they all identifiable?

# Since Time is a level 1 predictor, we might want to estimate the heterogeneity
# of its effect by adding a random slope for it:
lmer(outcome ~ Time * group + (1 + Time | PersonID), data = dataset_long)
# But we cannot - we don't have enough observations to identify both the level 1
# variance _and_ the level 2 variance of the effect of time.

## Fitting model(s) ------------------------------------

# Fitting the mixed model:
mod_max.mixed <- lmer(
  outcome ~ Time * group + (1 | PersonID),
  REML = FALSE, # We'll talk bout this next week
  data = dataset_long
)


# Lets ignore the nested nature of the data, and fit the SAME fixed effects:
mode_max.fixed <- lm(outcome ~ Time * group, data = dataset_long)


# We can directly compare the model with sjPlot::tab_model():
sjPlot::tab_model(
  mode_max.fixed,
  mod_max.mixed,
  dv.labels = c("No Random Effects", "Mixed Effects Model"),

  show.se = TRUE,
  show.df = TRUE,
  df.method = "satterthwaite"
)
# The interaction's SE, CIs and p have changed - why?
# Will adding random effects always increase power? What does this depend on?
p_spaghetti + facet_grid(cols = vars(group)) # What can we see here?


# This model can also be fit with an rmANOVA - and it will be equivalent!
afex::aov_ez(
  id = "PersonID",
  dv = "outcome",
  data = dataset_long,
  between = "group",
  within = "Time"
)
# Notice that the df and p value for the interaction are nearly identical!
# (They are NOT the same for Time and group - in the ANOVA they are main effect,
# in the LMM they are simple effects. We will discuss ANOVA tables for LMMs later
# in the semester.)

## Simple slopes analysis --------------------------

# For significant interaction we would like to test simple slopes...

# If you're not already familiar with the {emmeans} package - it's a great tool
# for post-hoc/contrasts/simple slope analysis of many types of models in R.

(ems <- emmeans(mod_max.mixed, ~ Time + group)) # Get a "grid" of means
contrast(ems, method = "revpairwise", by = "group") # conpute contrasts


# Since Time is "numeric" we can also extract "slopes" (same result here)
emtrends(
  mod_max.mixed, # the model
  var = "Time", # the focal predictor
  ~group, # the moderator
  infer = TRUE
)


# see how Time effect for group 0 is equal to the effect of time
# in the main model:
model_parameters(mod_max.mixed, ci_method = "S")


## Extracting model parts ---------------------------------

# This model's notation would be:
# outcome_ij = b_0j + b_1 * Time + e_ij
#       b_0j = gamma_00 + gamma_01 * group + U_0
#       b_1j = gamma_10 + gamma_11 * group

# We can extract each of these parts:
fixef(mod_max.mixed) # gamma
ranef(mod_max.mixed) # U_0
coef(mod_max.mixed) # b_0j (but also all the other fixed values)
residuals(mod_max.mixed) # e_ij
