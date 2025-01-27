
library(dplyr)
library(ggplot2)

library(lmerTest)

library(performance)
library(parameters)

# The data ----------------------------------------------------------------

order_data <- read.csv("order.csv")
head(order_data)
# The task: subjects were presented with 3 circles filled with of dots. They had
# to decide if all of circles had the same number of dots in them, or not. (See
# task-stimuli.png)


order_data_correct <- order_data |> 
  as_tibble() |> 
  # Keep only correct trials
  filter(acc == 1) |> 
  select(-acc) |> 
  # Keep only some of the conditions
  filter(Condition %in% c("Ascending", "Descending", "Non Order")) |> 
  mutate(
    Condition = factor(Condition) |> relevel(ref = "Non Order")
  )


# The variables:
head(order_data_correct)
#   Subject - Subject ID
#       Sex - sex of the Subject
#      Stim - stimulus ID (number of dots in the left, middle and right circles)
# Condition - what type of stimulus is it?
#       acc - was the response correct?
#        rt - reaction time



## Understanding the data --------------------

# The outcome is "rt".
# What is the random grouping variable(s)? Why? What is the relationship between
# them? What level are the other variables on? School (3)? Person (2)?
# Measurement (1)?

order_data_correct |> 
  filter(Subject == "402")

order_data_correct |> 
  filter(Stim == "L1.M2.R4") |> 
  print(n = 20)




# The research questions: Does the order (or lack thereof) of the dots
# (ascending or descending) facilitate the processing of the stimuli?





# Random intercept model ------------------

mod_rndm.intr <- lmer(rt ~ 1 + (1 | Subject) + (1 | Stim),
                      data = order_data_correct)

icc(mod_rndm.intr)
icc(mod_rndm.intr, by_group = TRUE) # ICC for each group
# We can see that there is considerable variance both between Subjects and
# between Stimuli! That means that not only do subjects differ in how fast they
# are over all, but also stimuli differ in how fast they are responded to!

ranova(mod_rndm.intr) # Both are significant





# Condition effect -----------------------

mod_fix.cond <- lmer(rt ~ Condition + (1 | Subject) + (1 | Stim),
                     data = order_data_correct)

# Since Condition is a Stim-level effect, it should explain some of the
# differences between the stimuli (in their intercepts):
VarCorr(mod_rndm.intr)
VarCorr(mod_fix.cond)

1 - (120.02 / 115.22)^2
# Accounting for ~0% of the variance in the time it take to respond to them.

anova(mod_fix.cond, mod_rndm.intr) 
# This effect if non-sig, and both BIC and AIC support the simpler model.


## Random slopes --------

# Condition is nested *within* subject, so we can model that random slope(s):

mod_rndm.cond <- lmer(rt ~ Condition + (Condition | Subject) + (1 | Stim),
                      data = order_data_correct)

anova(mod_rndm.cond, mod_fix.cond, refit = FALSE)
# Not clear if there's a difference between subjects in their order-effects.



model_parameters(mod_rndm.cond, ci_method = "S")
# We can see that ordered stimuli are responded to faster, but there results are
# not significant.




# Exercise ----------------------------------------------------------------

# 1. Add sex as a fixed effect.
# 2. Can sex be added as a random slope? Do it. Interpret the fixed effect for
#    sex. Plot your results.
# 3. Go back to the mod_rndm.cond mode, and remove the random intercept for
#    stim. How has this affected the significance of the effect of condition?
#    What can we learn from this?



