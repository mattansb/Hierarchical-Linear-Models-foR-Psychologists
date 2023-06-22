library(dplyr)
library(lmerTest)
library(performance)
library(ggplot2)
library(parameters)

order_data <- read.csv("order.csv")
head(order_data)
# The task: subjects were presented with 3 circles filled with of dots. They had
# to decide if all of circles had the same number of dots in them, or not.
# (See task-stimuli.png)
#
# The research questions:
# 1. If the number of dots are in order (ascending or descending), does that
#   facilitate the processing of the stimuli?
# 2. Are there sex differences in the processing of the stimuli?
# 

# The variables:
head(order_data)
#   Subject - Subject ID
#       Sex - sex of the Subject
#      Stim - stimulus ID (number of dots in the left, middle and right circles)
# Condition - what type of stimulus is it?
#       acc - was the response correct?
#        rt - reaction time


# - What are the random variables?
# - Are what level are each of the effects, and within which random variable are
#   they nested?

# Keep only correct trials
order_data_correct <- order_data |> 
  filter(acc == 1,
         Condition %in% c("Ascending", "Descending", "Non Order")) |> 
  mutate(Condition = factor(Condition) |> relevel(ref = "Non Order"))


# Fit ---------------------------------------------------------------------

## Random intercept model ------------------

m0 <- lmer(rt ~ 1 + (1 | Subject) + (1 | Stim),
           data = order_data_correct)

icc(m0, by_group = TRUE) # by_group = TRUE gives us the ICC for each group!
# We can see that there is considerable variance both between Subjects and
# between Stimuli! That means that not only do subjects differ in how fast they
# are over all, but also stimuli differ in how fast they are responded to!

ranova(m0) # Both are significant


## Condition effect -----------------------

m1 <- lmer(rt ~ Condition + (1 | Subject) + (1 | Stim),
           data = order_data_correct)

# Since Condition is a Stim-level effect, it should explain some of the
# differences between the stimuli (in their intercepts):
VarCorr(m0)
VarCorr(m1)

1 - (120.02 / 115.22) ^ 2
# Accounting for ~0% of the variance in the time it take to respond to them.

anova(m1, m0) 
# However, this effect if non-sig, and both BIC and AIC support the simpler
# model.


### Random effect --------

# Condition is nested *within* subject, so we can model that random slope(s):

m1r <- lmer(rt ~ Condition + (Condition | Subject) + (1 | Stim),
            data = order_data_correct)

anova(m1, m1r, refit = FALSE)
# Seems like there *isn't* much difference between subjects in their
# order-effects.

model_parameters(m1r)
VarCorr(m1r)
# We can see that ordered stimuli are responded to faster, but there results are
# not significant.




## Sex effect ----------------------------------------------------------


m2 <- lmer(rt ~ Condition + Sex + (Condition | Subject) + (1 | Stim),
           data = order_data_correct)
anova(m2, m1r) # Oof... rough.

summary(m2) # Here the effect of sex is just significant
# Males showing slower responses.


# Since Sex is a Subject-level effect, it should explain some of the differences 
# between the subjects (in their intercepts):
VarCorr(m1r)
VarCorr(m2)

1 - (143.403 / 158.374) ^ 2
# Accounting for ~18% of the variance in the time it take to respond!



### Random effect --------

# Sex is nested *within* Stim, so we can model that random slope(s):

m2r <- lmer(rt ~ Condition + Sex + (Condition | Subject) + (Sex | Stim),
            data = order_data_correct)

anova(m2, m2r, refit = FALSE)
# Seems like some Stimuli evoke larger sex differences than others!

VarCorr(m2r)



coef(m2r)[["Stim"]] |> 
  tibble::rownames_to_column("Stim") |> 
  left_join(order_data_correct |> 
              distinct(Stim, Condition),
            by = "Stim") |> 
  mutate(Stim = forcats::fct_reorder(Stim, SexMale)) |> 
  ggplot(aes(SexMale, Stim, color = Condition)) + 
  geom_point() 

# No clear pattern....
# How would we test if the type of Stim is related to the sex differences?





# Exercise ----------------------------------------------------------------

# Remove the random effect for Stim and test the effect of condition.
# 1. How does this differ from what we found above?
# 2. What can we learn from this difference?




