library(tidyverse)
library(patchwork)

library(lmerTest)

library(performance)
library(parameters)

# load r2_pseudo function
source(
  "https://github.com/mattansb/Hierarchical-Linear-Models-foR-Psychologists/raw/refs/heads/main/helpers.R"
)

# The data ----------------------------------------------------------------

order_data <- read.csv("order.csv")
head(order_data)
# The task: subjects were presented with 3 circles filled with of dots.
# They had to decide if *all* of circles had the same number of dots in them, or
# if at least one had a different number of dots (See task-stimuli.png)

order_data_correct <- order_data |>
  # Drop subjects that didn't understand the task
  filter(mean(acc[Condition == "partial rep"]) > 0.6, .by = Subject) |>
  # Keep only correct trials
  filter(acc == 1) |>
  select(-acc) |>
  # Keep only some of the conditions
  filter(Condition %in% c("Ascending", "Descending", "Non Order")) |>
  mutate(
    Condition = factor(
      Condition,
      levels = c("Non Order", "Ascending", "Descending")
    ),
    Subject = factor(Subject),
    Stim = factor(Stim),
    Sex = factor(Sex, levels = c("Male", "Female"))
  )


# The variables:
head(order_data_correct)
#   Subject - Subject ID
nlevels(order_data_correct$Subject)
#       Sex - sex of the Subject
#      Stim - stimulus ID (number of dots in the left, middle and right circles)
nlevels(order_data_correct$Stim)
# Condition - what type of stimulus is it?
#       acc - was the response correct?
#        rt - reaction time

## Understanding the data --------------------

# The outcome is "rt".
# What is the random grouping variable(s)? Why? What is the relationship between
# them? What level are the other variables on?

order_data_correct |>
  filter(Subject == "402")

order_data_correct |>
  filter(Stim == "L1.M2.R4") |>
  head(n = 20)


# The research questions: Does the order (or lack thereof) of the dots
# (ascending or descending) facilitate the processing of the stimuli?

# Random intercept model ------------------

mod_rndm.intr <- lmer(
  rt ~ 1 + (1 | Subject) + (1 | Stim),
  data = order_data_correct
)

icc(mod_rndm.intr)
icc(mod_rndm.intr, by_group = TRUE) # ICC for each group
# We can see that there is considerable variance both between Subjects and
# between Stimuli! That means that not only do subjects differ in how fast they
# are over all, but also stimuli differ in how fast they are responded to!

ranova(mod_rndm.intr) # Both are significant

# What does this look like?
p_subject <- order_data_correct |>
  mutate(
    Subject = forcats::fct_reorder(Subject, rt, .fun = mean)
  ) |>
  ggplot(aes(Subject, rt)) +
  geom_point(alpha = 0.4, shape = 16) +
  stat_summary(geom = "point", fun = "mean", color = "red", size = 2) +
  theme_bw() +
  theme(
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.x = element_blank()
  )

p_stim <- p_subject +
  aes(x = Stim) +
  (order_data_correct |>
    mutate(
      Stim = forcats::fct_reorder(Stim, rt, .fun = mean)
    ))

p_subject + p_stim + plot_layout(widths = c(2, 3))


# Condition effect -----------------------

mod_fix.cond <- lmer(
  rt ~ Condition + (1 | Subject) + (1 | Stim),
  data = order_data_correct
)

# Since Condition is a Stim-level effect, it should explain some of the
# differences between the stimuli (in their intercepts):
VarCorr(mod_rndm.intr)
VarCorr(mod_fix.cond)

r2_pseudo(mod_fix.cond, mod_rndm.intr)[1, ]
# Accounting for ~0% of the variance in the time it take to respond to them.

anova(mod_fix.cond, mod_rndm.intr)
# This effect if non-sig, and both BIC and AIC support the simpler model.

## Random slopes --------

# Condition is nested *within* subject, so we can model that random slope(s):

mod_rndm.cond <- lmer(
  rt ~ Condition + (Condition | Subject) + (1 | Stim),
  data = order_data_correct
)

anova(mod_rndm.cond, mod_fix.cond, refit = FALSE)
# There *might* be a difference between subjects in their order-effects.

model_parameters(mod_rndm.cond, ci_method = "S")
# We can see that ordered stimuli are responded to faster, but there results are
# not significant.
