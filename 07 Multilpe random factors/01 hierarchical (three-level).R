
library(dplyr)
library(ggplot2)
library(patchwork)

library(lmerTest)

library(performance)
library(parameters)
library(marginaleffects)

# The data ----------------------------------------------------------------

egsingle <- mlmRev::egsingle

# This data is a subset of the mathematics scores from the U.S. Sustaining
# Effects Study. 
head(egsingle)
?mlmRev::egsingle


# The subset consists of information on 1721 students from 60 schools.
nlevels(egsingle$schoolid)
nlevels(egsingle$childid)


## Understanding the data --------------------

# The outcome is "math".
# What is the random grouping variable(s)? Why? What is the relationship between
# them? What level are the other variables on? School (3)? Person (2)?
# Measurement (1)?

egsingle |> 
  filter(childid == "273026452")

egsingle |> 
  filter(schoolid == "4440")


# We are interested in the the linear growth in math scores across time (grade)
# and how the percentage of lower income students in the school affects the
# growth.



# Random intercepts model ----------------------------------------------------

mod_rndm.intr <- lmer(math ~ 1 + (1 | childid:schoolid) + (1 | schoolid),
                      data = egsingle)



icc(mod_rndm.intr) # variance explained by child and school.
icc(mod_rndm.intr, by_group = TRUE) # stability within Child (within school) and within school.
# We can see that there is some variability between school as well as between
# the students within each of the schools.

ranova(mod_rndm.intr)
# We can see that both random intercept terms are significant and substantial.

set.seed(42)
egsingle |> 
  mutate(
    childid = forcats::fct_reorder(childid, math)
  ) |> 
  filter(schoolid %in% sample(levels(schoolid), 12)) |> 
  ggplot(aes(childid, math)) + 
  facet_wrap(facets = vars(schoolid), scales = "free_x") + 
  geom_point() + 
  labs(x = "Child Within School", y = "Math") + 
  theme_classic() + 
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.x = element_blank())







# The model formula:
# Level 1: 
#     math_ijk = b_0jk + e_ijk
#
# Level 2 (Child): 
#     b_0jk = gamma_00k + U_0jk
#
# Level 3 (School): 
#     gamma_00k = zeta_00 + U_0k
#
# Composite:
#     symptoms_ji = math_ijk = zeta_00 + U_0k + U_0jk + e_ijk
#
# We can get these U values:
U_values <- ranef(mod_rndm.intr)
names(U_values)




# Growth model ------------------------------------------------------------

# grade=0 is preschool ("trom hova"). This is suitable for interpretation.

p_child <- ggplot(egsingle, aes(grade, math)) +
  stat_smooth(aes(group = interaction(schoolid, childid)), 
              method = "lm", se = FALSE,
              alpha = 0.2) +
  stat_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Trend per Child (nested in School)", 
       x = "Grade", y = "Math") + 
  lims(y = c(-5, 5)) + 
  theme_bw()

p_school <- ggplot(egsingle, aes(grade, math)) +
  stat_smooth(aes(group = schoolid), 
              method = "lm", se = FALSE, color = "purple",
              alpha = 0.2) +
  stat_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Trend per School", 
       x = "Grade", y = "Math") + 
  lims(y = c(-5, 5)) + 
  theme_bw()

p_school + p_child





mod_grade <- lmer(math ~ grade + (1 | childid:schoolid) + (1 | schoolid),
                  data = egsingle)
anova(mod_grade, mod_rndm.intr)
# Not surprisingly, the growth model is supported.

# Which variance component is explained by this linear growth?
# How would be compute the relevant pseudo-R2?
VarCorr(mod_rndm.intr)
VarCorr(mod_grade)


model_parameters(mod_grade, ci_method = "S")
# There is a positive linear trend such that the IRT improves by 0.77 points 
# from year to year on average.
# What does this significance refer to?



## Adding random slopes --------------------------------

# Since the growth trend is nested within subject and within school, we can
# estimate the heterogeneity of this effect between schools and within school
# between children.

# We can start by estimating the within child random effect:
mod_rndm.grade <- lmer(math ~ grade + (grade | childid:schoolid) + (1 | schoolid),
                       data = egsingle)
anova(mod_rndm.grade, mod_grade, refit = FALSE)
# We can see that children do differ in their linear growth






# Now let's add the random slope for schools:
mod_rndm.grade2 <- lmer(math ~ grade + (grade | childid:schoolid) + (grade | schoolid),
                        data = egsingle,
                        control = lmerControl("bobyqa"))

VarCorr(mod_rndm.grade)
VarCorr(mod_rndm.grade2)
# We can see that the child-level variance in the growth slope has reduced:
1 - (0.063096 / 0.12190)^2
# 73% of the variance between how children differ in their growth can be
# attributed to different schools showing different growth.





## Conditional growth model -----------------------------------------------

# Let's examine the interacting effect of percentage of low-income students in
# the school on the linear growth.
ggplot(egsingle, aes(grade, math)) +
  stat_smooth(aes(group = schoolid, color = lowinc), 
              method = "lm", se = FALSE,
              alpha = 0.2) +
  stat_smooth(method = "lm", se = FALSE, color = "black") +
  scale_color_distiller("% Low\nIncome", type = "div",
                        palette = 3, direction = -1,
                        breaks = seq(0, 100, by = 20)) + 
  labs(title = "Trend per school",
       x = "Grade", y = "Math") + 
  lims(y = c(-5, 5)) + 
  theme_bw()
# From the plot, we can see that schools with a lower income student body have
# lower math grades. But are the slopes also different?


mod_lowinc <- lmer(math ~ grade * lowinc + 
                     (grade | childid:schoolid) + (grade | schoolid),
                   data = egsingle,
                   control = lmerControl("bobyqa"))
anova(mod_lowinc, mod_rndm.grade2)
# The models are different - but the difference between models accounts for the
# main effect and the interaction.


model_parameters(mod_lowinc, ci_method = "S")
# - The simple effect of grade (for school with no lower income students) is 0.88.
# - The simple effect of lowinc (at grade=0) is -0.01 -> the more lower income
#   the student body is, the lower the math grades at grade 0 are.
# - The interaction term is significant and negative.

# We can probe the interaction:
slopes(mod_lowinc, variables = "grade", 
       newdata = datagrid(childid = NA, schoolid = NA, lowinc = c(0, 50, 100)),
       re.form = NA, vcov = "satterthwaite")
# We can see that across different %s of lower-income students, the growth model
# is positive and of very similar magnitude.



# Since the interaction is a cross level interaction (levels 1 and 3) we can see
# how much of the differences between the schools in their linear growth is
# explained by lowinc (pseudo-R2).
VarCorr(mod_lowinc)
VarCorr(mod_rndm.grade2)
1 - (0.108592 / 0.114115)^2
# 9% of the variance in the growth is explained by lowinc!



# Questions ----------------------------------------------------------------

# 1. Can we add a random slope for lowinc?
# 2. We also have the variable of "female".
#   - Adding it as a fixed effect will explain which variance component?
#   - Can we add it as a random slope?


