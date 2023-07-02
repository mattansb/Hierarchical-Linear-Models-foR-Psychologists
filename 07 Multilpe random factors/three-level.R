library(dplyr)
library(lmerTest)
library(performance)
library(ggplot2)
library(parameters)
library(emmeans)

egsingle <- read.csv("egsingle.csv") |> 
  mutate(sex = factor(sex, levels = c("Male", "Female")))

# This data is a subset of the mathematics scores from the U.S. Sustaining
# Effects Study. The subset consists of information on 1721 students from 60
# schools.

head(egsingle)
# schoolid - school identifiers
#   lowinc - percentage of low-income students in the school
#  childid - child identifiers
#      sex - sex (M/F) of the child
#    grade - the grade the child is in for this measurement 
#     math - the IRT math scale score 


# We are interested in the the linear growth in math scores across time (grade)
# and how the percentage of lower income students in the school affects the
# growth.


# Intercepts only ---------------------------------------------------------

m0 <- lmer(math ~ 1 + (1 | childid:schoolid) + (1 | schoolid),
           data = egsingle)

RE <- ranef(m0)
names(RE)

icc(m0) # variance explained by child and school.
icc(m0, by_group = TRUE) # stability within Child (within school) and within school.
# We can see that there is some variability between school as well as between
# the students within each of the schools.

ranova(m0)
# We can see that both random intercept terms are significant.



# Growth model ------------------------------------------------------------

# grade=0 is preschool ("trom hova"). This is suitable for interpretation.

ggplot(egsingle, aes(grade, math)) +
  geom_smooth(aes(group = interaction(schoolid, childid)), 
              method = "lm", se = FALSE,
              alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Trend per child:school")

ggplot(egsingle, aes(grade, math)) +
  geom_smooth(aes(group = schoolid), 
              method = "lm", se = FALSE,
              alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Trend per school")


m1 <- lmer(math ~ grade + (1 | childid:schoolid) + (1 | schoolid),
           data = egsingle)
anova(m1, m0)
# Not surprisingly, the growth model is supported.


model_parameters(m1, effects = "fixed", ci_method = "S")
# There is a positive linear trend such that the IRT improves by 0.77 points 
# from year to year on average.



## Random effects --------------------------

# Since the growth trend is nested within subject and within school, we can
# estimate the heterogeneity of this effect between schools and within school
# between children.

# We can start by estimating the within child random effect:
m1b <- lmer(math ~ grade + (grade | childid:schoolid) + (1 | schoolid),
            data = egsingle)
anova(m1, m1b, refit = FALSE)
# We can see that children do differ in their linear growth

# We can compute the 95% PI:
VarCorr(m1b)
0.77 + c(-1, 1) * 1.96 * 0.12191
# 95% of children improve year to year by between 0.52 to 1.01 points.


# Q: Interpret the correlation between the student-level random effects.



# Now let's add the random slope for schools:
m1c <- lmer(math ~ grade + (grade | childid:schoolid) + (grade | schoolid),
            data = egsingle)

VarCorr(m1c)
# We can see that the child-level variance in the growth slope has reduced:
1 - (0.063093 / 0.12191)^2
# 73% of the variance between how children differ in their growth can be
# attributed to different schools showing different growth.

# Here to we can compute 95% PIs between the schools:
0.77 + c(-1, 1) * 1.96 * 0.114114
# 95% of schools show a growth between 0.55 and 0.99 from year to year.


# Q: Interpret the correlation between the school-level random effects.




# Conditional growth model -----------------------------------------------

# Let's examine the interacting effect of percentage of low-income students in
# the school on the linear growth.

ggplot(egsingle, aes(grade, math)) +
  geom_smooth(aes(group = schoolid, color = lowinc), 
              method = "lm", se = FALSE,
              alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Trend per school")
# From the plot, we can see that schools with a lower income student body
# have lower math grades. But are the slopes also different?


m2 <- lmer(math ~ grade * lowinc + (grade | childid:schoolid) + (grade | schoolid),
           data = egsingle)
anova(m2, m1c)
# The models are different - but this is the main effect and the interaction!

model_parameters(m2, effects = "fixed", ci_method = "S")
# - The simple effect of grade (for school with no lower income students) is 0.88.
# - The simple effect of lowinc (at grade=0) is -0.01 -> the more lower income
#   the student body is, the lower the math grades at grade 0 are.
# - The interaction term is significant and negative.

# We can probe the interaction:
emtrends(m2, ~ lowinc, var = "grade",
         at = list(lowinc = c(0, 50, 100)),
         infer = TRUE)
# We can see that across different %s of lower-income students, the growth model
# is positive and of very similar magnitude.



# Since the interaction is a cross level interaction (levels 1 and 3) we can see
# how much of the differences between the schools in their linear growth is
# explained by lowinc (pseudo-R2).
VarCorr(m1c)
VarCorr(m2)
1 - (0.108619 / 0.114114)^2
# 9% of the variance is explained by lowinc.


# Exercise ----------------------------------------------------------------

# We also have the variable of "sex".
# - Add it as a fixed effect and compute the relevant pseudo-R2.
# - Can we add it as a random effect? Compute the change in variance.

