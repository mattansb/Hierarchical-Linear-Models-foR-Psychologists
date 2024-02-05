library(ggplot2)

library(nlme)
library(glmmTMB)


data <- read.csv("Hoffman_Chap.6-Quadratic growth model.csv")
data$PersonID <- factor(data$PersonID)
head(data)


set.seed(42)
data |> 
  subset(PersonID %in% sample(levels(PersonID), 16)) |> 
  ggplot(aes(session, rt)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ PersonID, scales = "free") +
  theme_classic()
# Can you see the auto-correlation?


# Standard model ---------------------------

m1a <- lme(rt ~ session,
           random = ~ session | PersonID,
           method = "REML",
           data = data)
summary(m1a)

# Or:
m1b <- glmmTMB(rt ~ session + (session | PersonID),
               REML = TRUE,
               data = data)
summary(m1b)


# Autocorrelation ---------------------------

m2a <- lme(rt ~ session, 
           random = ~ session | PersonID,
           correlation = corAR1(form = ~ session | PersonID),
           method = "REML",
           data = data)
anova(m2a, m1a)

summary(m2a)


# AR1 parameter:
m2a[["modelStruct"]][["corStruct"]]
# Positive := residuals tend to be similar session-to-session.
# (See if you can see it in the plot)


# OR
# https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
m2b <- glmmTMB(rt ~ session + (session | PersonID) +
                 ar1(0 + factor(session) | PersonID),
               dispformula = ~ 0, 
               REML = TRUE,
               data = data)
summary(m2b)
VarCorr(m2b)
