library(nlme)
library(glmmTMB)
library(ggplot2)

data <- read.csv("Hoffman_Chap.6-Quadratic growth model.csv")
head(data)

data |> 
  subset(PersonID %in% sample(PersonID, 16)) |> 
  ggplot(aes(session, rt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ PersonID, scales = "free") +
  theme_void()
# Can you see the auto-correlation?


# Standard model ---------------------------

m1a <- lme(rt ~ session, data = data,
           random = ~ session | PersonID)
summary(m1a)

# Or:
m1b <- glmmTMB(rt ~ session + (session | PersonID), 
               data = data)
summary(m1b)


# Autocorrelation ---------------------------

m2a <- lme(rt ~ session, data = data,
           random = ~ session | PersonID,
           correlation = corAR1(form = ~ session | PersonID))
summary(m2a)
anova(m2a, m1a)

# AR1 parameter:
m2a[["modelStruct"]][["corStruct"]]
# Positive := residuals tend to be similar session-to-session.
# (See if you can see it in the plot)


# OR
# https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
m2b <- glmmTMB(rt ~ session + (session | PersonID) +
                 ar1(0 + factor(session) | PersonID),
               data = data)
summary(m2b)
VarCorr(m2b)
