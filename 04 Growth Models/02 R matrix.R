
library(ggplot2)

library(nlme)
library(glmmTMB)
# library(brms) # Bayesian

library(parameters)
# remotes::install_github('m-clark/mixedup')

# Data ----------------------------------------------------------------

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


# Standard model --------------------------------------------------

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


# AR1 Autocorrelation ---------------------------------------------------

m2a <- lme(rt ~ session, 
           random = ~ session | PersonID,
           correlation = corAR1(form = ~ session | PersonID),
           method = "REML",
           data = data)
anova(m2a, m1a)

model_parameters(m2a)
mixedup::extract_cor_structure(m2a) # AR1 parameter
# Positive := residuals tend to be similar session-to-session.
# (See if you can see it in the plot)



# OR
# https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
m2b <- glmmTMB(rt ~ session + (session | PersonID) +
                 ar1(0 + factor(session) | PersonID),
               dispformula = ~ 0, 
               REML = TRUE,
               data = data)
anova(m2b, m1b) # doens't work because the model didn't converge

model_parameters(m2b)
mixedup::extract_cor_structure(m2b, which_cor = "ar1") # AR1 parameter


