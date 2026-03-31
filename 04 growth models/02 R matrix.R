library(ggplot2)

# pak::pak("lme4/lme4") # requires dev version of lme4
library(lme4)

library(parameters)

# Data ----------------------------------------------------------------

temp <- tempfile()
download.file(
  "https://www.pilesofvariance.com/Chapter6/SPSS/SPSS_Chapter6.zip",
  temp
)
dataset <- haven::read_sav(unz(temp, "SPSS_Chapter6/SPSS_Chapter6.sav"))
unlink(temp)


dataset$PersonID <- factor(dataset$PersonID)
head(dataset, n = 6)


set.seed(20260203)
dataset |>
  # Sample 16 random participants to make the plot more readable:
  subset(PersonID %in% sample(levels(PersonID), 16)) |>
  ggplot(aes(session, rt)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(facets = vars(PersonID), scales = "free_y") +
  theme_classic()
# Can you spot the auto-correlation? Is it positive or negative?

# Standard model --------------------------------------------------

mod_sssn <- lmer(
  rt ~ session + (session | PersonID),
  data = dataset
)
# Each participant has their own intercept and slope, but the residuals are
# assumed to be independent. Is this a good assumption? Probably not, given the
# plot above.

# AR1 Autocorrelation ---------------------------------------------------

# https://cran.r-project.org/web/packages/lme4/vignettes/covariance_structures.html
mod_sssn.ar1 <- lmer(
  rt ~ session +
    (session | PersonID) +
    # We ADD an AR1 correlation structure to the residuals.
    ar1(0 + factor(session) | PersonID, hom = TRUE),
  data = dataset
)
# > (0 +              We don't want to estimate another intercept
# >  factor(session), Time is discrete
# >  hom = TRUE)      We assume the variance is equal across sessions

anova(mod_sssn.ar1, mod_sssn, refit = FALSE)
# The AR1 model is a better fit to the data!

# Let's look at the AR1 parameter:
VarCorr(mod_sssn.ar1)
# Positive := residuals tend to be similar session-to-session.
# (See if you can see it in the plot)

# Other packages ----------------------------------------------------------

# --- nlme ---
# This is an older package, but it has a lot of functionality for modelling
# different types of autocorrelation.
nlme::lme(
  rt ~ session,
  random = ~ session | PersonID,
  correlation = nlme::corAR1(form = ~ session | PersonID),
  data = dataset
)

# --- glmmTMB ---
# This is a newer package, which has a lot of functionality for modelling
# different types of autocorrelation, and many many types of generalized models
# (more on those in a later lesson). My experience is that it is more difficult
# to get models to converge than with lme4, but it is also more flexible in the
# types of models you can fit.
# https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
glmmTMB::glmmTMB(
  rt ~ session + (session | PersonID) + ar1(0 + factor(session) | PersonID),
  dispformula = ~0,
  data = dataset,
  REML = TRUE,
  control = glmmTMB::glmmTMBControl(optimizer = optim)
)
# (In this example the model doesn't converge...)

# --- brms ---
# This is currently the strongest modeling package in R with the most features,
# and so it is unsurprising that it is a Bayesian modelling package. It is a
# wrapper around the Bayesian modelling language Stan.
# https://paulbuerkner.com/brms/reference/autocor-terms.html
brms::brm(
  rt ~ session + (session | PersonID) + ar(session, gr = PersonID),
  data = dataset
)
# Using default priors - probably a bad idea... Bayesian modelling requires
# careful thought and planning. But the ideas in the context of multilevel
# modeling, are the same.
