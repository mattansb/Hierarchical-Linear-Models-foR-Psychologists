library(ggplot2)

library(nlme)
library(glmmTMB)
library(brms) # Bayesian

library(parameters)
# remotes::install_github('m-clark/mixedup')

# Data ----------------------------------------------------------------

temp <- tempfile()
download.file(
  "https://www.pilesofvariance.com/Chapter6/SPSS/SPSS_Chapter6.zip",
  temp
)
dataset <- haven::read_sav(unz(temp, "SPSS_Chapter6/SPSS_Chapter6.sav"))
unlink(temp)


dataset$PersonID <- factor(dataset$PersonID)
head(dataset)


set.seed(42)
dataset |>
  subset(PersonID %in% sample(levels(PersonID), 16)) |>
  ggplot(aes(session, rt)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~PersonID, scales = "free") +
  theme_classic()
# Can you spot the auto-correlation? Is it positive or negative?

# Standard model --------------------------------------------------

mod_sssn.lme <- lme(
  rt ~ session,
  random = ~ session | PersonID,
  method = "REML",
  data = dataset
)

# Or:
mod_sssn.gTMB <- glmmTMB(
  rt ~ session + (session | PersonID),
  REML = TRUE,
  data = dataset
)

# Or:
mod_sssn.brm <- brm(rt ~ session + (session | PersonID), data = dataset)
# Using default priors - probably a bad idea...
# (More on Bayesian modelling below.)

# AR1 Autocorrelation ---------------------------------------------------

mod_sssn.ar1.lme <- lme(
  rt ~ session,
  random = ~ session | PersonID,
  correlation = corAR1(form = ~ session | PersonID),
  method = "REML",
  data = dataset
)
anova(mod_sssn.ar1.lme, mod_sssn.lme)

model_parameters(mod_sssn.ar1.lme)
mixedup::extract_cor_structure(mod_sssn.ar1.lme) # AR1 parameter
# Positive := residuals tend to be similar session-to-session.
# (See if you can see it in the plot)

# OR
# https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
mod_sssn.ar1.gTMB <- glmmTMB(
  rt ~ session + (session | PersonID) + ar1(0 + factor(session) | PersonID),
  dispformula = ~0,
  REML = TRUE,
  control = glmmTMBControl(optimizer = optim),
  data = dataset
)
anova(mod_sssn.ar1.gTMB, mod_sssn.gTMB) # doens't work because the model didn't converge

model_parameters(mod_sssn.ar1.gTMB)
mixedup::extract_cor_structure(mod_sssn.ar1.gTMB, which_cor = "ar1") # AR1 parameter


# OR...
# https://paulbuerkner.com/brms/reference/autocor-terms.html
mod_sssn.ar1.brm <- brm(
  rt ~ session + (session | PersonID) + ar(session, gr = PersonID),
  data = dataset
)
# Again using default priors - probably a bad idea...
loo::loo_compare(loo::loo(mod_sssn.ar1.brm), loo::loo(mod_sssn.brm)) |>
  print(simplify = FALSE)

mod_sssn.ar1.brm
posterior::as_draws_rvars(mod_sssn.ar1.brm)$ar |>
  bayestestR::describe_posterior(test = NULL)

# We can see here that Bayesian modelling gives us a slightly different (weaker)
# estimate of the AR1 parameter, and that the model _without_ the AR1 parameter
# is preferred.
# This is a very rough example - Bayesian modelling requires careful thought and
# planning. But we can see that the ideas, in the context of multilevel
# modeling, are the same.
