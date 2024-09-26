

# Hierarchical Linear Models foR Psychologists

[![](https://img.shields.io/badge/Open%20Educational%20Resources-Compatable-brightgreen.png)](https://creativecommons.org/about/program-areas/education-oer/)
[![](https://img.shields.io/badge/CC-BY--NC%204.0-lightgray)](http://creativecommons.org/licenses/by-nc/4.0/)  
[![](https://img.shields.io/badge/Language-R-blue.png)](http://cran.r-project.org/)

<sub>*Last updated 2024-09-26.*</sub>

This Github repo contains all lesson files for *Hierarchical Linear
Models in R*. The goal is to impart students with the basic tools to
construct, evaluate and compare various **(generalized) linear mixed
models, using
[`lme4`](https://cran.r-project.org/web/packages/lme4/index.html/)**,
based on Lesa Hoffman’s [*Longitudinal Analysis: Modeling Within-Person
Fluctuation and Change*](https://www.pilesofvariance.com/index.html).
(Materials developed with Yael Bar-Shachar.)

These topics were taught in the graduate-level course ***Hierarchical
Linear Models for Psychologists*** (Psych Dep., Ben-Gurion University of
the Negev; Psych Dep., Tel-Aviv University). This course assumes basic
competence in R (importing, regression modeling, plotting, etc.), along
the lines of [*Practical Applications in R for
Psychologists*](https://github.com/mattansb/Practical-Applications-in-R-for-Psychologists).

**Notes:**

- This repo contains only materials relating to *Practical Applications
  in R*, and does not contain any theoretical or introductory
  materials.  
- Please note that some code does not work *on purpose*, to force
  students to learn to debug.

## Setup

You will need:

1.  A fresh installation of [**`R`**](https://cran.r-project.org/)
    (preferably version 4.3.2 or above).
2.  [RStudio IDE](https://www.rstudio.com/products/rstudio/download/)
    (optional, but recommended).
3.  The following packages, listed by lesson:

| Lesson | Packages |
|----|----|
| [01 HLM Basics](.\01%20HLM%20Basics) | [`tidyverse`](https://CRAN.R-project.org/package=tidyverse), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`emmeans`](https://CRAN.R-project.org/package=emmeans), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`merDeriv`](https://CRAN.R-project.org/package=merDeriv), [`haven`](https://CRAN.R-project.org/package=haven), [`sjPlot`](https://CRAN.R-project.org/package=sjPlot), [`afex`](https://CRAN.R-project.org/package=afex) |
| [02 Model Comparisons](.\02%20Model%20Comparisons) | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`haven`](https://CRAN.R-project.org/package=haven), [`scales`](https://CRAN.R-project.org/package=scales), [`glue`](https://CRAN.R-project.org/package=glue), [`sjPlot`](https://CRAN.R-project.org/package=sjPlot), [`bayestestR`](https://CRAN.R-project.org/package=bayestestR) |
| [03 Cross level interactions and effect sizes](.\03%20Cross%20level%20interactions%20and%20effect%20sizes) | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`datawizard`](https://CRAN.R-project.org/package=datawizard), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`marginaleffects`](https://CRAN.R-project.org/package=marginaleffects), [`scales`](https://CRAN.R-project.org/package=scales) |
| [04 Growth Models](.\04%20Growth%20Models) | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`parameters`](https://CRAN.R-project.org/package=parameters), [`performance`](https://CRAN.R-project.org/package=performance), [`marginaleffects`](https://CRAN.R-project.org/package=marginaleffects), [`haven`](https://CRAN.R-project.org/package=haven), [`scales`](https://CRAN.R-project.org/package=scales), [`glue`](https://CRAN.R-project.org/package=glue), [`nlme`](https://CRAN.R-project.org/package=nlme), [`glmmTMB`](https://CRAN.R-project.org/package=glmmTMB), [`brms`](https://CRAN.R-project.org/package=brms), [`remotes`](https://CRAN.R-project.org/package=remotes), [`mixedup`](https://CRAN.R-project.org/package=mixedup), [`loo`](https://CRAN.R-project.org/package=loo), [`posterior`](https://CRAN.R-project.org/package=posterior), [`bayestestR`](https://CRAN.R-project.org/package=bayestestR) |
| [05 Within-Person Fluctuation Models](.\05%20Within-Person%20Fluctuation%20Models) | [`tidyverse`](https://CRAN.R-project.org/package=tidyverse), [`datawizard`](https://CRAN.R-project.org/package=datawizard), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`haven`](https://CRAN.R-project.org/package=haven) |
| [06 GLMMs](.\06%20GLMMs) | [`tidyverse`](https://CRAN.R-project.org/package=tidyverse), [`lme4`](https://CRAN.R-project.org/package=lme4), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`marginaleffects`](https://CRAN.R-project.org/package=marginaleffects), [`haven`](https://CRAN.R-project.org/package=haven), [`insight`](https://CRAN.R-project.org/package=insight), [`scales`](https://CRAN.R-project.org/package=scales) |
| [07 Multilpe random factors](.\07%20Multilpe%20random%20factors) | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`patchwork`](https://CRAN.R-project.org/package=patchwork), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`marginaleffects`](https://CRAN.R-project.org/package=marginaleffects), [`mlmRev`](https://CRAN.R-project.org/package=mlmRev), [`forcats`](https://CRAN.R-project.org/package=forcats) |
| [08 ANOVA](.\08%20ANOVA) | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`datawizard`](https://CRAN.R-project.org/package=datawizard), [`emmeans`](https://CRAN.R-project.org/package=emmeans), [`afex`](https://CRAN.R-project.org/package=afex), [`car`](https://CRAN.R-project.org/package=car), [`effectsize`](https://CRAN.R-project.org/package=effectsize), [`patchwork`](https://CRAN.R-project.org/package=patchwork), [`performance`](https://CRAN.R-project.org/package=performance), [`statmod`](https://CRAN.R-project.org/package=statmod), [`see`](https://CRAN.R-project.org/package=see) |
| [09 Assumptions](.\09%20Assumptions) | [`tidyverse`](https://CRAN.R-project.org/package=tidyverse), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`DHARMa`](https://CRAN.R-project.org/package=DHARMa), [`scales`](https://CRAN.R-project.org/package=scales) |

You can install all the packages used by running:

    # in alphabetical order:

    pkgs <- c(
      "afex", "bayestestR", "brms", "car", "datawizard", "DHARMa",
      "dplyr", "effectsize", "emmeans", "forcats", "ggplot2", "glmmTMB",
      "glue", "haven", "insight", "lme4", "lmerTest", "loo", "marginaleffects",
      "merDeriv", "mixedup", "mlmRev", "nlme", "parameters", "patchwork",
      "performance", "posterior", "remotes", "scales", "see", "sjPlot",
      "statmod", "tidyverse"
    )

    install.packages(pkgs, dependencies = TRUE)

<details>
<summary>
<i>Package Versions</i>
</summary>

The package versions used here:

- `afex` 1.4-1 (*CRAN*)
- `bayestestR` 0.14.0 (*CRAN*)
- `brms` 2.21.0 (*CRAN*)
- `car` 3.1-2 (*CRAN*)
- `datawizard` 0.12.3 (*CRAN*)
- `DHARMa` 0.4.6 (*CRAN*)
- `dplyr` 1.1.4 (*CRAN*)
- `effectsize` 0.8.9 (*CRAN*)
- `emmeans` 1.10.4 (*CRAN*)
- `forcats` 1.0.0 (*CRAN*)
- `ggplot2` 3.5.1 (*CRAN*)
- `glmmTMB` 1.1.9 (*CRAN*)
- `glue` 1.7.0 (*CRAN*)
- `haven` 2.5.4 (*CRAN*)
- `insight` 0.20.4 (*CRAN*)
- `lme4` 1.1-35.5 (*CRAN*)
- `lmerTest` 3.1-3 (*CRAN*)
- `loo` 2.8.0 (*CRAN*)
- `marginaleffects` 0.22.0 (*CRAN*)
- `merDeriv` 0.2-4 (*CRAN*)
- `mixedup` 0.4.0 (*Github: m-clark/mixedup*)
- `mlmRev` 1.0-8 (*CRAN*)
- `nlme` 3.1-164 (*CRAN*)
- `parameters` 0.22.2 (*CRAN*)
- `patchwork` 1.3.0 (*CRAN*)
- `performance` 0.12.3 (*CRAN*)
- `posterior` 1.6.0 (*CRAN*)
- `remotes` 2.5.0 (*CRAN*)
- `scales` 1.3.0 (*CRAN*)
- `see` 0.9.0 (*CRAN*)
- `sjPlot` 2.8.16 (*CRAN*)
- `statmod` 1.5.0 (*CRAN*)
- `tidyverse` 2.0.0 (*CRAN*)

</details>
