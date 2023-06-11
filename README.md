
# Hierarchical Linear Models foR Psychologists

[![](https://img.shields.io/badge/Open%20Educational%20Resources-Compatable-brightgreen)](https://creativecommons.org/about/program-areas/education-oer/)
[![](https://img.shields.io/badge/CC-BY--NC%204.0-lightgray)](http://creativecommons.org/licenses/by-nc/4.0/)  
[![](https://img.shields.io/badge/Language-R-blue)](http://cran.r-project.org/)

<sub>*Last updated 2023-06-11.*</sub>

This Github repo contains all lesson files for *Hierarchical Linear
Models in R*. The goal is to impart students with the basic tools to
construct, evaluate and compare various **(generalized) linear mixed
models, using
[`lme4`](https://cran.r-project.org/web/packages/lme4/index.html/)**.
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
    (preferably version 4.2 or above).
2.  [RStudio IDE](https://www.rstudio.com/products/rstudio/download/)
    (optional, but recommended).
3.  The following packages, listed by lesson:

| Lesson                                                                                                    | Packages                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
|-----------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| [01 HLM Basics](/01%20HLM%20Basics)                                                                       | [`tidyverse`](https://CRAN.R-project.org/package=tidyverse), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`emmeans`](https://CRAN.R-project.org/package=emmeans), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`merDeriv`](https://CRAN.R-project.org/package=merDeriv), [`sjPlot`](https://CRAN.R-project.org/package=sjPlot), [`afex`](https://CRAN.R-project.org/package=afex)                                                                                                                                                                                                                                       |
| [02 Model Comparisons](/02%20Model%20Comparisons)                                                         | [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`sjPlot`](https://CRAN.R-project.org/package=sjPlot), [`bayestestR`](https://CRAN.R-project.org/package=bayestestR)                                                                                                                                                                                                                                                                                                                                                   |
| [03 Cross level interactions and effect sizes](/03%20Cross%20level%20interactions%20and%20effect%20sizes) | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`datawizard`](https://CRAN.R-project.org/package=datawizard), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`emmeans`](https://CRAN.R-project.org/package=emmeans), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2)                                                                                                                                                                                                                                                                                            |
| [04 Growth Models](/04%20Growth%20Models)                                                                 | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`parameters`](https://CRAN.R-project.org/package=parameters), [`performance`](https://CRAN.R-project.org/package=performance), [`emmeans`](https://CRAN.R-project.org/package=emmeans), [`ggeffects`](https://CRAN.R-project.org/package=ggeffects), [`nlme`](https://CRAN.R-project.org/package=nlme), [`glmmTMB`](https://CRAN.R-project.org/package=glmmTMB)                                                                                                                                                                                  |
| [05 Within-Person Fluctuation Models](/05%20Within-Person%20Fluctuation%20Models)                         | [`tidyverse`](https://CRAN.R-project.org/package=tidyverse), [`datawizard`](https://CRAN.R-project.org/package=datawizard), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters)                                                                                                                                                                                                                                                                                                                                                                                                      |
| [06 GLMMs](/06%20GLMMs)                                                                                   | [`tidyverse`](https://CRAN.R-project.org/package=tidyverse), [`lme4`](https://CRAN.R-project.org/package=lme4), [`performance`](https://CRAN.R-project.org/package=performance), [`parameters`](https://CRAN.R-project.org/package=parameters), [`haven`](https://CRAN.R-project.org/package=haven), [`insight`](https://CRAN.R-project.org/package=insight), [`ggeffects`](https://CRAN.R-project.org/package=ggeffects)                                                                                                                                                                                                                                                                                                  |
| [07 Multilpe random factors](/07%20Multilpe%20random%20factors)                                           | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`tibble`](https://CRAN.R-project.org/package=tibble), [`forcats`](https://CRAN.R-project.org/package=forcats), [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`lmerTest`](https://CRAN.R-project.org/package=lmerTest), [`performance`](https://CRAN.R-project.org/package=performance), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`parameters`](https://CRAN.R-project.org/package=parameters), [`emmeans`](https://CRAN.R-project.org/package=emmeans) |
| [08 Misc](/08%20Misc)                                                                                     | [`dplyr`](https://CRAN.R-project.org/package=dplyr), [`lme4`](https://CRAN.R-project.org/package=lme4), [`performance`](https://CRAN.R-project.org/package=performance), [`see`](https://CRAN.R-project.org/package=see), [`ggplot2`](https://CRAN.R-project.org/package=ggplot2), [`patchwork`](https://CRAN.R-project.org/package=patchwork), [`afex`](https://CRAN.R-project.org/package=afex), [`emmeans`](https://CRAN.R-project.org/package=emmeans), [`car`](https://CRAN.R-project.org/package=car)                                                                                                                                                                                                                |

You can install all the packages used by running:

    # in alphabetical order:

    pkgs <- c(
      "afex", "bayestestR", "car", "datawizard", "dplyr", "emmeans",
      "forcats", "ggeffects", "ggplot2", "glmmTMB", "haven", "insight",
      "lme4", "lmerTest", "merDeriv", "nlme", "parameters", "patchwork",
      "performance", "see", "sjPlot", "tibble", "tidyverse"
    )

    install.packages(pkgs, dependencies = TRUE)

<details>
<summary>
<i>Package Versions</i>
</summary>

The package versions used here:

- `afex` 1.3-0 (*CRAN*)
- `bayestestR` 0.13.1 (*CRAN*)
- `car` 3.1-2 (*CRAN*)
- `datawizard` 0.7.1 (*CRAN*)
- `dplyr` 1.1.1 (*CRAN*)
- `emmeans` 1.8.6 (*CRAN*)
- `forcats` 1.0.0 (*CRAN*)
- `ggeffects` 1.2.1.9 (*Github: strengejacke/ggeffects*)
- `ggplot2` 3.4.2 (*CRAN*)
- `glmmTMB` 1.1.7 (*CRAN*)
- `haven` 2.5.2 (*CRAN*)
- `insight` 0.19.1 (*CRAN*)
- `lme4` 1.1-33 (*CRAN*)
- `lmerTest` 3.1-3 (*CRAN*)
- `merDeriv` 0.2-4 (*CRAN*)
- `nlme` 3.1-160 (*CRAN*)
- `parameters` 0.21.0 (*CRAN*)
- `patchwork` 1.1.2 (*CRAN*)
- `performance` 0.10.3 (*CRAN*)
- `see` 0.7.5 (*CRAN*)
- `sjPlot` 2.8.14 (*CRAN*)
- `tibble` 3.2.1 (*CRAN*)
- `tidyverse` 2.0.0 (*CRAN*)

</details>
