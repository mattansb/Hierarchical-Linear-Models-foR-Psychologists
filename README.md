

# Hierarchical Linear Models foR Psychologists

[![](https://img.shields.io/badge/Open%20Educational%20Resources-Compatable-brightgreen.png)](https://creativecommons.org/about/program-areas/education-oer/)
[![](https://img.shields.io/badge/CC-BY--NC%204.0-lightgray)](http://creativecommons.org/licenses/by-nc/4.0/)
[![](https://img.shields.io/badge/Language-R-blue.png)](http://cran.r-project.org/)

<sub>*Last updated 2026-02-02.*</sub>

This Github repo contains all lesson files for *Hierarchical Linear
Models in R*. The goal is to impart students with the basic tools to
construct, evaluate and compare various **(generalized) linear mixed
models, using
[`lme4`](https://cran.r-project.org/web/packages/lme4/index.html/)**,
based on Lesa Hoffman’s [*Longitudinal Analysis: Modeling Within-Person
Fluctuation and Change*](https://www.pilesofvariance.com/index.html).

These topics were taught in the graduate-level course ***Hierarchical
Linear Models for Psychologists*** (Psych Dep., Ben-Gurion University of
the Negev; Psych Dep., Tel-Aviv University). This course assumes basic
competence in R (importing, regression modeling, plotting, etc.), along
the lines of [*Practical Applications in R for
Psychologists*](https://github.com/mattansb/Practical-Applications-in-R-for-Psychologists).

**Notes:**

- This repo contains only materials relating to *Practical Applications
  in R*, and does not contain any theoretical or introductory materials.
- Please note that some code does not work *on purpose*, to force
  students to learn to debug.

## Setup

You will need:

1.  A fresh installation of [**`R`**](https://cran.r-project.org/)
    (preferably version 4.5.0 or above).
2.  [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) or
    [Positron](https://positron.posit.co/download) (optional, but
    recommended).
3.  The following packages, listed by lesson:

| Lesson | Packages |
|:---|:---|
| [01 HLM basics](/01%20HLM%20basics) | `tidyverse`, `lmerTest`, `performance`, `parameters`, `merDeriv`, `emmeans`, `haven`, `sjPlot`, `afex` |
| [02 estimation and inference](/02%20estimation%20and%20inference) | `tidyverse`, `lmerTest`, `performance`, `parameters`, `haven`, `scales`, `glue`, `sjPlot`, `bayestestR` |
| [03 cross-level interactions and effect sizes](/03%20cross-level%20interactions%20and%20effect%20sizes) | `tidyverse`, `datawizard`, `lmerTest`, `performance`, `parameters`, `marginaleffects`, `scales` |
| [04 growth models](/04%20growth%20models) | `dplyr`, `ggplot2`, `lmerTest`, `parameters`, `performance`, `haven`, `scales`, `glue`, `marginaleffects`, `nlme`, `glmmTMB`, `brms`, `remotes`, `mixedup`, `loo`, `posterior`, `bayestestR` |
| [05 within-person fluctuation models](/05%20within-person%20fluctuation%20models) | `tidyverse`, `datawizard`, `lmerTest`, `performance`, `parameters`, `haven` |
| [06 GLMMs](/06%20GLMMs) | `tidyverse`, `lme4`, `performance`, `parameters`, `marginaleffects`, `haven`, `insight`, `scales` |
| [07 multilpe random factors](/07%20multilpe%20random%20factors) | `dplyr`, `ggplot2`, `patchwork`, `lmerTest`, `performance`, `parameters`, `marginaleffects`, `mlmRev`, `forcats` |
| [08 ANOVA](/08%20ANOVA) | `dplyr`, `ggplot2`, `lmerTest`, `datawizard`, `emmeans`, `afex`, `car`, `effectsize`, `patchwork`, `performance`, `statmod`, `see`, `brms` |
| [09 assumptions](/09%20assumptions) | `tidyverse`, `lmerTest`, `performance`, `DHARMa`, `scales` |

<details>

<summary>

<i>Installing R Packages</i>
</summary>

You can install all the R packages used by running:

    # in alphabetical order:

    pak::pak(
      c(

        "cran::DHARMa", # 0.4.7
        "cran::afex", # 1.5-1
        "bayestestR", # 0.17.0.2
        "cran::brms", # 2.23.0
        "cran::car", # 3.1-3
        "datawizard", # 1.3.0
        "cran::dplyr", # 1.1.4
        "effectsize", # 1.0.1.2
        "cran::emmeans", # 2.0.1
        "cran::forcats", # 1.0.1
        "cran::ggplot2", # 4.0.1
        "cran::glmmTMB", # 1.1.14
        "cran::glue", # 1.8.0
        "cran::haven", # 2.5.5
        "insight", # 1.4.4.3
        "cran::lme4", # 1.1-38
        "cran::lmerTest", # 3.2-0
        "cran::loo", # 2.9.0
        "cran::marginaleffects", # 0.31.0
        "cran::merDeriv", # 0.2-5
        "github::m-clark/mixedup", # 0.4.0
        "cran::mlmRev", # 1.0-8
        "cran::nlme", # 3.1-168
        "parameters", # 0.28.3
        "cran::patchwork", # 1.3.2
        "cran::performance", # 0.15.3
        "cran::posterior", # 1.6.1
        "cran::remotes", # 2.5.0
        "cran::scales", # 1.4.0
        "cran::see", # 0.12.0
        "cran::sjPlot", # 2.9.0
        "cran::statmod", # 1.5.1
        "cran::tidyverse" # 2.0.0

      )
    )

</details>

------------------------------------------------------------------------

### Acknowledgements

Materials developed with Yael Bar-Shachar.
