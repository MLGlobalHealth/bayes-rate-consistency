# Bayesian Rate Consistency Model

### for estimating high resolution human social contact patterns

## About this repository

This repository contains the code for the paper: *Estimating fine age structure and time trends in human contact patterns from coarse contact data: the Bayesian rate consistency model.* <https://arxiv.org/abs/2210.11358>

### Contents

-   `notebooks`: Contains R Markdown documents for pre-processing COVIMOD and POLYMOD code
-   `scripts`: Contains the R code that is used the generate simulated data, run Stan models, and post-process the models
-   `R`: Helper functions for pre-processing, post-processing, Stan, and others
-   `stan_models`: Stan model files
-   `paper`: R scripts used to generate tables and figures used in the paper.

Please report any bugs and issues to: [shozen.dan\@gmail.com](mailto:shozen.dan@gmail.com){.email} or directly post a issue on GitHub.

## Installation

Clone the repository to your chosen directory on your local machine.

``` bash
git clone git@github.com:MLGlobalHealth/bayes-rate-consistency.git
```

Navigate to the repository within [RStudio](https://posit.co/downloads/) and click on the `covimod-gp.Rproj` to activate the project or run the following line of code with R. You will need to specify the path to the cloned repository.

```{r}
renv::activate("path/to/repository")
```

To install all required libraries, execute

```{r}
renv::init
renv::restore()
```

You may need to manually install some packages such as `cmdstanr`. If that is the case, please refer to: [Getting started with CmdStanR](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

## Running the models

At the moment, we are unable to provide COVIMOD or POLYMOD data due to data agreement terms. However, the de-identified data is scheduled to be released in the near future and this repository will be updated accordingly. However, you can find a tutorial on how to run our simulation experiments in: `tutorials/running-simulations.Rmd`.
