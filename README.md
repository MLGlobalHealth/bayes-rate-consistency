# bayes-rate-consistency
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/) [![arXiv](https://img.shields.io/badge/arXiv-2210.11358-b31b1b.svg)](https://arxiv.org/abs/2210.11358)

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

#### For Imperial HPC users
For those using Imperial's High Performance Computing Clusters, begin by following the instructions in the [Conda application guide](https://wiki.imperial.ac.uk/display/HPC/Conda) to setup Conda for your HPC account. Assuming you have done this, begin by creating an environment into which we will install the required R and dependencies.
```shell
module load anaconda3/personal
conda create -n bayes-rate-consistency r-base=4.1.3 -c conda-forge
source activate bayes-rate-consistency
```
Navigate to the root of the directory and execute `install-dependencies-hpc.R`
```shell
Rscript install-dependencies-hpc.R
```
This will install the required R libraries recorded in the `renv.lock` file and also install `cmdstanr` from source.

#### From the command line
To install the required R libraries using the command line interface, navigate to the repository and run the following command
```shell
Rscript install-dependencies.R
```

#### From RStudio
Navigate to the repository within [RStudio](https://posit.co/downloads/) and click on the `covimod-gp.Rproj` to activate the project from the Files window.

Then, execute the following lines of code using the R console to install the required dependencies
```r
renv::init()
renv::restore()
```
You may need to manually install some packages such as `cmdstanr`. If that is the case, please refer to: [Getting started with CmdStanR](https://mc-stan.org/cmdstanr/articles/cmdstanr.html).

## Running the models
At the moment, we are unable to provide COVIMOD or POLYMOD data due to data agreement terms. However, the de-identified data is scheduled to be released in the near future and this repository will be updated accordingly. However, you can find a tutorial on how to run our simulation experiments in: `tutorials/running-simulations.Rmd`.
