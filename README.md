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

## Quick Start
### Setup
#### For Imperial HPC users
For those using Imperial's High Performance Computing Clusters, begin by following the instructions in the [Conda application guide](https://wiki.imperial.ac.uk/display/HPC/Conda) to setup Conda for your HPC account. Assuming you have done this, begin by creating an environment into which we will install the required R and dependencies.
```bash
module load anaconda3/personal
conda create -n bayes-rate-consistency r-base=4.1.3 -c conda-forge
source activate bayes-rate-consistency
```
Navigate to the root of the directory and execute `install-dependencies-hpc.R`
```bash
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

### Simulations
#### Experiment configurations
To run simulation experiments for various scenarios and versions of the cross-sectional bayes-rate-consistency model, begin by configuring the contents of `settings/simulation.yml`. The contents of the file are as follows:
```yaml
seed: 1234

data:
  size: 2000
  covid: TRUE
  strata: "COVIMOD"

model:
  name: "hsgp-m52-rd"
  hsgp_binf: 1.5
  hsgp_m1: 30
  hsgp_m2: 20
  chains: 4
  warmup: 500
  sampling: 1500
  max_treedepth: 13
```
- **seed**: Random seed to ensure replicability
- **data**:
  - **size**: Number of participants in the contact survey
  - **covid**: pre-COVID19 scenario or in-COVID19 scenario (`TRUE` or `FALSE`)
  - **strata**: Age stratification scheme for contact's age. Available options are `"3yr"`, `"5yr"`, `"COVIMOD"`, `"CoMix"`.
- **model**: 
  - **name**: Name of the model to use. Available options are `"2dgp-eq-cd"`, `"2dgp-m32-cd"`, `"2dgp-m52-cd"`, `"2dgp-eq-rd"`, `"2dgp-m32-rd"`, `"2dgp-m52-rd"`, `"hsgp-eq-cd"`, `"hsgp-m32-cd"`, `"hsgp-m52-cd"`, `"hsgp-eq-rd"`, `"hsgp-m32-rd"`, `"hsgp-m52-rd"`. Please refer to `stan_models/README.md` for specific details about each model.
  - **hsgp_binf**: Boundary inflation factor for Hilbert Space approximate Gaussian Processes. Will be ignored when using model beginning with `"2dgp"`.
  - **hsgp_m1**: Number of basis functions for the first age dimension.  Will be ignored when using model beginning with `"2dgp"`.
  - **hsgp_m2**: Number of basis functions for the second age dimension.
  Will be ignored when using model beginning with `"2dgp"`.
  - **chains**: The number of MCMC chains to run in Stan.
  - **warmup**: The number of MCMC warmup iterations.
  - **sampling**: The number of MCMC sampling iterations.
  - **max_treedepth**: The maximum depth of the binary tree that the No-U-Turn-Sampler (NUTS) builds.

#### For Imperial HPC users
After setting the parameters in `settings/simulation.yml`, set the `REPO_PATH` and `OUT_PATH` variables within `hpc_scripts/simulation-fullproc.sh` such that they are the **absolute paths** to the repository and the directory to house the generated outputs, respectively.
```bash
#!/bin/bash
########## EDIT THIS SECTION ##########
REPO_PATH=/rds/general/user/sd121/home/bayes-rate-consistency
OUT_PATH=/rds/general/user/sd121/home/bayes-rate-consistency-output
#######################################

# Create main script
cat > "$OUT_PATH/simulation-fullproc.pbs" <<EOF
#!/bin/bash

#PBS -l walltime=08:00:00
#PBS -l select=1:ncpus=4:ompthreads=1:mem=50gb
#PBS -J 1-10

module load anaconda3/personal
source activate bayes-rate-consistency

cd "$REPO_PATH"

# Generate simulated datasets
Rscript scripts/sim-dataset.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --pidx "\$PBS_ARRAY_INDEX"

# Run Stan models
Rscript scripts/sim-run-stan.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --pidx "\$PBS_ARRAY_INDEX"

# Postprocessing
Rscript scripts/sim-postprocess.R \
  -i "$REPO_PATH" \
  -o "$OUT_PATH" \
  --pidx "\$PBS_ARRAY_INDEX"
EOF

# Execute main script
cd $OUT_PATH
qsub simulation-fullproc.pbs
```
This script generates a bash script named `simulation-fullproc.pbs` in the specified output directory. This script is then submitted as a job to the HPC.
> :exclamation: By default, the script above will run 10 array jobs which will generate 10 sets of outputs. To prevent this, change `#PBS -J 1-10` to `#PBS -J 1`.

Finally, from the root of the repository execute:
```bash
bash hpc_scripts/simulation-fullproc.sh
```

#### Laptop
TODO

### COVIMOD and other contact studies
At the moment, we are unable to provide COVIMOD or POLYMOD data due to data agreement terms. However, the de-identified data is scheduled to be released in the near future and this repository will be updated accordingly.


