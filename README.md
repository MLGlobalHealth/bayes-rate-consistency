# Estimating fine age structure and time trends in human contact patterns from coarse contact data: the Bayesian rate consistency model

Shozen Dan, Yu Chen, Yining Chen, Mélodie Monod, Veronika K Jaeger, Samir Bhatt, André Karch, Oliver Ratmann. Estimating fine age structure and time trends in human contact patterns from coarse contact data: the Bayesian rate consistency model https://arxiv.org/abs/2210.11358

## Abstract
Since the emergence of severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2),
many contact surveys have been conducted to measure the fundamental changes in
human interactions that occurred in the face of the pandemic and non-pharmaceutical
interventions. These surveys were typically conducted longitudinally, using protocols
that have important differences from those used in the pre-pandemic era. Here, we
present a model-based statistical approach that can reconstruct contact patterns at
1-year resolution even when the age of the contacts is reported coarsely by 5 or 10-year
age bands. This innovation is rooted in population-level consistency constraints in how
contacts between groups must add up, which prompts us to call the approach presented
here the Bayesian rate consistency model. The model also incorporates computationally
efficient Hilbert Space Gaussian process priors to infer the dynamics in age- and
gender-structured social contacts, and is designed to adjust for reporting fatigue
emerging in longitudinal surveys.

We demonstrate on simulations the ability to reconstruct social contact patterns by
gender and 1-year age interval from coarsely reported data with adequate accuracy and
within a fully Bayesian framework to quantify uncertainty. We then investigate the
patterns and dynamics of social contact data collected in Germany from April to June
2020 across five longitudinal survey waves. We reconstruct the fine age structure in
social contacts during the early stages of the pandemic and demonstrate that social
contact intensities rebounded in a highly structured, non-homogeneous manner. We also
show that by July 2020, social contact intensities remained well below pre-pandemic
values despite a considerable easing of non-pharmaceutical interventions.

This model-based inference approach is open access, computationally tractable
enabling full Bayesian uncertainty quantification, and readily applicable to
contemporary survey data as long as the exact age of survey participants is reported.

## About this repository
This repository contains the code for the paper: *Estimating fine age structure and time trends in human contact patterns from coarse contact data: the Bayesian rate consistency model.*

* `notebooks`: Contains R Markdown documents for pre-processing COVIMOD and POLYMOD code
* `scripts`: Contains the R code that is used the generate simulated data, run Stan models, and post-process the models
* `R`: Helper functions for pre-processing, post-processing, Stan, and others
* `stan_models`: Stan model files
* `paper`: R scripts used to generate tables and figures used in the paper.

Please report any bugs and issues to: shozen.dan@gmail.com or directly post a issue on GitHub.

## Installation

