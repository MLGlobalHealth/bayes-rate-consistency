# Stan models

This directory contains all of the Stan code we use to run simulation experiments, cross-sectional analyses, and longitudinal analyses.

| Naming            | Description                                                                       |
|------------------|-----------------------------------------------------|
| gp-functions.stan | Contains general helper functions used in all GP models                           |
| 2dgp-             | Full-rank two-dimensional Gaussian process models                                 |
| hsgp-             | Hilbert space approximate Gaussian process models                                 |
| eq-               | Models that use the exponential quadratic (squared exponential) covariance kernel |
| m32-              | Models that use the Matern 3/2 covariance kernel                                  |
| m52-              | Models that use the Matern 5/2 covariance kernel                                  |
| -cd               | Classical parameterisation (age-age) of the contact rate matrices                 |
| -rd               | Restructured parameterisation (difference-in-age) of the contact rate matrices    |
| -lrd              | Longitudinal models in the restructure parameterisation                           |
