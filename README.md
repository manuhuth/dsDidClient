<!-- badges: start -->
  [![R-CMD-check](https://github.com/manuhuth/dsDidClient/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/manuhuth/dsDidClient/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/manuhuth/dsDidClient/branch/main/graph/badge.svg)](https://app.codecov.io/gh/manuhuth/dsDidClient?branch=main)
[![R-CMD-check](https://github.com/manuhuth/dsDidClient/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/manuhuth/dsDidClient/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Federated Difference-in-Differences with multiple time periods
We propose dsDid a federated learning package with a federated version of the DID approach of [Callaway and Sant'Anna](https://www.sciencedirect.com/science/article/abs/pii/S0304407620303948) at its core. It allows for the federated estimation of treatment effects per period and the corresponding federated uncertainty quantification. Given convergence of the generalized linear models used to construct the treatment effects of the DID estimate, we are able to obtain exact federated treatment effects and asymptotic standard errors, as well as distributional equivalent bootstrapped standard errors.

## Federated Learning
Federated Learning enables collaboration among multiple data owners who only share summary statistics, resulting in joint model training with larger sample sizes as for individual local training, all while preserving indiviudal data privacy. The increased sample sizes lead to stronger statistical power and therefore to a more rigorous falsification of statistical hypotheses. Federated Learning can produce parameter estimates with convergence properties identical to pooled estimates or even the same parameter estimates.

## Implementation in RDataSHIELD
[RDataSHIELD](https://www.datashield.org/) provides a Federated Learning platform that addresses the most fundamental challenges in facilitating the access of researchers and other healthcare professionals to individual-level data. Although initially developed for work in the biomedical and social sciences, DataSHIELD can be used in any setting where microdata (data on individual subjects) must be analysed but cannot physically be shared with the research users.

## Ensuring data privacy
dsDid incorporates the standard RDataSHIELD security measures such as validity checks on the minimum non-zero counts of observational units for e.g. mean calculations, and the maximum number of parameters in a regression. Additionally, it includes further security measures tailored to the needs of the difference-in-differences approach like random shuffling of rows, immediate processing of data that is send from the client to the server in order to prevent attacks as described in [Huth et al. (2022)](https://www.biorxiv.org/content/10.1101/2022.10.09.511497v1), and allowing only single numbers to be send to the servers. Hence, we do not recommend to use this package ds.cbind or ds.rbind from the [ds.Base](https://github.com/datashield/dsBaseClient) package.


