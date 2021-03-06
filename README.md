# COVID-19 length of hospital stay
This repository contains the data and code used to produce the results presented in "COVID-19 length of hospital stay: a systematic review and data synthesis". 
A pre-print of the manuscript is available here: https://www.medrxiv.org/content/10.1101/2020.04.30.20084780v1

The main analysis dataset can be found in `data/LOS analysis dataset.csv`. This contains all LoS estimates extracted from the included studies, along with data describing the study and the patient population.

These data are summarised in `code/los_summary.R` to produce the figures included in the manuscript.

The extracted estimates are combined into summary distributions using the script `code/comb_dists.R`. A function to sample from the obtained summary distributions for `setting = China/Other` and `type = general/ICU` can be found in `code/los_create_distributions.R`. This function returns a set of `sample_size` samples from the specified summary distribution, the fitted Weibull parameters of each individual study distribution and the errors between the observed and fitted quantiles.

### Contributors
@esnightingale, @NaomiWaterlow, @erees and @samclifford contributed to this repository.

