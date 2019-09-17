# dengue-Zika-chik_Americas/Scripts

Please find below descriptions of the scripts used for our analyses. Please note that soome of the scripts will not run because they require the fitted models (which were too large to include here). When possible, we provide Rdata files generated from our model fits in the *Materials* subfolder to assist with this issue and provide the information necessary to generate figures.

## Scripts for data collation

Please see documentation in dengue-Zika-chik_Americas/coreScript.sh

## Scripts for data exploration

"06-getDescriptives" calculates annual incidence and other metrics describing the time series.

"08-Figure1_FigureSI1.R" explores seasonality and generates Fig. 1 and SI Fig. 1

## Scripts for the subnational-level models

"06-generate-incidence-dat.R" generates and saves incidence.dat files required for fitting the subnational-level stan models.

"06-stan-get-NB-models-HPC" generates the subnational-level stan models. 

"06-stan-model-fit-analyses" checks convergence metrics for the subnational-level stan models and provides plots that visualize the coefficients from these models. This file generates SI Figures 2 and 3. 

"06-generate-m-files" evaluates where the observed dengue incidence falls in the prediction intervals from the subnation-level dengue incidence stan models.

"06-generate-m-files.R" generates the colors and alpha values needed to represent the differences between model predictions and observed cases (see Fig. 2, SI Figs. 5,6)

"06-permuation-test-HPC.R" runs permutation tests to evaluate the quantile values reported in Figure 2.

"08-Figure2.R" generates Figure 2 and SI Figures 5 and 6.

## Scripts for the hierarchical models

"06-analyze.R" generates data frame (incidence.dat) needed for the hierarchical models. This data frame is country specific and contains subnational dengue, Zika, chikungunya data. Microcephaly data is also included in the data frame for Brazil. This file also generates the model formulas needed for the hierarchical models.

"06-analyze-random-recent.R" generates the same data structures as "06-analyze.R", but additionally generates analogous data frames with the last three years of data (2015-2017) replaced with three consecutive years of earlier data (e.g. 2002-2005).

"06-analyze-hierarchical.R" generates combined Bayesian credible intervals for the coefficients produced in the stan models, where combined means the sum of the shared effect national-level coefficient and the subnational-level deviation.

"07-compare-hierarchical-results.R" generates SI Figure 9 by sourcing "Scripts/06-analyze-hierarchical.R"

"08-Figure3.R" generates Figure 3.

## Scripts with helper functions

"04-generateBiweeks.R" defines a function convert a date vector to a biweek number.

"04-set_location_order.R" returns subnational-locations ordered by region and then latitude within region (vectors: lat.ordered.Brazil and lat.ordered.CO)

"functions.R" includes a function from EpiWeek package (version 1.1) used to convert epiweeks to dates and convergence check and warning functions from the ShinyStan R package (version 2.5.0). Please see file for additional reference documentation.

## Scripts for compartmental model results 

Please see documentation and files in dengue-Zika-chik_Americas/06-compartmentalModels





