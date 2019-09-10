# dengue-Zika-chik_Americas/06-compartmentalModels

Please find below descriptions of the scripts used for our analyses. Please note that soome of the scripts will not run because they require the simulation output (100 simulations per enhancement and cross-protection scenario for each R_0 pair). We provide some summary data from our simulations in the *sims_output* subfolder to provide data necessary to generate figures.

## Model files

Equations for implementing the stochastic compartmental model are provided as .txt files in the *sims_input* subfolder

## Simulation output processing files

"indeces.R" and "indexing_functions.R" provide functions for transferring between compartment indeces and compartment names

"processSimulations.R" loads the simulation ouput and calculates summary metrics and inter-quartile ranges for DENV prevalence after the introduction of ZIKV.
   
"08-Figure4.R" generates Fig. 4 of the main text with simulation results


