# Beaudrot et al 2020 Ecology
Code and files for Beaudrot, Palmer, Anderson & Packer 2020 Ecology manuscript "Mixed-species groups of Serengeti grazers: a test of the stress gradient hypothesis"

File summary:

File "Analysis_and_coefficient_plot.R" contains the R code for reproducing the model results, g-test results, odds ratios, and Figure 3 coefficient plot

File "grazers.csv" is the main data file necessary for reproducing the model results and coefficient plot in the main text. File "grazers_excluding_nocturnal.csv"" is the parallel file for reproducing the appendix results that exclude nocturnal observations. In these files, each row is a camera trap observation that contains a focal grazer species named in 'species'. If an observation includes a mixed-species group, then assoc=1 and the name of the second species is given in 'MixedSpp1'. Model covariates included season, NDVI, 'BINHAB' for habitat, 'encount_risk' for lion density, and 'KOP.DIST.M' for distance to nearest kopje.

Figure "g.test.R" contains the function to perform a G-test, which is a log-likelihood goodness of fit text.

File "All_other_figures.R" contains the code to produce Figure 2 in the main text and all supplementary figures and associated results. It requires the following four data files:

Files "obs2.csv" and "obs2_excluding_nocturnal.csv" contain observational data for each 16-day ndvi sampling bin that is further formatted for figures and appendix results.

File "ndvi.csv" has ndvi values at each camera trap for each 16-day ndvi sampling bin.

File "new_covs.csv" has camera trap specific covariate data.
