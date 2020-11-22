# 0-functions

This subdirectory houses various functions used to streamline the model fitting process and output summarization process. Below is a brief description of what each of the files contains, organized alphebetically.

### `calc_mult_gof.R`

Contains a function that calculates chi-squared goodness of fit statistics for a given age/sex composition data set.

### `edit_full_model.R`

This function allows running many different models all based on a master model file. Based on the input arguments to this function, it edits the code from a full model to remove complexity and arrive at a simpler model (e.g., one that does not include time-trending probability of returning at age). Relies heavily on the 'stringr' package for pattern matching and replacement.

### `extract_jags_metadata.R`

After the model is done fitting using JAGS, this function scrapes the output object to neatly format some important information that might be of interest later (mcmc settings, start time, stop time, elapsed time, effective parameters, DIC statistics).

### `full_model.R`

Contains the code for the full model: all trends included and use of Dirichlet-distributed age at return by sex. These features are edited by the function found in `edit_full_model.R` for any given model run.

### `gen_inits.R`

Contains a function to generate reasonable initial values for the MCMC sampler depending on the model structure.

### `get_WAIC.R`

Contains a function to calculate WAIC from any model. In order to use this function, ensure that the option `do_waic` is set to `TRUE` in `2-model-fit/2-fit-model-any.R`, otherwise the necessary information will not be calculated during MCMC sampling.

### `id_model.R`

Contains a function that creates a model id (e.g., E-ASL) based on the meta data exported when fitting the model. Options exist to include the reproductive unit (N, E, EM), trends (A, S, L), fecundity data source, and whether Dirichlet ages were used.

### `pp_checks.R`

Contains functions to perform posterior predictive checks for abundance and composition data. Also contains a function for plotting the output from these checks.

### `prep_samples.R`

Contains a function that calculates average parameter values to use for equilibrium calculations for all posterior samples from a model fit. Requires a time period be provided, and averages are calculated and returned for each posterior sample. Quantities include: alpha, beta, sigma_R, phi, selectivity (by age/sex/mesh size), probability of return (by age/sex), and reproductive units (produced by each individual by age/sex)

### `print_mcmc_start_message.R`

Contains a function to create and print a totally unnecessary but nice summary of the model that is about to be fitted.

### `write_full_model.R`

Takes the contents of the full model and writes it to a txt file to be edited by the function found in `edit_full_model.R`.

### `yield_funcs.R`

Contains functions for performing the yield-per-recruit algorithm used to obtain estimates of equilibrium states based on maximum catch or maximum recruitment.



