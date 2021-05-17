> This repository stores the code and data used to perform the analysis presented in _Incorporating demographic information into spawner-recruit analyses alters biological reference points for a western Alaska salmon population_ by authors B. Staton, M. Catalano, S. Fleischman, and J. Ohlberger (published in the *Canadian Journal of Fisheries and Aquatic Sciences*)

[![ArticleDOI](https://img.shields.io/badge/Article%20DOI-10.1139%2Fcjfas--2020--0478-blue)](https://doi.org/10.1139/cjfas-2020-0478)

[![GitHub Repo Archive DOI](https://img.shields.io/badge/GitHub%20Repo%20Archive%20DOI-10.5281%2Fzenodo.4382757-blue)](https://doi.org/10.5281/zenodo.4382757)

## Analysis Structure and Motivation

The analysis fits age/sex structured Bayesian state-space spawner-recruit models to Chinook salmon (_Oncorhynchus tshawytscha_) data from the Kuskokwim River, western Alaska. The intent of the analysis is to investigate the consequences of time-trending demographic characteristics (probability of adult return by age, probability of adult return by sex, and mean adult size at age [which we convert into fecundity]). Thus, several state-space models are fitted, including those that assume recruitment is a function of (a) total spawners, (b) total egg production, or (c) total egg mass production, and that assume demographic characteristics are either time-trending or time-constant. All models (12 total in the main text analysis) include estimation of a size-selectivity function to explain discrepancies between age/sex composition data collected from sources that differ in selectivity and all models are fitted to identical data sets. Posterior estimates of population dynamics parameters from state-space models are supplied to a yield-per-recruit algorithm to estimate biological reference points that vary over time, including the escapement abundance (regardless of age or sex) expected to maximize total harvest or maximize total recruitment. By comparing these estimates among models that account for changes in "escapement quality" to those that do not, we are able to shed light on the plausible management consequences of accounting for these factors in Pacific salmon stock assessments.

## Repository Structure

The repository is organized as follows (more detailed READMEs are found in each subdirectory).

| Subdirectory     | Description                                                  |
| ---------------- | ------------------------------------------------------------ |
| `0-functions`    | Houses code for a variety of functions used to streamline the model fitting and summarization process |
| `1-data-prep`    | Houses code for compiling fate-specific age/sex composition data and length-at-age/sex data |
| `2-model-fit`    | Houses code to fit the models with JAGS, including code to interface with the HPC |
| `3-post-process` | Houses code to create figures found in the main text and to create all online supplements with Rmarkdown |

## Model Output

Given the output from these models is very large (thousands of posterior samples across thousands of nodes for 31 models; 12 models for main text analyses, remaining 19 models for sensitivity analyses), they are not hosted in this Git repository. Users interested in running the code in the `3-post-process` directory will need to run the models themselves. 

Alternatively, posterior samples used for generating figures found in the publication are available upon request from the authors.

The complete set of models for publication were fitted using the [Hopper High Performance Computing Cluster](https://hpcportal.auburn.edu/hpc/2016_cluster.php) at Auburn University to allow running such a large number of models with very long MCMC algorithms (~1.1 million iterations per each of 4 chains, per fitted model) to ensure adequate convergence. However, interested users can fit the models on their own computer, using vastly reduced MCMC dimensions to obtain posterior output, recognizing that MCMC convergence may not be completely adequate to make totally valid inference.

## Dependencies

Program [JAGS](<http://mcmc-jags.sourceforge.net/>) is required to run the models and the R package '[jagsUI](https://CRAN.R-project.org/package=jagsUI)' is used to call JAGS from R. Other miscellaneous packages are used for data formatting and preparations (e.g., 'reshape2', 'dplyr', etc.). See the file `load-packages.R` for a complete list of required packages.

Posterior summarization relies heavily on the R package '[postpack](https://bstaton1.github.io/postpack/)', written by B. Staton, to make inference from many models simultaneously.

One other package written by B. Staton is used ('StatonMisc'). At the time of writing, it is [found only on GitHub](https://github.com/bstaton1/StatonMisc). The exact version used in this project can be installed via:

```R
install.packages("remotes")
remotes::install_github("bstaton1/StatonMisc", ref = "0803a5a")
```

## Session Info

For reproducibility purposes, all post-MCMC analyses were conducted using this configuration:

```
- Session info --------------------------------------------------------------------------
 setting  value                       
 version  R version 4.0.2 (2020-06-22)
 os       Windows 10 x64              
 system   x86_64, mingw32             
 ui       RStudio                     
 language (EN)                        
 collate  English_United States.1252  
 ctype    English_United States.1252  
 tz       America/Los_Angeles         
 date     2020-11-22                  

- Packages ------------------------------------------------------------------------------
 package     * version date       lib source                              
 abind       * 1.4-5   2016-07-21 [1] CRAN (R 4.0.0)                      
 assertthat    0.2.1   2019-03-21 [1] CRAN (R 4.0.2)                      
 cli           2.0.2   2020-02-28 [1] CRAN (R 4.0.2)                      
 coda          0.19-3  2019-07-05 [1] CRAN (R 4.0.2)                      
 colorspace    1.4-1   2019-03-18 [1] CRAN (R 4.0.2)                      
 crayon        1.3.4   2017-09-16 [1] CRAN (R 4.0.2)                      
 digest        0.6.25  2020-02-23 [1] CRAN (R 4.0.2)                      
 dplyr       * 1.0.0   2020-05-29 [1] CRAN (R 4.0.2)                      
 ellipsis      0.3.1   2020-05-15 [1] CRAN (R 4.0.2)                      
 evaluate      0.14    2019-05-28 [1] CRAN (R 4.0.2)                      
 fansi         0.4.1   2020-01-08 [1] CRAN (R 4.0.2)                      
 generics      0.0.2   2018-11-29 [1] CRAN (R 4.0.2)                      
 glue          1.4.1   2020-05-13 [1] CRAN (R 4.0.2)                      
 hms           0.5.3   2020-01-08 [1] CRAN (R 4.0.2)                      
 htmltools     0.5.0   2020-06-16 [1] CRAN (R 4.0.2)                      
 httr          1.4.1   2019-08-05 [1] CRAN (R 4.0.0)                      
 jagsUI      * 1.5.1   2019-07-30 [1] CRAN (R 4.0.2)                      
 kableExtra  * 1.1.0   2019-03-16 [1] CRAN (R 4.0.2)                      
 knitr       * 1.29    2020-06-23 [1] CRAN (R 4.0.2)                      
 latex2exp   * 0.4.0   2015-11-30 [1] CRAN (R 4.0.2)                      
 lattice     * 0.20-41 2020-04-02 [1] CRAN (R 4.0.2)                      
 lifecycle     0.2.0   2020-03-06 [1] CRAN (R 4.0.2)                      
 magrittr    * 1.5     2014-11-22 [1] CRAN (R 4.0.2)                      
 munsell       0.5.0   2018-06-12 [1] CRAN (R 4.0.2)                      
 packrat       0.5.0   2018-11-14 [1] CRAN (R 4.0.2)                      
 pillar        1.4.6   2020-07-10 [1] CRAN (R 4.0.2)                      
 pkgconfig     2.0.3   2019-09-22 [1] CRAN (R 4.0.2)                      
 plyr          1.8.6   2020-03-03 [1] CRAN (R 4.0.2)                      
 postpack    * 0.5.2   2020-09-17 [1] CRAN (R 4.0.2)                      
 purrr         0.3.4   2020-04-17 [1] CRAN (R 4.0.2)                      
 R6            2.4.1   2019-11-12 [1] CRAN (R 4.0.2)                      
 Rcpp          1.0.5   2020-07-06 [1] CRAN (R 4.0.2)                      
 readr         1.3.1   2018-12-21 [1] CRAN (R 4.0.2)                      
 reshape2    * 1.4.4   2020-04-09 [1] CRAN (R 4.0.2)                      
 rjags         4-10    2019-11-06 [1] CRAN (R 4.0.2)                      
 rlang         0.4.8   2020-10-08 [1] CRAN (R 4.0.3)                      
 rmarkdown   * 2.3     2020-06-18 [1] CRAN (R 4.0.2)                      
 rstudioapi    0.11    2020-02-07 [1] CRAN (R 4.0.2)                      
 rvest         0.3.5   2019-11-08 [1] CRAN (R 4.0.0)                      
 scales      * 1.1.1   2020-05-11 [1] CRAN (R 4.0.2)                      
 sessioninfo   1.1.1   2018-11-05 [1] CRAN (R 4.0.2)                      
 StatonMisc  * 0.1.5   2020-09-17 [1] Github (bstaton1/StatonMisc@0803a5a)
 stringi       1.4.6   2020-02-17 [1] CRAN (R 4.0.0)                      
 stringr     * 1.4.0   2019-02-10 [1] CRAN (R 4.0.2)                      
 tibble        3.0.4   2020-10-12 [1] CRAN (R 4.0.3)                      
 tidyselect    1.1.0   2020-05-11 [1] CRAN (R 4.0.2)                      
 tinytex     * 0.24    2020-06-20 [1] CRAN (R 4.0.2)                      
 truncnorm   * 1.0-8   2018-02-27 [1] CRAN (R 4.0.2)                      
 vctrs         0.3.4   2020-08-29 [1] CRAN (R 4.0.3)                      
 viridisLite   0.3.0   2018-02-01 [1] CRAN (R 4.0.2)                      
 webshot       0.5.2   2019-11-22 [1] CRAN (R 4.0.2)                      
 withr         2.2.0   2020-04-20 [1] CRAN (R 4.0.2)                      
 xfun          0.15    2020-06-21 [1] CRAN (R 4.0.2)                      
 xml2          1.3.2   2020-04-23 [1] CRAN (R 4.0.2)                      

[1] C:/Users/bstaton/Documents/R/R-4.0.2/library
```

