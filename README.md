**INSERT ZENODO DOI GRAPHIC HERE WHEN MINTED**

This repository stores the code and data used to perform the analysis presented in Staton et al.: _Incorporating demographic information into Pacific salmon spawner-recruit analyses alters management reference points for a western Alaska population_ (DOI: **Insert DOI if accepted**)

## Repository Structure

The repository is organized as follows (more detailed READMEs are found in each subdirectory):

* `0-functions`: houses the source code for a variety of functions used to streamline the model fitting and summarization process. 
* `1-data-prep`: houses the source code for compiling the age/sex composition and length-at-age-and-sex data used during model fitting
* `2-model-fit`: houses the code to fit the model
* `3-post-process`: houses the code to create the figures found in the main text and to create all online supplements

## Model Output

Given the output from these models is very large (thousands of posterior samples across thousands of nodes for 31 models), they are not hosted in this Git repository. Users interested in running the code in `3-post-process` directory will need to run the models themselves. 

## Dependencies

Program [JAGS](<http://mcmc-jags.sourceforge.net/>) is required to run the models and the R package `jagsUI` (available on CRAN) is used to call JAGS from R. Miscellaneous other packages are used for data formatting and preparations.

Posterior summarization relies on the `postpack` [package](https://github.com/bstaton1/postpack), written by B. Staton. At the time of writing, it is found only on Github, to install it run:

```R
install.packages("remotes") # if you don't have it already
remotes::install_github("bstaton1/postpack", build_vignettes = T)
```