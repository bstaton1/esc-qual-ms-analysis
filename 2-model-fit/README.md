# 2-model-fit

This subdirectory contains the code used to actually perform the fitting of state-space models.

To streamline the fitting process, this code was built to be ran on a high performance computer (HPC) system. The scripts `0-job-template.sh`, `0-queue-model.sh`, `create-job.R`, and `cleanup-jobs.sh` are used in that process. These are tailored to the specific HPC used to run the models for our study, and will not work for HPCs other than the Hopper cluster at Auburn University. Most users can ignore these files.

The remainder of this README describes the structure of this subdirectory and its files.

### `inputs`

This subdirectory contains several input files that contain the data the model is fitted to. 

* `com-age-sex-comp.csv` contains the commercial harvest age/sex composition, obtained by running the code in the `1-data-prep/a-age-data/3-com` directory
* `esc-age-sex-comp.csv` contains the escapement age/sex composition, obtained by running the code in the `1-data-prep/a-age-data/1-esc` directory
* `sub-age-sex-comp.csv` contains the subsistence harvest age/sex composition, obtained by running the code in the `1-data-prep/a-age-data/2-sub` directory
* `run-harv-ests-cv.csv` contains the estimated abundance states of total run, commerical harvest, and subsistence harvest (aggregated across age/sex) and their associated observation coefficients of variation. Harvest data pulled from Appendix C2 of Larson ([2020](http://www.adfg.alaska.gov/FedAidPDFs/RIR.3A.2020.02.pdf)); total run estimates pulled from Table 2 in that same report
* `esc-mean-length.csv` contains the mean length at age and sex of fish in the escapement, obtained by running the code in `1-data-prep/b-length-data`
* `mesh-types.csv` contains identifiers that indicate which mesh size was used by each fishery in each year. "mesh8" represents 8 inch mesh and "mesh6" represents 6 inch mesh.

### `model-files`

Stores the `.txt` files that contain the formal model definition called by JAGS. Only the JAGS model file for the full model is tracked by Git, other files placed in this subdirectory are ignored.

### `1-compile-data.R`

Compiles the input data for a given model.

### `2-fit-model-any.R`

Performs the model fitting and exports the output to storage. It is intended to be ran via command line, i.e., executing:

```bash
Rscript 2-fit-model-any.R 10
```

from a terminal window will run the code to fit model 10 (model E-ASL, see `model-key.csv`). It can also be ran in an interactive R session as well if users do not have access to a command line interface.

### `model-key.csv`

Contains the "settings" for each of the 31 models ran for main text and sensitivity analyses. Each model has a setting for:

* The model number (1 - 31)
* The "z_unit" (fish_count, egg_count, or egg_mass)
* Whether age at return, sex of return, or length at return are time trending (0 = no, 1 = yes), 
* The _a_ and _b_ coefficients used in translating length-at-age to fecundity
* The fecundity data set used to obtain the _a_ and _b_ coefficients
* Whether the model belongs in the main text analysis, or a supplement
* The scheme used for rescaling multinomial counts (by data set) to obtain an effective sample size rather than the actual sample size

Many aspects of the `1-compile-data.R` and `2-fit-model-any.R` respond to the information contained in this file.

# Running models on the HPC

_This section is only for users who have access to the Hopper HPC cluster. If you are not an Auburn University employee or student, you probably do not have access._

First, get the code for this repository onto the HPC:

```bash
git clone git@github.com:bstaton1/esc-qual-ms-analysis.git
```

Next, navigate to the directory:

```bash
cd esc-qual-ms-analysis/2-model-fit
```

Then run

```bash
sh 0-queue-model.sh
```

You will be prompted to enter the model number (values 1 -- 31 accepted, see the values in `model-key.csv` for details on which model is which). Type a number and press <kbd>ENTER</kbd>. Next you will be prompted to enter the Hopper node to run the job on (values of 055, 144, or 145 are accepted). Type the node and press <kbd>ENTER</kbd>. After a couple seconds, a job number will be returned informing you the job has been successfully sent to the queue. You can check that the job was sent by running

```bash
qstat
```

When the model is done running (will take a day or two once out of the queue), the output files will be placed in the directory `esc-qual-ms-analysis/model-output`. You can navigate there to check that the model finished successfully (there should be `post-x.rds`, `msy-x.rds`, `Rmax-x.rds`, and `meta-x.rds`, where `x` is the model number you ran).

Once you have several models, you should bundle the output into a archive file (i.e., ending in `tar.gz`) to make it easier to copy the large output back to your computer. Navigate to the main directory:

```bash
cd
cd esc-qual-ms-analysis
```

and run

```bash
sh make-bundle.sh
```

You will be prompted to enter several model numbers separated by spaces. Do not enter more than 3 models at a time, as this is run on the local node and takes a fair amount of time. Long-running commands that are ran on the local node are automatically cancelled. You will next be prompted to enter a file name for the output. Say you entered models 1, 2, and 3, the name might be something like `bundle_1-3.tar.gz`

Once you have completed this for all models you wish to run, the `*.tar.gz` files can be copied back to your computer using `scp`.

## Defining JAGS Model Structure

One aspect of the coding framework used here is perhaps worthy of note to interested readers. The analysis fits a total of 31 models, 12 for the main-text analysis and the remainder as part of sensitivity analyses. Some of the alternative models require different JAGS code to fit, others are altered by simply changing the data passed to them.

In the cases where the model code needs to be changed, it would be cumbersome to have many different model files to maintain. If a small change needed to be made, then it would need to be made to many files simultaneously. This can inhibit model development and is error prone (e.g., the change is made in some model files, but not others).

As a solution, this code relies on a main model definition (found in a function body in `0-functions/full_model.R`), which is used to create a `full-model.txt` file. This houses the code for the most complex model, and a function is used (`edit_full_model.R`) to change certain lines to simplify the full model for a particular simpler model.

For example, consider the case concerning models that do not incorporate time-trending return by sex dynamics. In the full model, the part of the JAGS code controlling the temporal aspects of return by sex looks like this:

```R
### PART 2: MATURITY PROCESS SUBMODEL ###
  # 2a) brood-year specific sex ratio
  delta_0 ~ dnorm(0,1e-6)
  delta_1 ~ dnorm(0,1e-6)
  for (y in 1:ny) {
    logit(psi[y]) <- delta_0 + delta_1 * y
    R_sex[y,1] <- R[y] * psi[y]
    R_sex[y,2] <- R[y] * (1 - psi[y])
  }
```

This model will allow the probability that a recruit returns as a female to trend over time. If we wanted to fit a model without this (i.e., with `delta_1` fixed at zero), we could simply save this full model as a new file with a new name, and edit the line(s) in question.

This is the job of `edit_full_model()`. It accepts a `sex_trend` argument which, if `TRUE`, will execute the following R code to make the switch to set `delta_1 <- 0` rather than having it take on a non-zero value (i.e., a prior):

```R
match = model_lines[str_detect(model_lines, "delta_1 ~ ")]
white_space =  unlist(str_extract_all(match, "  +"))
replace = "delta_1 <- 0"
model_lines[which(model_lines == match)] = paste(white_space, replace, sep = "")
```

The first line finds the line that needs to be changed. The second line extracts all of the white space in front of the text that needs changing (to make sure the indenting remains the same after the edit is made - this is not strictly necessary). The third line specifies what the revised line should look like. The fourth line performs the replacement.

This is mentioned here only because this framework proved to be very useful in model development, and Staton hasn't seen it done before. It is likely that other practitioners may find this sort of trick useful. 