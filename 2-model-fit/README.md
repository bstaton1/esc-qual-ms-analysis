# 2-model-fit

This subdirectory contains the code used to actually perform the state-space model fitting.

To streamline the fitting process, this code was built to be ran on a high performance computer (HPC) system. The scripts `0-job-template.sh`, `0-queue-model.sh`, `create-job.R`, and `rm-jobs.sh` are used in that process. These are tailored to the specific HPC used to run the models for our study, and will not work for HPCs other than the Hopper cluster at Auburn University. Most users can ignore these files.

### `inputs`

Contains several input files that contain the data the model is fitted to. 

* `com-age-sex-comp.csv` contains the commercial harvest age/sex composition, obtained by running the code in the `1-data-prep/a-age-data/3-com` directory
* `esc-age-sex-comp.csv` contains the escapement age/sex composition, obtained by running the code in the `1-data-prep/a-age-data/1-esc` directory
* `sub-age-sex-comp.csv` contains the subsistence harvest age/sex composition, obtained by running the code in the `1-data-prep/a-age-data/2-sub` directory
* `run-harv-ests-cv.csv` contains the estimated abundance states of total run, commerical harvest, and subsistence harvest (aggregated across age/sex) and their associated observation coefficients of variation. Harvest data pulled from Appendix C2 of Larson ([2020](http://www.adfg.alaska.gov/FedAidPDFs/RIR.3A.2020.02.pdf)); total run estimates pulled from Table 2 in that same report
* `esc-mean-length.csv` contains the mean length at age and sex of fish in the escapement, obtained by running the code in `1-data-prep/b-length-data
* `mesh-types.csv` contains identifiers that indicate which mesh size was used by each fishery in each year. "unr" represents "unrestricted" and corresponds to 8 inch mesh, "res" represents "restricted" and corresponds to 6 inch mesh.

### `model-files`

Stores the .txt files that are called by JAGS.

### `1-compile-data.R`

Compiles the input data for a given model.

### `2-fit-model-any.R`

Performs the model fitting and exports the output to storage. It is intended to be ran via command line, i.e.,

```bash
Rscript 2-fit-model-any.R 10
```

from a terminal window will run the code to fit model 10 (E-ASL, see `model-key.csv`). It can also be ran interactively as well if users do not have access to a command line.

### `model-key.csv`

Contains the settings for each of the 31 models ran for main text and sensitivity analyses. Many aspects of the `1-compile-data.R` and `2-fit-model-any.R` respond to the information contained in this file.

# Running models on the HPC

_This section is only for users who have access to the Hopper HPC cluster. If you are not an Auburn University employee or student, you probably do not have access._

Once you have this code on the HPC, navigate to the directory:

```bash
cd esc-qual-ms-analysis/2-model-fit
```

Then run

```bash
sh 0-queue-model.sh
```

You will be prompted to enter the model number (values 1 -- 31 accepted, see the values in `model-key.csv` for details on which model is which). Type a number and press <kbd>ENTER</kbd>. Next you will be prompted to enter the Hopper node to run the job on (values of 055, 144, or 145 are accepted). Type the node and press <kbd>ENTER</kbd>. You can check that the job was sent by running

```bash
qstat
```

When the model is done running (will take a day or two once out of the queue), the output files will be placed in the directory `esc-qual-ms-analysis/model-output`. You can navigate there to check that the model finished successfully (there should be `post-x.rds`, `msy-x.rds`, `Rmax-x.rds`, and `meta-x.rds`, where `x` is the model number you ran).

Once you have several models, you should bundle the output into a tar.gz file to make it easier to copy the large output back to your computer. Navigate to the main directory:

```bash
cd
cd esc-qual-ms-analysis
```

and run

```bash
sh make-bundle.sh
```

You will be prompted to enter several model numbers separated by spaces. You shouldn't enter more than 3 models at a time, as this is run on the local node and takes a fair amount of time. Long-running commands that are ran on the local node are automatically cancelled. You will next be prompted to enter a file name for the output. Say you entered models 1, 2, and 3, the name might be something like `eq-bundle_1-3.tar.gz`

Once you have completed this for all models you wish to run, the .tar.gz files can be copied back to your computer using `scp`.

