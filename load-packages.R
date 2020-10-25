
## all packages used in this project
## source this at the top of each script

suppressPackageStartupMessages({
  # CRAN packages
  library(abind)         # for combining arrays
  library(dplyr)         # for rapid large data manipulation
  library(jagsUI)        # for calling JAGS from R
  library(kableExtra)    # for making nice tables in Rmd output
  library(knitr)         # for rendering Rmd output
  library(latex2exp)     # for writing math symbols on plots
  library(magrittr)      # for the pipe character
  library(postpack)      # for summarizing posteriors
  library(scales)        # for transparent colors
  library(reshape2)      # for reformatting data: long to wide and back
  library(rmarkdown)     # for rendering Rmd output
  library(stringr)       # for string manipulation
  library(tinytex)       # for build PDF rmd output
  library(truncnorm)     # for truncated normal dist
  
  # GitHub packages
  library(StatonMisc)    # for shortcut utilities: progress_updater(), file_device(), cv2sig(), sig2cv(), date2doy(), doy2date(), etc.
})
