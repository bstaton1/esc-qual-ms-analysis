# 3-post-process

This subdirectory contains all code for creating the figures found in the main text and the online supplements.

### `main-text-figs.R`

This script creates all figures found in the main text and creates some of the numerical output presented in-text. You must have posterior samples form models 1 - 12 before attempting to run this code.

### `Detailed_Output_template.Rmd`

This contains the source code to build the detailed model-specific supplements for any model in the analysis. The file `render-Detailed-output.R` creates the supplements. It takes quite a while to run (~1hr for all models).

### `os-sensitivity`

This subdirectory contains all of the material to create the online supplement that presents output of sensitivity analyses and various other analyses that would not fit in the main text. It takes quite a while to run (~15 mins).

