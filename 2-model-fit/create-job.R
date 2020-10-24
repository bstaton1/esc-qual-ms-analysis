
##### THIS SCRIPT'S PURPOSE IS TO CREATE A TEMPORARY JOB FILE TO PASS TO QSUB #####
# it accepts two command line arguments:
  # $1: model
  # $2: node (055, 144, or 145)

# load packages
source("../load-packages.R")

# read in the command line arguments
args = commandArgs(trailingOnly = T)
model = args[1]
node = args[2]

# stop if node incorrect
if (!(node %in% c("055", "144", "145"))) stop("The node must be one of: '055', '144', or '145'")

# stop if model incorrect
if (!(model %in% read.csv("model-key.csv")$model)) stop("The requested model was not found in 'model-key.csv'")

# read in the send job template
code = readLines("0-job-template.sh")

# replace which node to use: this is one "computer" on the super computer. It has ram, processors, etc.
if (node == "055") {
  node_settings = "#PBS -l nodes=node055:ppn=4,walltime=60:00:00,flags=ADVRES:liuzhan_lab"
} else {
  node_settings = "#PBS -q fastfat -l nodes=nodeNODE:ppn=4,walltime=60:00:00,flags=ADVRES:liuzhan_ff"
  node_settings = str_replace(node_settings, "NODE", as.character(node))
}
code = unlist(lapply(code, function(x) str_replace(x, "#NODE-SETTINGS-HERE", node_settings)))

# replace which model to run: this is a number; models defined in model-key.csv
code = unlist(lapply(code, function(x) str_replace(x, "MODEL", as.character(model))))
code = c(code, "")   

# write out the job file
file_name = paste0("0-job-model-", model, ".sh")

# write the code to the file
writeLines(code, file_name)

# print a message
cat("Job file created:", file_name, "\n")
