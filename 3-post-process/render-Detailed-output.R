# clear the workspace
rm(list = ls(all = T))

# set the working directory to this location
setwd("3-post-process")

# where to put the output
html_dir = "os-detailed"
if (!dir.exists(html_dir)) dir.create(html_dir)

# which models to produce supplements for
models = c(1,3,6,5,4,9,10,12)
n_models = length(models)
model_ids = c("N-0", "N-ASL", "E-0", "E-L", "E-S", "E-A", "E-SL", "E-AL", "E-AS", "E-ASL", "EM-0", "EM-ASL")
for (m in 1:n_models) {
  models = c(1,3,6,5,4,9,10,12)
  models = 10
  model = models[m]
  html_dir = "os-detailed"
  
  out_file = paste0("Supp-", LETTERS[m+1], "_", model_ids[model], ".html", sep = "")
  rmarkdown::render(
    input = "Detailed_Output_template.Rmd",
    output_file = file.path(html_dir, out_file)
  )
}

# set the working directory back to the project root
setwd("../")
