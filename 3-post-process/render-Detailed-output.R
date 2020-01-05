# clear the workspace
rm(list = ls(all = T))

# where to put the output
html_dir = "os-detailed"
if (!dir.exists(html_dir)) dir.create(html_dir)

models = c(1,2,3,7,8,9)
model_ids = c("N-0", "N-ASL", "E-0", "E-L", "E-S", "E-A", "E-SL", "E-AL", "E-AS", "E-ASL", "EM-0", "EM-ASL")
for (model in models) {
  out_file = paste0("Detailed_Output_", model, ".html", sep = "")
  rmarkdown::render(
    input = "Detailed_Output_template.Rmd",
    output_file = file.path(html_dir, out_file))
  
  # if you want to open each file when complete
  # environment is cleared when running each document, so you have to specify the file location again
  html_dir = "os-detailed"
  out_file = file.path(html_dir, paste0("Detailed_Output_", model, ".html", sep = ""))
  browseURL(out_file)
}

