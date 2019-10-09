rm(list = ls(all = T))

models = c(1,2,10,12)
for (model in models) {
  out_file = paste0("Detailed_Output_", model, ".html", sep = "")
  rmarkdown::render(
    input = "Detailed_Output_template.Rmd",
    output_format = "html_document",
    output_file = out_file)
  out_file = paste0("Detailed_Output_", model, ".html", sep = "")
  browseURL(out_file)
}

