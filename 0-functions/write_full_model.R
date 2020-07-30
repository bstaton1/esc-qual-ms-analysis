write_full_model = function(path = file.path("2-model-fit", "model-files", "full-model.txt")) {
  dput(jags_model_code, file = path, control = "all")
}
