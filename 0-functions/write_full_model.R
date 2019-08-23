write_full_model = function(path = "model-files/full-model.txt") {
  dput(jags_model_code, file = path, control = "all")
}