
path = "../0-functions"
files = dir(path, pattern = "\\.R$")
junk = sapply(files, function(x) source(file.path(path, x)))

