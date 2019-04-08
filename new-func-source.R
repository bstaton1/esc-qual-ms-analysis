
path = "../0-functions"
files = dir(path)
junk = sapply(files, function(x) source(file.path(path, x)))

