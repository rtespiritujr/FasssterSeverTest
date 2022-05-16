library(plumber)
r = plumber::plumb("plumberAPI.R")  # Where 'plumberAPI.R' is the location of the file shown above
r$run(port=8888,swagger = TRUE)