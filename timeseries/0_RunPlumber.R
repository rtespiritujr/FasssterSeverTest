r = plumber::plumb("timeseries/1_Setup.R") 
r$run(port=8585,swagger = TRUE)
getwd()
