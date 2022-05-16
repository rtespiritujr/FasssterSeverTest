r = plumber::plumb("timeseriesVax/1_SetupVAX.R") 
r$run(port=8686,swagger = TRUE)
getwd()
