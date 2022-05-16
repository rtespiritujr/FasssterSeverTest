source("timeseries/2_OdeFunction.R")

#* Get projections
#* @param lambdaDateInput End of CQs (yyyy-mm-dd;yyyy-mm-dd;yyyy-mm-dd)
#* @param lambdaValuesInput Lambda values for each CQ end (.19;.30;.45)
#* @param psgcInput No leading zero (ie. Region 1 is only 10000000)
#* @param startHcDate Start of increased HC (yyyy-mm-dd)
#* @param HcLevel HC Level in decimal form (.xx)
#* @param projectionDuration Duration of projection in days
#* @param scaleLevel Scale level. Put "0" if no scaling to be done
#* @param fitParamToUse "Active" or "Cumulative"
#* @param interventionStartDate start date for intervention (yyyy-mm-dd)
#* @param interventionEndDate end date for intervention (yyyy-mm-dd)
#* @param interventionPercentage percentage of effect (.xx)
#* @param interventionCompliance percentage of compliance (.xx)
#* @param dateAdjustment Should the results be adjusted to the nearest date ("TRUE"/"FALSE")
#* @get /getProjections
function(lambdaDateInput, lambdaValuesInput, psgcInput, 
         startHcDate, HcLevel, projectionDuration, scaleLevel, fitParamToUse, 
         interventionStartDate, interventionEndDate, interventionPercentage,
         interventionCompliance, dateAdjustment) {
  tryCatch({
    return(RunProjections(lambdaDateInput, lambdaValuesInput, psgcInput, 
                          startHcDate, HcLevel, projectionDuration, scaleLevel, fitParamToUse, 
                          interventionStartDate, interventionEndDate, interventionPercentage,
                          interventionCompliance, dateAdjustment))
  },
  error = function(e) {
    return(e)
  })
}


#* Get projections
#* @param psgcInput No leading zero (ie. Region 1 is only 10000000)
#* @get /getPsgcDetails
function(psgcInput) {
  return(getPsgcDetails(psgcInput))
}


#* Get projections
#* @param psgcInput No leading zero (ie. Region 1 is only 10000000)
#* @get /getPsgcEconDetails
function(psgcInput) {
  packages <- c("readr","dplyr")
  
  lapply(packages, require, character.only = TRUE)
  
  data.df <- read.csv("timeseries/inputs/econ_params.csv") %>% filter(area == psgcInput)
  
  tryCatch({
    return(data.df)
  },
  error = function(e) {
    return(error)
  })
  
}
