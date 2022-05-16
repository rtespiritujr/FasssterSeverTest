source("timeseriesVax/2_OdeFunctionVAX.R")
source("timeseriesVax/1_LoadLibraries.R")
source("linelist/read_linelist_non_imputed.R")
source("timeseriesVax/3_ProcessActiveCasesVAX.R")

#* Get projections SVEIR
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
#* @param v1Value vaccination pop
#* @param v2Value vaccination pop
#* @param jValue vaccination pop
#* @param filterPSGCs psgcs to filter

#* @get /getProjections
function(lambdaDateInput, lambdaValuesInput, psgcInput, 
         startHcDate, HcLevel, projectionDuration, scaleLevel, fitParamToUse, 
         interventionStartDate, interventionEndDate, interventionPercentage,
         interventionCompliance, dateAdjustment,v1Value,v2Value,jValue,filterPSGCs) {
  tryCatch({
    return(RunProjections(lambdaDateInput, lambdaValuesInput, psgcInput, 
                          startHcDate, HcLevel, projectionDuration, scaleLevel, fitParamToUse, 
                          interventionStartDate, interventionEndDate, interventionPercentage,
                          interventionCompliance, dateAdjustment,v1Value,v2Value,jValue,filterPSGCs))
  },
  error = function(e) {
    return(e)
  })
}

#* Get projections SVEIRS
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
#* @param v1Value vaccination pop
#* @param v2Value vaccination pop
#* @param v3Value vaccination pop
#* @param jValue vaccination pop
#* @param filterPSGCs psgcs to filter

#* @get /getProjectionsSVEIRS
function(lambdaDateInput, lambdaValuesInput, psgcInput, 
         startHcDate, HcLevel, projectionDuration, scaleLevel, fitParamToUse, 
         interventionStartDate, interventionEndDate, interventionPercentage,
         interventionCompliance, dateAdjustment,v1Value,v2Value,v3Value,jValue,filterPSGCs) {
  tryCatch({
    return(RunProjections_SVEIRS(lambdaDateInput, lambdaValuesInput, psgcInput, 
                          startHcDate, HcLevel, projectionDuration, scaleLevel, fitParamToUse, 
                          interventionStartDate, interventionEndDate, interventionPercentage,
                          interventionCompliance, dateAdjustment,v1Value,v2Value,v3Value,jValue,filterPSGCs))
  },
  error = function(e) {
    return(e)
  })
}

#* Get Location Details SVEIR
#* @param psgcInput No leading zero (ie. Region 1 is only 10000000)
#* @get /getPsgcDetails
function(psgcInput) {
  return(getPsgcDetails(psgcInput))
}

#* Get Location Details SVEIRS
#* @param psgcInput No leading zero (ie. Region 1 is only 10000000)
#* @get /getPsgcDetailsSVEIRS
function(psgcInput) {
  return(getPsgcDetails_SVEIRS(psgcInput))
}

#* Get projections
#* @param psgcInput No leading zero (ie. Region 1 is only 10000000)
#* @get /getPsgcEconDetails
function(psgcInput) {
  packages <- c("readr","dplyr")
  
  lapply(packages, require, character.only = TRUE)
  
  data.df <- read.csv("timeseriesVax/inputs/econ_params.csv") %>% filter(area == psgcInput)
  
  tryCatch({
    return(data.df)
  },
  error = function(e) {
    return(error)
  })
  
}
