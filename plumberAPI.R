############
# EpiNow API
############
#* input city psgc
#* @param city_psgc Input city_psgc
#* @get /getepinow
function(city_psgc){
  packages <- c("readr","dplyr")
  lapply(packages, require, character.only = TRUE)
  data.df <- read.csv("EpiNow/regional-summary/latest/rt.csv") %>% filter(region == city_psgc)
}

############
# EpiNow2 API
############
#* input city psgc
#* @param city_psgc Input city_psgc
#* @get /getepinowtwo
function(city_psgc,variableInput=""){
  packages <- c("readr","dplyr")
  lapply(packages, require, character.only = TRUE)
  data.df <- read.csv("EpiNow2/summary.csv") %>% filter(region == city_psgc)
  if(any(c("R","infections","reported_cases")==variableInput)) {
    data.df <- data.df %>% filter(variable==variableInput)
  }
  return(data.df)
}

############
# EpiNow2 Cases API
############
#* input psgc
#* @param psgc
#* @get /getcases
function(psgc){
  packages <- c("readr","dplyr")
  lapply(packages, require, character.only = TRUE)
  data.df <- read.csv("EpiNow2/cases.csv") %>% filter(region == psgc)
}


#* input city psgc
#* @param dateinput Input date
#* @get /getepinowbydate
function(dateinput,variableInput="R"){
  packages <- c("readr","dplyr")
  lapply(packages, require, character.only = TRUE)
  data.df <- read.csv("EpiNow2/summary.csv") %>% filter(date == dateinput & variable == variableInput)
}

############
# EpiEstim API
############
# source("EpiEstim/1_PreprocessData.R")
source("EpiEstim/2_EpiEstimAPI.R")
#* Get EpiEstim R(t)
#* @param psgc Psgc count
#* @param dateToProcess 'onsetCount', 'specimenCount', 'reportedCount'
#* @get /getRt
function(psgc, dateToProcess){
  return(generateRt(psgc, dateToProcess))
}

############
# ContactNetwork
############
# source("contactnetwork/2_CreateNetwork.R")

# #* Get networks
# #* @param psgcInput with leading zero (ie. Region 1 is 010000000)
# #* @get /RunNetworks
# function(psgcInput) {
#   tryCatch({
#     return(RunNetworks(psgcInput))
#   },
#   error = function(e) {
#     return(e)
#   })
# }

############
# SpatialAutoCorr API Suspect/Probable
############
# source('spatialautocorrelation/LISA-Suspect.R')
# #* Echo back the input
# #* @param city_psgc Input city_psgc
# #* @get /getclusterss
# function(city_psgc){
#   results <- compute_moran(city_psgc)
#   output <- results$data.frame %>% data.frame() %>%
#     select(Bgy_Code,
#            contains("cases"),
#            infection_rate,
#            classification,
#            population,
#            contains("AR"))
#   return(output)
# }

############
# SpatialAutoCorr API
############
source('spatialautocorrelation/function.R')
#* Echo back the input
#* @param city_psgc Input city_psgc
#* @get /getclusters
function(city_psgc){
  results <- lisa_compute_moran(city_psgc)
  output <- results$data.frame %>% data.frame() %>%
    select(Bgy_Code,
           contains("cases"),
           infection_rate,
           classification,
           population,
           contains("AR"))
  return(output)
}