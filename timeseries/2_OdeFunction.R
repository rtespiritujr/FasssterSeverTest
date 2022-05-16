library(deSolve)
library(GenSA)
library(Metrics)
library(dplyr)
library(plyr)
library(tidyr)
library(lubridate)
library(filesstrings)
library(tools)
library(taRifx)
library(ff)
library(stringr)
library(stats)

summaries = read.csv("timeseries/inputs/parameters.csv") #the file with the summaries

# confirmedCases.df = read.csv(unzip("linelist/ConfirmedCases.zip", files = "ConfirmedCases.csv"), stringsAsFactors = FALSE)
source("linelist/read_linelist.R")


fitParamToUse = "Cumulative"

if(fitParamToUse == "Cumulative"){
  confirmedCases.df <- get_linelist() %>% 
    dplyr::mutate_at(vars(contains('Date_') | contains("_Date")), as.Date)
  confirmedCases.df$nationalPSGC = "180000000"
  source("timeseries/3_ProcessActiveCases.R")
}else{
  confirmedCases.df <- compileActiveCases()
  maxDate = confirmedCases.df$linelist_date[1]
  source("timeseries/3_ProcessActiveCases.R")
}



getPsgcDetails = function(psgcInput) {
  dfToReturn = summaries[which(as.numeric(summaries$area) == as.numeric(psgcInput)),]
  return(dfToReturn)
}

# if (TRUE){
#   HcLevel = "0.179885123390889" # removed feature 2020-12-16
#   dateAdjustment = "FALSE"
#   fitParamToUse = "Cumulative"
#   interventionCompliance = "0"
#   interventionEndDate = "2025-01-01"
#   interventionPercentage = "0"
#   interventionStartDate = "2021-12-01"
#   lambdaDateInput = "2020-03-31;2020-04-30;2020-05-31;2020-06-30;2020-07-31;2020-08-31;2020-09-30;2020-10-31;2020-11-30;2020-12-31;2021-01-31;2021-02-28;2021-03-31"
#   lambdaValuesInput = "0.95;0.673528983392573;0.835474764041064;0.577962557194458;0.686097188946627;0.79217539628473;0.932410484193506;0.562601514431079;0.835517376459215;0.4997331749183;0.78314585479782;0.368276314295283;0.117472363566184"
#   projectionDuration = "600"
#   psgcInput = "130000000"
#   scaleLevel = "0"
#   startHcDate = "2021-03-01" # removed feature 2020-12-16
# 
#   output = RunProjections(lambdaDateInput,
#                           lambdaValuesInput,
#                           psgcInput,
#                           startHcDate,
#                           HcLevel,
#                           projectionDuration,
#                           scaleLevel,
#                           fitParamToUse,
#                           interventionStartDate,
#                           interventionEndDate,
#                           interventionPercentage,
#                           interventionCompliance,
#                           dateAdjustment)
#   oderes = output$OdeResult
# 
#   library(ggplot2)
#   library(reshape2)
#   if (fitParamToUse == "Cumulative"){
#     dfToGraph <- oderes %>%
#       dplyr::select(Date, C, CumulativeSumAdmitted)
#   } else {
#     dfToGraph <- oderes %>%
#       dplyr::select(Date, Q, ActiveCases)
#   }
# 
#   dfToGraph %>%
#     dplyr::filter(Date < Sys.Date()) %>%
#     melt(id = "Date") %>%
#     ggplot(aes(x = Date, y = value, color = variable)) +
#     geom_line(stat = "identity", position = "identity", size = 1.2)
# }

RunProjections = function(lambdaDateInput, lambdaValuesInput,
                          psgcInput, startHcDate, HcLevel, projectionDuration, 
                          scaleLevel, fitParamToUse, interventionStartDate,
                          interventionEndDate, interventionPercentage,
                          interventionCompliance, dateAdjustment) {
  dateAdjustmentSearchSpace = 10
  lambdaDateInput = as.Date(strsplit(lambdaDateInput, ";")[[1]])
  lambdaValuesInput = as.numeric(strsplit(lambdaValuesInput, ";")[[1]])
  
  interventionStartDate = as.Date(interventionStartDate)
  interventionEndDate = as.Date(interventionEndDate)
  interventionPercentage = as.numeric(interventionPercentage)
  interventionCompliance = as.numeric(interventionCompliance)
  
  startHcDate = as.Date(startHcDate)
  HcLevel = as.numeric(HcLevel)
  origProjectDuration = as.numeric(projectionDuration)
  projectionDuration = as.numeric(projectionDuration) + dateAdjustmentSearchSpace + 1
  scaleLevel = as.numeric(scaleLevel)
  dateAdjustment = as.logical(dateAdjustment)
  
  parGen_estimateLambda2 = function(){ #tinanggal yung engineer = TRUE
    parnames = c("tau", "beta", "c", "omega", "theta",  "r", "deltaA", "epsT", "mu")
    pargenDetails = psgcInput %>% as.numeric() %>% getPsgcDetails() %>% filter(FittedTo == fitParamToUse)
    
    deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
    
    result = c(
      pargenDetails['tau'], pargenDetails['beta'],# transmission
      pargenDetails['c'], pargenDetails['omega'], # transitions
      pargenDetails['theta'], pargenDetails['r'], # recoveries
      deltaS.l[1],
      pargenDetails['epsT'], 
      pargenDetails['mu']
    )
    
    names(result) = parnames
    
    return(list(parameters = result, deltaS.l = deltaS.l))
  }
  
  pargenDetails <- getPsgcDetails(psgcInput) %>% filter(FittedTo == fitParamToUse)
  
  deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  
  Covid19Model = function(t, state, parameters){
    with(as.list(c(state,parameters)),{
      if (t >= intervStartDate & t <= intervEndDate) {
        interventionEffect = (interventionPercentage * interventionCompliance)
      } else {
        interventionEffect = 0
      }
      
      if (t <= tail(equar.l, n = 1)) {
        reallam = (lambdaValuesInput[min(which(t <= equar.l))])
      } else {
        reallam = 0
      }
      
      startYear <- 2020
      startMonth <- 3
      
      computeYear <- format(first_date + t - 1, "%Y") %>% as.numeric()
      computeMonth <- format(first_date + t - 1, "%m") %>% as.numeric()
      
      deltaS_index <- 12*computeYear + computeMonth - (12*startYear + startMonth) + 1
      
      deltaS <- ifelse(deltaS_index >= length(deltaS.l),
                       deltaS.l[length(deltaS.l)],
                       deltaS.l[deltaS_index])
      
      # if(t >= setdate){
      #   deltaS = setdelt
      # }
      
      deltaA <- deltaS
      #put if kapag need ng 1.5*beta multiplier
      dS = A - (1-reallam)*(1-interventionEffect)*beta*S*(IA + IS)/(S+E+IA+IS+Q+R) - mu*S
      dE = (1-reallam)*(1-interventionEffect)*beta*S*(IA + IS)/(S+E+IA+IS+Q+R) - (mu + 1/tau)*E 
      dIA = c*E/tau - (mu + omega + deltaA + theta)*IA
      dIS = (1-c)*E/tau + omega*IA - (mu + epsT + deltaS)*IS # changed eps I to eps T
      dQ = deltaA*IA + deltaS*IS - (mu + epsT + r)*Q
      dC = deltaA*IA + deltaS*IS
      dD = epsT*IS + epsT*C
      dR = theta*IA + r*Q - mu*R
      
      list(c(dS, dE, dIA, dIS, dQ, dC, dD, dR))
    })
  }
  
  scen = which(as.numeric(summaries$area) == as.numeric(psgcInput) & summaries$FittedTo == fitParamToUse)
  
  first_date = as.Date(as.character(summaries[scen, 'first_date']))
  
  inputs = summaries[scen, ]
  inputs = suppressWarnings(as.numeric(inputs))
  names(inputs) = colnames(summaries)[1:ncol(summaries)]
  
  N = inputs['pop']
  A = N*0.020177/365
  
  equar.l = as.numeric((lambdaDateInput - first_date + 1))
  
  pars = parGen_estimateLambda2()$parameters
  
  setdate = as.numeric(startHcDate - first_date) + 1
  setdelt = HcLevel
  
  intervStartDate = as.numeric(interventionStartDate - first_date) + 1
  intervEndDate = as.numeric(interventionEndDate - first_date) + 1
  
  
  istates = c(S = as.numeric(N), 
              E = as.numeric(inputs['E0']), 
              IA = as.numeric(inputs['IA0']), 
              IS = as.numeric(inputs['IS0']), 
              Q = as.numeric(inputs['C0']), 
              C = as.numeric(inputs['C0']), 
              D = 0, 
              R = 0)
  
  projtime = seq(1, projectionDuration, by = 1)
  check = ode(y=istates,time=projtime,func=Covid19Model,parms=pars)
  
  
  check = as.data.frame(check[,2:ncol(check)])
  
  dailyDeaths = c(0,diff(check$D))
  dailyRecovered =  c(0,diff(check$R))
  dailyCritical = check$Q*.06
  dailySevere = check$Q*.14
  dailyMild = check$Q*.80
  
  cumulativeCritical = check$C*.06
  cumulativeSevere = check$C*.14
  cumulativeMild = check$C*.80
  
  totalInfectious = check$IA + check$IS
  
  datesSequence = data.frame(Date = seq(first_date, 
                                        first_date + (nrow(check)-1), 
                                        1,))
  
  checkReturn = cbind(format(datesSequence, "%Y-%m-%d"), check, dailyDeaths, dailyRecovered,
                      dailyCritical, dailySevere, dailyMild,
                      cumulativeCritical, cumulativeSevere, cumulativeMild,
                      totalInfectious)
  
  
  
  
  
  
  # # Retain the else statement after all fit to cumulative are based on report date
  # if (deltaS.l[1] == 0.111028684 & fitParamToUse == "Cumulative") {
  #   actualIncidenceCounts = getActiveCases(psgcInput, "Active")
  # } else{
  #   actualIncidenceCounts = getActiveCases(psgcInput, fitParamToUse)
  # }
  
  actualIncidenceCounts = getActiveCases(psgcInput, fitParamToUse)
  
  
  if(dateAdjustment){
    maxDateIndex = which(as.character(checkReturn$Date) == maxDate)
    
    #Movement to match latest data and projections
    if (fitParamToUse == "Cumulative") {
      maxCumulative = tail(actualIncidenceCounts$CumulativeSumAdmitted, n=1)
      rangeOfCheck = checkReturn$C[(maxDateIndex-dateAdjustmentSearchSpace):(maxDateIndex+dateAdjustmentSearchSpace)]
      dateMovement = which(abs(rangeOfCheck-maxCumulative) == min(abs(rangeOfCheck-maxCumulative))) - (dateAdjustmentSearchSpace+1)
    } else {
      maxActive = tail(actualIncidenceCounts$ActiveCases, n=1)
      rangeOfCheck = checkReturn$Q[(maxDateIndex-dateAdjustmentSearchSpace):(maxDateIndex+dateAdjustmentSearchSpace)]
      dateMovement = which(abs(rangeOfCheck-maxActive) == min(abs(rangeOfCheck-maxActive))) - (dateAdjustmentSearchSpace+1)
    }
    
    datesSequence = data.frame(Date = seq(first_date - dateMovement, 
                                          first_date + (nrow(check)-1) - dateMovement, 
                                          1,))
    
    checkReturn = cbind(format(datesSequence, "%Y-%m-%d"),checkReturn[,c(2:(ncol(checkReturn)))])
  } else {
    dateMovement = 0
  }
  
  checkReturn = checkReturn %>% 
    dplyr::mutate(Date = as.character(Date))
  checkReturn = merge(x = checkReturn, y = actualIncidenceCounts, by = "Date", all.x = TRUE)
  
  if (scaleLevel != 0) {
    checkReturn = as.data.frame((checkReturn[,2:ncol(checkReturn)]/(inputs['pop']/inputs['popMultiplier']))*scaleLevel)
    checkReturn = cbind(datesSequence,checkReturn)
  }
  checkReturn$Date = as.Date(as.character(checkReturn$Date))
  psgcDetails = getPsgcDetails(psgcInput)
  firstDate = as.Date(psgcDetails[which(psgcDetails$FittedTo == fitParamToUse), "first_date"])
  lastDate = firstDate + origProjectDuration - 1
  checkReturn = checkReturn[which(checkReturn$Date >= firstDate & checkReturn$Date <= lastDate),]
  
  
  ########### OTHER DETAILS ###########
  
  peakIndex = which(checkReturn$Q == max(checkReturn$Q))
  peakDate = checkReturn[peakIndex,"Date"]
  peakConfirmed = checkReturn[peakIndex,"Q"]
  peakMortality = checkReturn[peakIndex, "dailyDeaths"]
  peakSevere = peakConfirmed*.14
  peakCritical = peakConfirmed*.06
  
  epidemicEndIndex = suppressWarnings(min(which(
    checkReturn$IA < 1 &
      checkReturn$IS < 1 &
      checkReturn$Q < 1
  )))
  
  epidemicEndDate = checkReturn[epidemicEndIndex,"Date"]
  
  otherInfo = data.frame(peakDate = as.character(peakDate),
                         epidemicEndDate = as.character(epidemicEndDate), 
                         peakConfirmed = peakConfirmed,
                         peakMortality = peakMortality,
                         peakSevere = peakSevere,
                         peakCritical = peakCritical,
                         dateAdjustment = dateMovement)
  
  ##############
  
  returnValues = list(OdeResult = checkReturn, OtherDetails = otherInfo)
  return(returnValues)
}
