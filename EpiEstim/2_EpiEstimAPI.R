library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(lubridate)
library(EpiEstim)

ncrList.l = read.csv("./EpiEstim/inputs/NCRPsgc.csv")[,1]

generateRt = function(psgc, dateToProcess) {
  countToProcess = dateToProcess
  fileName = sprintf("%s_IncidenceCounts.csv", psgc)
  disease_incidence_data = read.csv(sprintf("./EpiEstim/outputs/incidenceCounts/all/%s", fileName)) #Read file
  disease_incidence_data = disease_incidence_data[,c("date", countToProcess)]
  
  cumSum.l = cumsum(disease_incidence_data[,2])
  minIndex = min(which(cumSum.l >= 12))
  
  if(minIndex != 1) {
    disease_incidence_data[minIndex, 2] = cumSum.l[minIndex]
    disease_incidence_data = disease_incidence_data[-(0:(minIndex-1)),]
  }
  
  disease_incidence_data = disease_incidence_data[-((max(which(disease_incidence_data[,2] > 0))+1):nrow(disease_incidence_data)), ]
  
  incidenceDataToProcess.df = rbind(0,disease_incidence_data) #Provides buffer for first row (estimate_R function does not read first row for some reason)
  
  incidenceDataToProcess.df = as.data.frame(incidenceDataToProcess.df[,2])
  colnames(incidenceDataToProcess.df) = "V1"
  
  totalRows = nrow(incidenceDataToProcess.df)
  
  if (psgc %in% ncrList.l) {
    meanPriorVal = 4
  } else if (psgc == 1) {
    meanPriorVal = 4
  } else {
    meanPriorVal = 3
  }
  
  res_parametric_si = estimate_R(incidenceDataToProcess.df, 
                                 method="parametric_si",
                                 config = make_config(list(
                                   si_parametric_distr = "W",
                                   mean_prior = meanPriorVal,
                                   std_prior = 2,
                                   mean_si = 4.8, 
                                   std_si = 2.3))
  )
  
  median.df = as.data.frame(res_parametric_si$R$`Median(R)`)
  upperBound.df = as.data.frame(res_parametric_si$R$`Quantile.0.975(R)`)
  lowerBound.df = as.data.frame(res_parametric_si$R$`Quantile.0.025(R)`)
  
  
  toExport.df = cbind(disease_incidence_data[7:nrow(disease_incidence_data),],median.df, upperBound.df, lowerBound.df) #Bakit nga ba 7 ito
  colnames(toExport.df) = c("date", "incidence", "median", "upperBound", "lowerBound")
  
  return(toExport.df)
}
