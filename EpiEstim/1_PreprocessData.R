library(dplyr)
library(plyr)
library(tidyr)
library(stringr)
library(lubridate)
library(EpiEstim)

# outputFolderIncidence = 'EpiEstim/outputs/incidenceCounts'
# filesToRemove = list.files(outputFolderIncidence, include.dirs = F, full.names = T, recursive = T)
# file.remove(filesToRemove)

huc_icc <- read.csv("./EpiEstim/inputs/huc_icc.csv")

# data.df = read.csv(unzip("linelist/ConfirmedCases.zip", files = "ConfirmedCases.csv"), sep = ',', stringsAsFactors = FALSE)
source("linelist/read_linelist.R")
data.df <- get_linelist()

## Clean Data Frame
data.df <- data.df %>%  
  filter(Case_Number != "") %>% 
  dplyr::mutate_at(vars(contains("PSGC")), as.double) %>% 
  dplyr::mutate(cityPSGC2 = case_when(provincePSGC == 133900000 ~ 133900000,
                                      TRUE ~ cityPSGC)) %>% 
  dplyr::mutate(provincePSGC = ifelse((cityPSGC2 %in% huc_icc$code & regionPSGC != 130000000), NA, provincePSGC)) # remove HUC from province except for NCR districts

data.df$regionPSGC = as.numeric(as.character(data.df$regionPSGC))
data.df$cityPSGC = as.numeric(as.character(data.df$cityPSGC))
data.df$provincePSGC = as.numeric(as.character(data.df$provincePSGC))

geographicalLevel.l = c("region", "province", "muncity", "ph")
GeographicalLevel = "muncity"
for (GeographicalLevel in geographicalLevel.l) {
  print(sprintf("Preprocessing data for %s!", GeographicalLevel))
  psgc.l = c()
  
  if (GeographicalLevel == "region") {
    psgc.l = as.vector(na.omit(unique(data.df$regionPSGC)))
  } else if (GeographicalLevel == "province") {
    psgc.l = as.vector(na.omit(unique(data.df$provincePSGC)))
  } else if (GeographicalLevel == "muncity") {
    psgc.l = as.vector(na.omit(unique(data.df$cityPSGC)))
    psgc.l = psgc.l[which(psgc.l != "133900000")] #removes City of Manila PSGC in list, make sure it's still part of provincePSGC after removing HUC
  } else {
    psgc.l = "1"
  }
  

  #psgc.v = 130000000
  for (psgc.v in psgc.l) {
    psgc = psgc.v

    if (GeographicalLevel == "region") {
      tOfT.df = data.df %>% filter(data.df$regionPSGC == psgc & (tolower(Repatriate) != "yes" | is.na(Repatriate)))
    } else if (GeographicalLevel == "province") {
      tOfT.df = data.df %>% filter(data.df$provincePSGC == psgc & (tolower(Repatriate) != "yes" | is.na(Repatriate)))
    } else if (GeographicalLevel == "muncity") {
      tOfT.df = data.df %>% filter(data.df$cityPSGC == psgc& (tolower(Repatriate) != "yes" | is.na(Repatriate)))
    } else {
      tOfT.df = data.df %>% filter(tolower(Repatriate) != "yes" | is.na(Repatriate))
    }
    # DOH_Imputed_Date_Onset
    # imputed_Date_Onset
    tOfT.df <- tOfT.df %>%
      mutate(dateReport = as.Date(Report_Date, format = '%Y-%m-%d'),
             dateOnset = as.Date(imputed_Date_Onset, format = '%Y-%m-%d'),
             dateSpecimen = as.Date(imputed_Date_Specimen, format = '%Y-%m-%d'))
    
    
    #######START TO GET COUNTS ######
    tOfTStartDate <- mdy("03/01/2020")
    tOfTEndDate <- Sys.Date()
    tOfTNdays <- interval(tOfTStartDate,tOfTEndDate)/days(1)
    tOfTDates.list <- c(as.Date(tOfTStartDate + days(0:tOfTNdays), format='%Y-%m-%d'))
    
    tNumberReported.list <- c()
    tNumberOnset.list <- c()
    tNumberSpecimen.list <- c()

    tCumulReported.list <- c()
    tCumulOnset.list <- c()
    tCumulSpecimen.list <- c()
    
    for (j in 1:length(tOfTDates.list)){
      reportedList = tOfT.df$dateReport == (tOfTDates.list[j])
      tNumberReported.list[j] <- sum(reportedList[!is.na(reportedList)])
      onsetList = tOfT.df$dateOnset == (tOfTDates.list[j])
      tNumberOnset.list[j] <- sum(onsetList[!is.na(onsetList)])
      specimenList = tOfT.df$dateSpecimen == (tOfTDates.list[j])
      tNumberSpecimen.list[j] <- sum(specimenList[!is.na(specimenList)])
      
      reportedCumulList = tOfT.df$dateReport <= (tOfTDates.list[j])
      tCumulReported.list[j] <- sum(reportedCumulList[!is.na(reportedCumulList)])
      onsetCumulList = tOfT.df$dateOnset <= (tOfTDates.list[j])
      tCumulOnset.list[j] <- sum(onsetCumulList[!is.na(onsetCumulList)])
      specimenCumulList = tOfT.df$dateSpecimen <= (tOfTDates.list[j])
      tCumulSpecimen.list[j] <- sum(specimenCumulList[!is.na(specimenCumulList)])
    }
    
    finalTOfT.df <- data.frame(t = tOfTDates.list)
    finalTOfT.df <- cbind(finalTOfT.df, tNumberReported.list, tNumberOnset.list, tNumberSpecimen.list, tCumulReported.list, tCumulOnset.list, tCumulSpecimen.list)
    
    toExport.df = finalTOfT.df[,c("t", "tNumberReported.list", "tNumberOnset.list", "tNumberSpecimen.list")]
    colnames(toExport.df) = c("date", "reportedCount", "onsetCount", "specimenCount")
    
    write.csv(toExport.df, sprintf("./EpiEstim/outputs/incidenceCounts/all/%s_IncidenceCounts.csv", format(psgc, scientific = FALSE)))
  }
}


