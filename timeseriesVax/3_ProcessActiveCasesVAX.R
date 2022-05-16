
fitParamToUse = "Cumulative"

if(fitParamToUse == "Cumulative"){
  confirmedCases_SEIR.df <- get_linelist() %>% 
    dplyr::mutate_at(vars(contains('Date_') | contains("_Date")), as.Date)
  confirmedCases_SEIR.df$nationalPSGC = "180000000"
  
  confirmedCases.df <- confirmedCases_SEIR.df %>% 
    dplyr::mutate_at(vars(contains('Date_') | contains("_Date")), as.Date)%>%
    dplyr::mutate(Report_Date = as.Date(Report_Date))%>%
    dplyr::mutate(Report_Date = ifelse(Report_Date < as.Date("2021-07-01"), as.Date("2021-07-01"), Report_Date))
  confirmedCases.df <- confirmedCases.df %>% 
    dplyr::mutate(Report_Date = as.Date(Report_Date, origin="1970-01-01"))
  confirmedCases.df$nationalPSGC = "180000000"
  
  
  confirmedCases_SVEIRS.df <- confirmedCases_SEIR.df %>% 
    dplyr::mutate_at(vars(contains('Date_') | contains("_Date")), as.Date)%>%
    dplyr::mutate(Report_Date = as.Date(Report_Date))%>%
    dplyr::mutate(Report_Date = ifelse(Report_Date < as.Date("2021-12-25"), as.Date("2021-12-25"), Report_Date))
  confirmedCases_SVEIRS.df <- confirmedCases_SVEIRS.df %>% 
    dplyr::mutate(Report_Date = as.Date(Report_Date, origin="1970-01-01"))
  confirmedCases_SVEIRS.df$nationalPSGC = "180000000"
  
} else{ #get active instead
  confirmedCases_SEIR.df <- writeActiveCases()
  maxDate = confirmedCases_SEIR.df$linelist_date[1]
  
  confirmedCases.df <- confirmedCases_SEIR.df %>% 
    dplyr::filter(linelist_date >= as.Date("2021-07-01"))
  maxDate = confirmedCases.df$linelist_date[1]
  
  confirmedCases_SVEIRS.df <- confirmedCases_SEIR.df %>% 
    dplyr::filter(linelist_date >= as.Date("2021-12-25"))
  maxDate_SVEIR = confirmedCases_SVEIRS.df$linelist_date[1]

}

compileActiveCases <- function(){
   USER = "fasssterDBAdmin"  #"rserver"
   PASS = "Sh1n30sAdm1n"  #"Rpa$$w0rd4321"
   HOST = "202.90.159.22" #fassster.ehealth.ph"
   DB = "fassster"
   COLLECTION = "active_cases"
   URI = sprintf("mongodb://%s:%s@%s/%s", USER, PASS, HOST, "admin")

#   #mongodb://fasssterDBAdmin:Sh1n30sAdm1n@202.90.159.22/fassster
#   #Auth DB = admin

#   # Connect to the database and the desired collection as root:
   db <- mongo(
     collection = COLLECTION, 
     db = DB, 
     url = URI)

   aggregate <- data.frame(db$aggregate())
   aggregate$X_id <- NULL
   aggregate$Unnamed..0 <- NULL
   aggregate_active <-  aggregate %>% mutate(cityPSGC = coalesce(municipalitycitypsgc,municipalitycitypsgc))%>%
     select(linelist_date,
            regionPSGC = regionpsgc, 
            provincePSGC = provincepsgc,
            cityPSGC,
            active_cases)
   aggregate_active <- aggregate_active[order(aggregate_active$linelist_date),]
   aggregate_active$linelist_date <- as.Date(factor(aggregate_active$linelist_date), format='%Y-%m-%d')
   active_cases.df <- cbind(aggregate_active[1],lapply(aggregate_active[2:5], function(x) as.numeric(as.character(x))))
   return(active_cases.df)
}

writeActiveCases <- function(){
  activedb_changelog <- file_info("linelist/ActiveCases/ActiveCases.rds")
  if ( difftime(Sys.time(), activedb_changelog$modification_time, units = "days") >= 1 ){
    
    tryCatch({
      active_cases.df <- compileActiveCases()
      saveRDS(active_cases.df, file = "linelist/ActiveCases/ActiveCases.rds")},
      error=function(e) {
        print("MongoDB for Active Cases is not working ")
        return(e)},
      finally={
        print("Will use local RDS file")
        active_cases.df <- readActiveCases()})    
  } 

return(readActiveCases())
}

readActiveCases <- function(){
  active_cases.df <- readRDS("linelist/ActiveCases/ActiveCases.rds") 
  return(active_cases.df)
}
getActiveCasesFilter = function(psgcInput, fitParamToUse, filterPSGCs) {
  psgc <- psgcInput %>% as.numeric()
  
  hucPSGC <- read.csv("timeseriesVax/inputs/huc_icc.csv") %>%
    dplyr::filter(code < 130000000 | code >= 140000000) %>% # removes PSGCs that start with 13 (NCR)
    dplyr::select(code)
  
  if (fitParamToUse == 'Active'){
    confirmedCasesSubset.df <- writeActiveCases() %>%
      filter(grepl(paste(c("^",paste(filterPSGCs, collapse="|^")), collapse = ''), cityPSGC))
    confirmedCasesSubset.df$nationalPSGC = 180000000
    confirmedCasesSubset.df <- confirmedCasesSubset.df %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc)
  } else{
    confirmedCasesSubset.df <- confirmedCases.df %>%
      filter(grepl(paste(c("^",paste(filterPSGCs, collapse="|^")), collapse = ''), cityPSGC)) %>%
      dplyr::mutate_at(vars(contains("PSGC")), as.integer) %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR districts
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc) %>%
      dplyr::mutate(dateAdmitted = (Date_Admitted), #imputed_Date_Admitted
                    dateResults = (Result_Date),
                    dateRecovered = (Date_Recovered), #imputed_Date_Recovered
                    dateDied = (Date_Died),
                    dateReport = (Report_Date),
                    dateOnset = (Date_Onset))#imputed_Date_Onset
  }
  
  
  tOfTStartDate <- as.Date("2021-07-01")
  
  if (fitParamToUse == "Active"){
    compiledTable.df <- confirmedCasesSubset.df %>% 
      dplyr::group_by(linelist_date) %>% 
      dplyr::summarize(active_cases = sum(active_cases)) %>% 
      as.data.frame %>% 
      dplyr::arrange(linelist_date) %>% 
      dplyr::filter(linelist_date>= as.Date('2020-03-01')) %>% 
      dplyr::transmute(Date = as.character(linelist_date),
                       ActiveCases = active_cases)
  } else{
    tOfTEndDate <- as.Date(max(confirmedCasesSubset.df$dateReport, na.rm = T))
    firstCaseDate <- as.Date(confirmedCasesSubset.df$dateReport) %>% min(na.rm = T)
    
    if (firstCaseDate < as.Date("2021-07-01")){
    firstCaseDate <- as.Date("2021-07-01")
    }
    tOfTNdays <- interval(tOfTStartDate, tOfTEndDate)/days(1)
    
    tOfTDates.list <- c(as.Date(tOfTStartDate + days(0:tOfTNdays), format='%Y-%m-%d')) %>% as.character()
    
    tNumberAdmitted.list <- confirmedCasesSubset.df$dateAdmitted %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberAdmitted.list <- data.frame(tNumberAdmitted.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberReported.list <- confirmedCasesSubset.df$dateReport %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberReported.list <- data.frame(tNumberReported.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberRecovered.list <- confirmedCasesSubset.df$dateRecovered %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberRecovered.list <- data.frame(tNumberRecovered.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberOnset.list <- confirmedCasesSubset.df$dateOnset %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberOnset.list <- data.frame(tNumberOnset.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberDied <- confirmedCasesSubset.df$dateDied %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberDied <- data.frame(tNumberDied) %>% filter(. %in% tOfTDates.list)
    
    compiledTable.df <- data.frame(Date = tOfTDates.list,
                                   CumulReport = cumsum(tNumberReported.list$Freq),
                                   CumulAdmitted = cumsum(tNumberAdmitted.list$Freq),
                                   CumulRecovered = cumsum(tNumberRecovered.list$Freq),
                                   CumulDied = cumsum(tNumberDied$Freq)) %>% 
      dplyr::filter(Date >= firstCaseDate) %>% 
      dplyr::transmute(Date = as.character(Date),
                       ActiveCases = case_when(fitParamToUse == "Active" ~ CumulAdmitted - CumulRecovered - CumulDied,
                                               TRUE ~ CumulReport - CumulRecovered - CumulDied),
                       CumulativeSumAdmitted = case_when(fitParamToUse == "Active" ~ CumulAdmitted,
                                                         TRUE ~ CumulReport))
  }
  
  return(compiledTable.df)
}

getActiveCases = function(psgcInput, fitParamToUse) {
  psgc <- psgcInput %>% as.numeric()
  
  hucPSGC <- read.csv("timeseriesVax/inputs/huc_icc.csv") %>%
    dplyr::filter(code < 130000000 | code >= 140000000) %>% # removes PSGCs that start with 13 (NCR)
    dplyr::select(code)
  
  if (fitParamToUse == 'Active'){
    confirmedCasesSubset.df <- writeActiveCases()
    confirmedCasesSubset.df$nationalPSGC = 180000000
    confirmedCasesSubset.df <- confirmedCasesSubset.df %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc)
  } else{
    confirmedCasesSubset.df <- confirmedCases.df %>%
      dplyr::mutate_at(vars(contains("PSGC")), as.integer) %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR districts
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc) %>%
      dplyr::mutate(dateAdmitted = (Date_Admitted), #imputed_Date_Admitted
                    dateResults = (Result_Date),
                    dateRecovered = (Date_Recovered), #imputed_Date_Recovered
                    dateDied = (Date_Died),
                    dateReport = (Report_Date),
                    dateOnset = (Date_Onset))#imputed_Date_Onset
  }
  
  
  tOfTStartDate <- as.Date("2021-07-01")
  
  if (fitParamToUse == "Active"){
    compiledTable.df <- confirmedCasesSubset.df %>% 
      dplyr::group_by(linelist_date) %>% 
      dplyr::summarize(active_cases = sum(active_cases)) %>% 
      as.data.frame %>% 
      dplyr::arrange(linelist_date) %>% 
      dplyr::filter(linelist_date>= as.Date('2020-03-01')) %>% 
      dplyr::transmute(Date = as.character(linelist_date),
                       ActiveCases = active_cases)
  } else{
    tOfTEndDate <- as.Date(max(confirmedCasesSubset.df$dateReport, na.rm = T))
    firstCaseDate <- as.Date(confirmedCasesSubset.df$dateReport) %>% min(na.rm = T)
    
    if (firstCaseDate < as.Date("2021-07-01")){
      firstCaseDate <- as.Date("2021-07-01")
    }
    tOfTNdays <- interval(tOfTStartDate, tOfTEndDate)/days(1)
    
    tOfTDates.list <- c(as.Date(tOfTStartDate + days(0:tOfTNdays), format='%Y-%m-%d')) %>% as.character()
    
    tNumberAdmitted.list <- confirmedCasesSubset.df$dateAdmitted %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberAdmitted.list <- data.frame(tNumberAdmitted.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberReported.list <- confirmedCasesSubset.df$dateReport %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberReported.list <- data.frame(tNumberReported.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberRecovered.list <- confirmedCasesSubset.df$dateRecovered %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberRecovered.list <- data.frame(tNumberRecovered.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberOnset.list <- confirmedCasesSubset.df$dateOnset %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberOnset.list <- data.frame(tNumberOnset.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberDied <- confirmedCasesSubset.df$dateDied %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberDied <- data.frame(tNumberDied) %>% filter(. %in% tOfTDates.list)
    
    compiledTable.df <- data.frame(Date = tOfTDates.list,
                                   CumulReport = cumsum(tNumberReported.list$Freq),
                                   CumulAdmitted = cumsum(tNumberAdmitted.list$Freq),
                                   CumulRecovered = cumsum(tNumberRecovered.list$Freq),
                                   CumulDied = cumsum(tNumberDied$Freq)) %>% 
      dplyr::filter(Date >= firstCaseDate) %>% 
      dplyr::transmute(Date = as.character(Date),
                       ActiveCases = case_when(fitParamToUse == "Active" ~ CumulAdmitted - CumulRecovered - CumulDied,
                                               TRUE ~ CumulReport - CumulRecovered - CumulDied),
                       CumulativeSumAdmitted = case_when(fitParamToUse == "Active" ~ CumulAdmitted,
                                                         TRUE ~ CumulReport))
  }
  
  return(compiledTable.df)
}

getActiveCasesFilter_SVEIRS = function(psgcInput, fitParamToUse, filterPSGCs) {
  psgc <- psgcInput %>% as.numeric()
  
  hucPSGC <- read.csv("timeseriesVax/inputs/huc_icc.csv") %>%
    dplyr::filter(code < 130000000 | code >= 140000000) %>% # removes PSGCs that start with 13 (NCR)
    dplyr::select(code)
  
  if (fitParamToUse == 'Active'){
    confirmedCasesSubset.df <- writeActiveCases() %>%
      filter(grepl(paste(c("^",paste(filterPSGCs, collapse="|^")), collapse = ''), cityPSGC))
    confirmedCasesSubset.df$nationalPSGC = 180000000
    confirmedCasesSubset.df <- confirmedCasesSubset.df %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc)
  } else{
    confirmedCasesSubset.df <- confirmedCases_SVEIRS.df %>%
      filter(grepl(paste(c("^",paste(filterPSGCs, collapse="|^")), collapse = ''), cityPSGC)) %>%
      dplyr::mutate_at(vars(contains("PSGC")), as.integer) %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR districts
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc) %>%
      dplyr::mutate(dateAdmitted = (Date_Admitted), #imputed_Date_Admitted
                    dateResults = (Result_Date),
                    dateRecovered = (Date_Recovered), #imputed_Date_Recovered
                    dateDied = (Date_Died),
                    dateReport = (Report_Date),
                    dateOnset = (Date_Onset))#imputed_Date_Onset
  }
  
  
  tOfTStartDate <- as.Date("2021-12-25")
  
  if (fitParamToUse == "Active"){
    compiledTable.df <- confirmedCasesSubset.df %>% 
      dplyr::group_by(linelist_date) %>% 
      dplyr::summarize(active_cases = sum(active_cases)) %>% 
      as.data.frame %>% 
      dplyr::arrange(linelist_date) %>% 
      dplyr::filter(linelist_date>= as.Date('2020-03-01')) %>% 
      dplyr::transmute(Date = as.character(linelist_date),
                       ActiveCases = active_cases)
  } else{
    tOfTEndDate <- as.Date(max(confirmedCasesSubset.df$dateReport, na.rm = T))
    firstCaseDate <- as.Date(confirmedCasesSubset.df$dateReport) %>% min(na.rm = T)
    
    if (firstCaseDate < as.Date("2021-12-25")){
    firstCaseDate <- as.Date("2021-12-25")
    }
    tOfTNdays <- interval(tOfTStartDate, tOfTEndDate)/days(1)
    
    tOfTDates.list <- c(as.Date(tOfTStartDate + days(0:tOfTNdays), format='%Y-%m-%d')) %>% as.character()
    
    tNumberAdmitted.list <- confirmedCasesSubset.df$dateAdmitted %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberAdmitted.list <- data.frame(tNumberAdmitted.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberReported.list <- confirmedCasesSubset.df$dateReport %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberReported.list <- data.frame(tNumberReported.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberRecovered.list <- confirmedCasesSubset.df$dateRecovered %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberRecovered.list <- data.frame(tNumberRecovered.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberOnset.list <- confirmedCasesSubset.df$dateOnset %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberOnset.list <- data.frame(tNumberOnset.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberDied <- confirmedCasesSubset.df$dateDied %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberDied <- data.frame(tNumberDied) %>% filter(. %in% tOfTDates.list)
    
    compiledTable.df <- data.frame(Date = tOfTDates.list,
                                   CumulReport = cumsum(tNumberReported.list$Freq),
                                   CumulAdmitted = cumsum(tNumberAdmitted.list$Freq),
                                   CumulRecovered = cumsum(tNumberRecovered.list$Freq),
                                   CumulDied = cumsum(tNumberDied$Freq)) %>% 
      dplyr::filter(Date >= firstCaseDate) %>% 
      dplyr::transmute(Date = as.character(Date),
                       ActiveCases = case_when(fitParamToUse == "Active" ~ CumulAdmitted - CumulRecovered - CumulDied,
                                               TRUE ~ CumulReport - CumulRecovered - CumulDied),
                       CumulativeSumAdmitted = case_when(fitParamToUse == "Active" ~ CumulAdmitted,
                                                         TRUE ~ CumulReport))
  }
  
  return(compiledTable.df)
}

getActiveCases_SVEIRS = function(psgcInput, fitParamToUse) {
  psgc <- psgcInput %>% as.numeric()
  
  hucPSGC <- read.csv("timeseriesVax/inputs/huc_icc.csv") %>%
    dplyr::filter(code < 130000000 | code >= 140000000) %>% # removes PSGCs that start with 13 (NCR)
    dplyr::select(code)
  
  if (fitParamToUse == 'Active'){
    confirmedCasesSubset.df <- writeActiveCases()
    confirmedCasesSubset.df$nationalPSGC = 180000000
    confirmedCasesSubset.df <- confirmedCasesSubset.df %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc)
  } else{
    confirmedCasesSubset.df <- confirmedCases_SVEIRS.df %>%
      dplyr::mutate_at(vars(contains("PSGC")), as.integer) %>% 
      dplyr::mutate(provincePSGC = ifelse((cityPSGC %in% hucPSGC$code & regionPSGC != 130000000), NA, provincePSGC)) %>%  # remove HUC from province except for NCR districts
      dplyr::filter(nationalPSGC == psgc | regionPSGC == psgc | provincePSGC == psgc | cityPSGC == psgc) %>%
      dplyr::mutate(dateAdmitted = (Date_Admitted), #imputed_Date_Admitted
                    dateResults = (Result_Date),
                    dateRecovered = (Date_Recovered), #imputed_Date_Recovered
                    dateDied = (Date_Died),
                    dateReport = (Report_Date),
                    dateOnset = (Date_Onset))#imputed_Date_Onset
  }
  
  
  tOfTStartDate <- as.Date("2021-12-25")
  
  if (fitParamToUse == "Active"){
    compiledTable.df <- confirmedCasesSubset.df %>% 
      dplyr::group_by(linelist_date) %>% 
      dplyr::summarize(active_cases = sum(active_cases)) %>% 
      as.data.frame %>% 
      dplyr::arrange(linelist_date) %>% 
      dplyr::filter(linelist_date>= as.Date('2020-03-01')) %>% 
      dplyr::transmute(Date = as.character(linelist_date),
                       ActiveCases = active_cases)
  } else{
    tOfTEndDate <- as.Date(max(confirmedCasesSubset.df$dateReport, na.rm = T))
    firstCaseDate <- as.Date(confirmedCasesSubset.df$dateReport) %>% min(na.rm = T)
    
    if (firstCaseDate < as.Date("2021-12-25")){
      firstCaseDate <- as.Date("2021-12-25")
    }
    tOfTNdays <- interval(tOfTStartDate, tOfTEndDate)/days(1)
    
    tOfTDates.list <- c(as.Date(tOfTStartDate + days(0:tOfTNdays), format='%Y-%m-%d')) %>% as.character()
    
    tNumberAdmitted.list <- confirmedCasesSubset.df$dateAdmitted %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberAdmitted.list <- data.frame(tNumberAdmitted.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberReported.list <- confirmedCasesSubset.df$dateReport %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberReported.list <- data.frame(tNumberReported.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberRecovered.list <- confirmedCasesSubset.df$dateRecovered %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberRecovered.list <- data.frame(tNumberRecovered.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberOnset.list <- confirmedCasesSubset.df$dateOnset %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberOnset.list <- data.frame(tNumberOnset.list) %>% filter(. %in% tOfTDates.list)
    
    tNumberDied <- confirmedCasesSubset.df$dateDied %>%
      c(tOfTDates.list) %>%
      table() - 1
    tNumberDied <- data.frame(tNumberDied) %>% filter(. %in% tOfTDates.list)
    
    compiledTable.df <- data.frame(Date = tOfTDates.list,
                                   CumulReport = cumsum(tNumberReported.list$Freq),
                                   CumulAdmitted = cumsum(tNumberAdmitted.list$Freq),
                                   CumulRecovered = cumsum(tNumberRecovered.list$Freq),
                                   CumulDied = cumsum(tNumberDied$Freq)) %>% 
      dplyr::filter(Date >= firstCaseDate) %>% 
      dplyr::transmute(Date = as.character(Date),
                       ActiveCases = case_when(fitParamToUse == "Active" ~ CumulAdmitted - CumulRecovered - CumulDied,
                                               TRUE ~ CumulReport - CumulRecovered - CumulDied),
                       CumulativeSumAdmitted = case_when(fitParamToUse == "Active" ~ CumulAdmitted,
                                                         TRUE ~ CumulReport))
  }
  
  return(compiledTable.df)
}
