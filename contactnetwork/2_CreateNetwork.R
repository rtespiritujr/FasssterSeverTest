library(epicontacts)
library(igraph)
library(outbreaks)
library(visNetwork)
library(lubridate)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
 
listfiles <- data.frame(list.files(path = "./contactnetwork/inputs", pattern=glob2rx("*_\\d\\d\\d\\d-\\d\\d-\\d\\d.csv")))
colnames(listfiles)[1] <- "filenames"
RecentDate <- substr(listfiles$filenames[1],17,26)

CaseList  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/OneModeCaseList_%s.csv", RecentDate)), stringsAsFactors = FALSE)
ContactList  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/OneModeContactList_%s.csv", RecentDate)), stringsAsFactors = FALSE)
TwoModeCaseList  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/TwoModeCaseList_%s.csv", RecentDate)), stringsAsFactors = FALSE)
TwoModeContactList  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/TwoModeContactList_%s.csv", RecentDate)), stringsAsFactors = FALSE)
TwoModeContactList_W  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/TwoModeContactList_Workplace_%s.csv", RecentDate)), stringsAsFactors = FALSE)
TwoModeContactList_H  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/TwoModeContactList_Household_%s.csv", RecentDate)), stringsAsFactors = FALSE)
TwoModeContactList_N  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/TwoModeContactList_Nosocomial_%s.csv", RecentDate)), stringsAsFactors = FALSE)
TwoModeContactList_S  <- data.frame(read.csv(sprintf("./contactnetwork/inputs/TwoModeContactList_Social_%s.csv", RecentDate)), stringsAsFactors = FALSE)

RunNetworks = function(psgcInput, timeInput = "14 Days") {
  ## Filter by timeInput
  if(timeInput == "7 Days"){
    CaseList <- filter(CaseList, as.Date(citizen_report_date, format='%m/%d/%Y') >= Sys.Date()-7 )
    ContactList <- filter(ContactList, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-7  )
    TwoModeCaseList <- filter(TwoModeCaseList, as.Date(citizen_report_date, format='%m/%d/%Y') >= Sys.Date()-7)
    TwoModeContactList <-filter(TwoModeContactList, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-7 )
    TwoModeContactList_W <- filter(TwoModeContactList_W, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-7 )
    TwoModeContactList_H <- filter(TwoModeContactList_H, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-7 )
    TwoModeContactList_N <- filter(TwoModeContactList_N, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-7 )
    TwoModeContactList_S <- filter(TwoModeContactList_S, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-7 )
  } else {
    CaseList <- filter(CaseList, as.Date(citizen_report_date, format='%m/%d/%Y') >= Sys.Date()-365 )
    ContactList <- filter(ContactList, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-365  )
    TwoModeCaseList <- filter(TwoModeCaseList, as.Date(citizen_report_date, format='%m/%d/%Y') >= Sys.Date()-365)
    TwoModeContactList <- filter(TwoModeContactList, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-365 )
    TwoModeContactList_W <- filter(TwoModeContactList_W, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-365 )
    TwoModeContactList_H <- filter(TwoModeContactList_H, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-365 )
    TwoModeContactList_N <- filter(TwoModeContactList_N, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-365 )
    TwoModeContactList_S <- filter(TwoModeContactList_S, as.Date(CREATED_AT, format='%m/%d/%Y') >= Sys.Date()-365 )
  }
                                                  
  ## Filter by PSGC INPUT
  if (substr(psgcInput,3,9) == "0000000" ){
    geogLevel <- "Region"
  } else if (substr(psgcInput,5,9) == "00000" ) {
    geogLevel <- "Province"
  } else if (substr(psgcInput,7,9) == "000" ) {
    geogLevel <- "CityMun"
  } else {
    geogLevel <- "Barangay"
  }

  if(geogLevel == "Region"){
    CaseList <- filter(CaseList, Res_RegionPSGC == substr(psgcInput,1,2))
    ContactList <- filter(ContactList, Res_RegionPSGC == substr(psgcInput,1,2))
    TwoModeCaseList <- filter(TwoModeCaseList, Res_RegionPSGC == substr(psgcInput,1,2))
    TwoModeContactList <- filter(TwoModeContactList, Res_RegionPSGC == substr(psgcInput,1,2))
    TwoModeContactList_W <- filter(TwoModeContactList_W, Res_RegionPSGC == substr(psgcInput,1,2))
    TwoModeContactList_H <- filter(TwoModeContactList_H, Res_RegionPSGC == substr(psgcInput,1,2))
    TwoModeContactList_N <- filter(TwoModeContactList_S, Res_RegionPSGC == substr(psgcInput,1,2))
    TwoModeContactList_S <- filter(TwoModeContactList_S, Res_RegionPSGC == substr(psgcInput,1,2))
    
  } else if (geogLevel == "Province") {
    CaseList <- filter(CaseList, Res_ProvincePSGC == substr(psgcInput,1,4))
    ContactList <- filter(ContactList, Res_ProvincePSGC == substr(psgcInput,1,4))
    TwoModeCaseList <- filter(TwoModeCaseList, Res_ProvincePSGC == substr(psgcInput,1,4))
    TwoModeContactList <- filter(TwoModeContactList, Res_ProvincePSGC == substr(psgcInput,1,4))
    TwoModeContactList_W <- filter(TwoModeContactList_W, Res_ProvincePSGC == substr(psgcInput,1,4))
    TwoModeContactList_H <- filter(TwoModeContactList_H, Res_ProvincePSGC == substr(psgcInput,1,4))
    TwoModeContactList_N <- filter(TwoModeContactList_S, Res_ProvincePSGC == substr(psgcInput,1,4))
    TwoModeContactList_S <- filter(TwoModeContactList_S, Res_ProvincePSGC == substr(psgcInput,1,4))
    
  } else if (geogLevel == "CityMun") {
    CaseList <- filter(CaseList, Res_CityMunPSGC == substr(psgcInput,1,6))
    ContactList <- filter(ContactList, Res_CityMunPSGC == substr(psgcInput,1,6))
    TwoModeCaseList <- filter(TwoModeCaseList, Res_CityMunPSGC == substr(psgcInput,1,6))
    TwoModeContactList <- filter(TwoModeContactList, Res_CityMunPSGC == substr(psgcInput,1,6))
    TwoModeContactList_W <- filter(TwoModeContactList_W, Res_CityMunPSGC == substr(psgcInput,1,6))
    TwoModeContactList_H <- filter(TwoModeContactList_H, Res_CityMunPSGC == substr(psgcInput,1,6))
    TwoModeContactList_N <- filter(TwoModeContactList_S, Res_CityMunPSGC == substr(psgcInput,1,6))
    TwoModeContactList_S <- filter(TwoModeContactList_S, Res_CityMunPSGC == substr(psgcInput,1,6))
    
  } else {
    CaseList <- filter(CaseList, Res_BarangayPSGC == psgcInput)
    ContactList <- filter(ContactList, Res_BarangayPSGC == psgcInput)
    TwoModeCaseList <- filter(TwoModeCaseList, Res_BarangayPSGC == psgcInput)
    TwoModeContactList <- filter(TwoModeContactList, Res_BarangayPSGC == psgcInput)
    TwoModeContactList_W <- filter(TwoModeContactList_W, Res_BarangayPSGC == psgcInput)
    TwoModeContactList_H <- filter(TwoModeContactList_H, Res_BarangayPSGC == psgcInput)
    TwoModeContactList_N <- filter(TwoModeContactList_S, Res_BarangayPSGC == psgcInput)
    TwoModeContactList_S <- filter(TwoModeContactList_S, Res_BarangayPSGC == psgcInput)
  }
  
  ## Count the number of cases per category 
  positive = sum(CaseList$HealthStatus == "Confirmed")
  suspect = sum(CaseList$HealthStatus == "Suspect")
  negative = sum(CaseList$HealthStatus == "Low Risk")
  traced = nrow(CaseList) - (positive + suspect + negative)
  
  # COMPUTE FOR CENTRALITY MEASURES -------------------------------------
  
  #graph_from_data_frame
  igraph_onemode <- graph_from_data_frame(ContactList, directed = TRUE)
  igraph_twomode <- graph_from_data_frame(TwoModeContactList, directed = TRUE)
  igraph_twomode_w <- graph_from_data_frame(TwoModeContactList_W, directed = TRUE)
  igraph_twomode_h <- graph_from_data_frame(TwoModeContactList_H, directed = TRUE)
  igraph_twomode_n <- graph_from_data_frame(TwoModeContactList_N, directed = TRUE)
  igraph_twomode_s <- graph_from_data_frame(TwoModeContactList_S, directed = TRUE)
  
  ## Get betweenness and degree centrality scores
  deg_network <- degree(igraph_onemode, mode="out")
  deg_network2 <- degree(igraph_twomode, mode="out")
  deg_network2_w <- degree(igraph_twomode_w, mode="out")
  deg_network2_h <- degree(igraph_twomode_h, mode="out")
  deg_network2_n <- degree(igraph_twomode_n, mode="out")
  deg_network2_s <- degree(igraph_twomode_s, mode="out")
  bet_network <- betweenness(igraph_twomode)
  
  ## Sort centrality scores
  superspreaders <- data.frame(head(sort(deg_network, decreasing = TRUE), 10))               #individuals
  superspreader_centers <- data.frame(head(sort(deg_network2, decreasing = TRUE), 10))       #locations
  superspreader_centers_w <- data.frame(head(sort(deg_network2_w, decreasing = TRUE), 10))   #workplaces
  superspreader_centers_h <- data.frame(head(sort(deg_network2_h, decreasing = TRUE), 10))   #households
  superspreader_centers_n <- data.frame(head(sort(deg_network2_n, decreasing = TRUE), 10))   #health facilities
  superspreader_centers_s <- data.frame(head(sort(deg_network2_s, decreasing = TRUE), 10))   #places visited
  bridges <-  data.frame(head(sort(bet_network, decreasing = TRUE), 10))                     #bridges
  
  #Mutate rownames to a column
  superspreaders <- superspreaders %>%
    mutate(Individual = rownames(superspreaders))
  superspreader_centers <- superspreader_centers %>%
    mutate(Location = rownames(superspreader_centers))
  superspreader_centers_w <- superspreader_centers_w %>%
    mutate(Workplace = rownames(superspreader_centers_w))
  superspreader_centers_h <- superspreader_centers_h %>%
    mutate(Household = rownames(superspreader_centers_h))
  superspreader_centers_n <- superspreader_centers_n %>%
    mutate(Health_Facility = rownames(superspreader_centers_n))
  superspreader_centers_s <- superspreader_centers_s %>%
    mutate(Place_Visited = rownames(superspreader_centers_s))
  bridges <- bridges %>%
    mutate(Bridge = rownames(bridges))
  
  
  #Rename Columns
  colnames(superspreaders)[1] <- "DegreeCentralityScore"
  colnames(superspreader_centers)[1] <- "DegreeCentralityScore"
  colnames(superspreader_centers_w)[1] <- "DegreeCentralityScore"
  colnames(superspreader_centers_h)[1] <- "DegreeCentralityScore"
  colnames(superspreader_centers_n)[1] <- "DegreeCentralityScore"
  colnames(superspreader_centers_s)[1] <- "DegreeCentralityScore"
  colnames(bridges)[1] <- "BetweennessCentralityScore"
  
  #Reorder columns
  superspreaders <- superspreaders[,c(2,1)]
  superspreader_centers <- superspreader_centers[,c(2,1)]
  superspreader_centers_w <- superspreader_centers_w[,c(2,1)]
  superspreader_centers_h <- superspreader_centers_h[,c(2,1)]
  superspreader_centers_n <- superspreader_centers_n[,c(2,1)]
  superspreader_centers_s <- superspreader_centers_s[,c(2,1)]
  bridges <- bridges[,c(2,1)]
  
  ##Filter by modeInput
  
  returnValues = list(onemode_contactviz = ContactList,
                      twomode_contactviz = TwoModeContactList,
                      individuals = superspreaders, 
                      locations = superspreader_centers,
                      workplaces = superspreader_centers_w, 
                      households = superspreader_centers_h,
                      health_facilities = superspreader_centers_n, 
                      places_visited = superspreader_centers_s,  
                      bridges = bridges, 
                      traced = traced, positive = positive, suspect = suspect, negative = negative)
  
  return(returnValues)
  
}
