summaries = read.csv("timeseriesVax/inputs/parametersSVEIR.csv")
summaries_SVEIRS = read.csv("timeseriesVax/inputs/parametersSVEIRS.csv") 
#SVEIR Code ------------------
getPsgcDetails = function(psgcInput) {
  dfToReturn = summaries[which(as.numeric(summaries$area) == as.numeric(psgcInput)),]
  return(dfToReturn)
}

RunProjections = function(lambdaDateInput, lambdaValuesInput,
                          psgcInput, startHcDate, HcLevel, projectionDuration, 
                          scaleLevel, fitParamToUse, interventionStartDate,
                          interventionEndDate, interventionPercentage,
                          interventionCompliance, dateAdjustment,
                          v1Value,v2Value,JValue,filterPSGCs){
  
  dateAdjustmentSearchSpace = 10
  lambdaDateInput = as.Date(strsplit(lambdaDateInput, ";")[[1]])
  lambdaValuesInput = as.numeric(strsplit(lambdaValuesInput, ";")[[1]])
  
  interventionStartDate = as.Date(interventionStartDate)
  interventionEndDate = as.Date(interventionEndDate)
  interventionPercentage = as.numeric(interventionPercentage)
  interventionCompliance = as.numeric(interventionCompliance)
  
  startHcDate = as.Date(startHcDate)
  HcLevel = as.numeric(HcLevel)
  filterPSGCs = strsplit(filterPSGCs, ";")[[1]]
  
  
  origProjectDuration = as.numeric(projectionDuration)
  projectionDuration = as.numeric(projectionDuration) + dateAdjustmentSearchSpace + 1
  scaleLevel = as.numeric(scaleLevel)
  dateAdjustment = as.logical(dateAdjustment)
  
  parGen_estimateLambda2 = function(){ #tinanggal yung engineer = TRUE
    parnames = c("tau", "beta", "c", "c1","c2","omega", "thetaA", "thetaS","r", 
                 "deltaA", "deltaA1","deltaA2","epsT","epsI","epsIH", 
                 "E1", "E2", "E3", "E4", "E5", "k1", "k2","mu")#"J"
    pargenDetails = psgcInput %>% as.numeric() %>% getPsgcDetails() %>% filter(FittedTo == fitParamToUse)
    
    deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
    
    result = c(
      pargenDetails['tau'], pargenDetails['beta'],# transmission
      pargenDetails['c'], pargenDetails['c1'], pargenDetails['c2'], pargenDetails['omega'], # transitions
      pargenDetails['thetaA'],pargenDetails['thetaS'], pargenDetails['r'], # recoveries
      0.25*deltaS.l[1], 0.1*deltaS.l[1], 0.1*deltaS.l[1], # deltaA
      pargenDetails['epsT'], pargenDetails['epsI'],pargenDetails['epsIH'],
      pargenDetails['E1'], pargenDetails['E2'], pargenDetails['E3'], pargenDetails['E4'], pargenDetails['E5'],
      pargenDetails['k1'],pargenDetails['k2'],
      pargenDetails['mu']#, pargenDetails['J'], pargenDetails['v1'], pargenDetails['v2'],
    )
    
    names(result) = parnames
    
    return(list(parameters = result, deltaS.l = deltaS.l))
  }
  
  pargenDetails <- getPsgcDetails(psgcInput) %>% filter(FittedTo == fitParamToUse)
  
  deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  v1.l <- v1Value %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  v2.l <- v2Value %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  J.l <- JValue %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  
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
      
      
      startYear <- 2021
      startMonth <- 7
      
      computeYear <- format(first_date + t - 1, "%Y") %>% as.numeric()
      computeMonth <- format(first_date + t - 1, "%m") %>% as.numeric()
      
      parameter_index <- 12*computeYear + computeMonth - (12*startYear + startMonth) + 1
      
      deltaS <- ifelse(parameter_index >= length(deltaS.l),
                       deltaS.l[length(deltaS.l)],
                       deltaS.l[parameter_index])
      
      tv1 <- ifelse(parameter_index >= length(v1.l),
                    v1.l[length(v1.l)],
                    v1.l[parameter_index])
      
      tv2 <- ifelse(parameter_index >= length(v2.l),
                    v2.l[length(v2.l)],
                    v2.l[parameter_index])
      
      J <- ifelse(parameter_index >= length(J.l),
                  J.l[length(J.l)],
                  J.l[parameter_index])
      
      if(t >= setdate){
        deltaS = setdelt
      }
      
      
      deltaS1 <- deltaS
      deltaS2 <- deltaS
      deltaA <- 0.25*deltaS
      deltaA1 <- 0.1*deltaS
      deltaA2 <- 0.1*deltaS
      
      dS= A - (1-reallam)*beta*S*(IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 +E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu*S) - tv1 - J
      dE = (1-reallam)*beta*S*(IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu + 1/tau)*E
      dIA = c*E/tau - (mu + omega + deltaA + thetaA)*IA
      dIS = (1-c)*E/tau + omega*IA - (mu + epsI + deltaS + thetaS)*IS
      
      dV1 = tv1 - k1*(1-reallam)*beta*V1*(IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu*V1) -tv2
      dEv1 = k1*(1-reallam)*beta*V1*( IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R)-(mu + 1/tau)*Ev1
      dIAv1 = c1*Ev1/tau - (mu + omega + deltaA1 + thetaA)*IAv1
      dISv1 = (1-c1)*Ev1/tau + omega*IAv1 - (mu + epsIH + deltaS1 + thetaS )*ISv1
      
      dV2 = tv2 - k2*(1-reallam)*beta*V2*( IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 +E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R)- mu*V2 + J
      dEv2 = k2*(1-reallam)*beta*V2*( IS + E1*IA + E2*ISv1 + E3*IAv1 + E4*ISv2 + E5*IAv2)/(S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+Q+R) - (mu + 1/tau)*Ev2
      dIAv2 = c2*Ev2/tau - (mu + omega + deltaA2 + thetaA)*IAv2
      dISv2 = (1-c2)*Ev2/tau + omega*IAv2 - (mu + deltaS2 + thetaS)*ISv2
      
      dQ = deltaA*IA + deltaS*IS + deltaA1*IAv1 + deltaS1*ISv1 + deltaA2*IAv2 + deltaS2*ISv2 - (mu + epsT + r)*Q
      dC = deltaA*IA + deltaS*IS + deltaA1*IAv1 + deltaS1*ISv1 + deltaA2*IAv2 + deltaS2*ISv2
      dR = r*Q + thetaA*(IA+IAv1+IAv2) + thetaS*(IS+ISv1+ISv2) - mu*R
      
      list(c(dS, dE, dIA, dIS, dV1, dEv1, dIAv1, dISv1, dV2, dEv2, dIAv2, dISv2, dQ, dC, dR))
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
  
  
  istates <- c(
    S = as.numeric(inputs['S0']),#inputs['E0']
    E = as.numeric(inputs['E0']),
    IA = as.numeric(inputs['IA0']),
    IS = as.numeric(inputs['IS0']),
    V1 = as.numeric(inputs['V10']),
    Ev1 = as.numeric(inputs['Ev10']),
    IAv1 = as.numeric(inputs['IAv10']),
    ISv1 = as.numeric(inputs['ISv10']),
    V2 = as.numeric(inputs['V20']),
    Ev2 = as.numeric(inputs['Ev20']),
    IAv2 = as.numeric(inputs['IAv20']),
    ISv2 = as.numeric(inputs['ISv20']),
    Q = as.numeric(inputs['Q0']),
    C = as.numeric(inputs['C0']),
    R = as.numeric(inputs['R0']))
  
  
  
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
  
  if (filterPSGCs == ""){ #if walang filter
    actualIncidenceCounts = getActiveCases(psgcInput, fitParamToUse)
  }else{
    actualIncidenceCounts = getActiveCasesFilter(psgcInput, fitParamToUse, filterPSGCs)
  }
  
  
  
  
  
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


#SVEIRs Code ------------------
getPsgcDetails_SVEIRS = function(psgcInput) {
  dfToReturn = summaries_SVEIRS[which(as.numeric(summaries_SVEIRS$area) == as.numeric(psgcInput)),]
  return(dfToReturn)
}


RunProjections_SVEIRS = function(lambdaDateInput, lambdaValuesInput,
                          psgcInput, startHcDate, HcLevel, projectionDuration, 
                          scaleLevel, fitParamToUse, interventionStartDate,
                          interventionEndDate, interventionPercentage,
                          interventionCompliance, dateAdjustment,
                          v1Value,v2Value,v3Value,JValue,filterPSGCs){
  
  dateAdjustmentSearchSpace = 10
  lambdaDateInput = as.Date(strsplit(lambdaDateInput, ";")[[1]])
  lambdaValuesInput = as.numeric(strsplit(lambdaValuesInput, ";")[[1]])
  
  interventionStartDate = as.Date(interventionStartDate)
  interventionEndDate = as.Date(interventionEndDate)
  interventionPercentage = as.numeric(interventionPercentage)
  interventionCompliance = as.numeric(interventionCompliance)
  
  startHcDate = as.Date(startHcDate)
  HcLevel = as.numeric(HcLevel)
  filterPSGCs = strsplit(filterPSGCs, ";")[[1]]
  

  origProjectDuration = as.numeric(projectionDuration)
  projectionDuration = as.numeric(projectionDuration) + dateAdjustmentSearchSpace + 1
  scaleLevel = as.numeric(scaleLevel)
  dateAdjustment = as.logical(dateAdjustment)
  
  parGen_estimateLambda2 = function(){ #tinanggal yung engineer = TRUE
    # parnames = c("tau", "beta", 
    #              "c", "c1","c2","c3","c4","omega", 
    #              "thetaA", "thetaS","r", 
    #              "epsT","epsI","epsIH","epsHT", 
    #              "E1", "E2", "E3", "E4", "E5",
    #              "E6", "E7", "E8", "E9", 
    #              "k1", "k2", "k3","k4","mu")#"J"
    
    parnames = c("tau", "beta", 
                 "c","omega", 
                 "thetaA", "thetaS","r", 
                 "epsT","epsI","epsIH","epsHT",
                 "E1", "E2", "E3", "E4", "E5",
                 "E6", "E7", "E8", "E9", 
                 "mu")#"J"
    
    
    pargenDetails = psgcInput %>% as.numeric() %>% getPsgcDetails_SVEIRS() %>% filter(FittedTo == fitParamToUse)
    
    deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
    
    # result = c(
    #   pargenDetails['tau'], pargenDetails['beta'],# transmission
    #   pargenDetails['c'], pargenDetails['c1'], pargenDetails['c2'], pargenDetails['c3'], pargenDetails['c2'], pargenDetails['omega'], # transitions
    #   pargenDetails['thetaA'],pargenDetails['thetaS'], pargenDetails['r'], # recoveries
    #   pargenDetails['epsT'], pargenDetails['epsI'],pargenDetails['epsIH'],pargenDetails['epsHT'],
    #   pargenDetails['E1'], pargenDetails['E2'], pargenDetails['E3'], pargenDetails['E4'], pargenDetails['E5'],
    #   pargenDetails['E6'], pargenDetails['E7'], pargenDetails['E8'], pargenDetails['E9'], 
    #   pargenDetails['k1'], pargenDetails['k2'], pargenDetails['k3'], pargenDetails['k4'],
    #   pargenDetails['mu']#, pargenDetails['J'], pargenDetails['v1'], pargenDetails['v2'],
    # )
    result = c(
      pargenDetails['tau'], pargenDetails['beta'],# transmission
      pargenDetails['c'],  pargenDetails['omega'], # transitions
      pargenDetails['thetaA'],pargenDetails['thetaS'], pargenDetails['r'], # recoveries
      pargenDetails['epsT'], pargenDetails['epsI'],pargenDetails['epsIH'],pargenDetails['epsHT'],
      pargenDetails['E1'], pargenDetails['E2'], pargenDetails['E3'], pargenDetails['E4'], pargenDetails['E5'],
      pargenDetails['E6'], pargenDetails['E7'], pargenDetails['E8'], pargenDetails['E9'], 
      pargenDetails['mu']#, pargenDetails['J'], pargenDetails['v1'], pargenDetails['v2'],
    )
    
    names(result) = parnames
    
    return(list(parameters = result, deltaS.l = deltaS.l))
  }
  
  pargenDetails <- getPsgcDetails_SVEIRS(psgcInput) %>% filter(FittedTo == fitParamToUse)
  
  deltaS.l <- pargenDetails["deltaS"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  v1.l <- v1Value %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  v2.l <- v2Value %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  v3.l <- v3Value %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  J.l <- JValue %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  k1.l <- pargenDetails["k1"] %>% as.character()%>% strsplit(";") %>% unlist() %>% as.numeric()
  k2.l <- pargenDetails["k2"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  k3.l <- pargenDetails["k3"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  k4.l <- pargenDetails["k4"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  c1.l <- pargenDetails["c1"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  c2.l <- pargenDetails["c2"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  c3.l <- pargenDetails["c3"] %>% as.character() %>% strsplit(";") %>% unlist() %>% as.numeric()
  c4.l <- c3.l
  
  Covid19Model_SVEIRS = function(t, state, parameters){
    with(as.list(c(state,parameters)),{
      # if (t >= intervStartDate & t <= intervEndDate) {
      #   interventionEffect = (interventionPercentage * interventionCompliance)
      # } else {
      #   interventionEffect = 0
      # }
      
      if (t <= tail(equar.l, n = 1)) {
        reallam = (lambdaValuesInput[min(which(t <= equar.l))])
      } else {
        reallam = 0
      }
      
      startYear <- 2021
      startMonth <- 12
      
      computeYear <- format(first_date + t - 1, "%Y") %>% as.numeric()
      computeMonth <- format(first_date + t - 1, "%m") %>% as.numeric()
      
      parameter_index <- 12*computeYear + computeMonth - (12*startYear + startMonth) + 1
      
      deltaS <- ifelse(parameter_index >= length(deltaS.l),
                       deltaS.l[length(deltaS.l)],
                       deltaS.l[parameter_index])
      
      # tv1 <- ifelse(parameter_index >= length(v1.l),
      #               v1.l[length(v1.l)],
      #               v1.l[parameter_index])
      
      tv1 <- if(t >= switchtime){0} else {
        ifelse(parameter_index >= length(v1.l),
               v1.l[length(v1.l)],
               v1.l[parameter_index])
      }
      
      tv2 <- ifelse(parameter_index >= length(v2.l),
                    v2.l[length(v2.l)],
                    v2.l[parameter_index])
      
      tv3 <- ifelse(parameter_index >= length(v3.l),
                    v3.l[length(v3.l)],
                    v3.l[parameter_index])
      
      # J <- ifelse(parameter_index >= length(J.l),
      #             J.l[length(J.l)],
      #             J.l[parameter_index])
      
      J <- if(t >= switchtime){0} else {
        ifelse(parameter_index >= length(J.l),
               J.l[length(J.l)],
               J.l[parameter_index])
      }
      
      k1 <- ifelse(parameter_index >= length(k1.l),
                   k1.l[length(k1.l)],
                   k1.l[parameter_index])
      
      k2 <- ifelse(parameter_index >= length(k2.l),
                   k2.l[length(k2.l)],
                   k2.l[parameter_index])
      
      k3 <- ifelse(parameter_index >= length(k3.l),
                   k3.l[length(k3.l)],
                   k3.l[parameter_index])
      
      k4 <- ifelse(parameter_index >= length(k4.l),
                   k4.l[length(k4.l)],
                   k4.l[parameter_index])
      
      c1 <- ifelse(parameter_index >= length(c1.l),
                   c1.l[length(c1.l)],
                   c1.l[parameter_index])
      
      c2 <- ifelse(parameter_index >= length(c2.l),
                   c2.l[length(c2.l)],
                   c2.l[parameter_index])
      
      c3 <- ifelse(parameter_index >= length(c3.l),
                   c3.l[length(c3.l)],
                   c3.l[parameter_index])
      
      c4 <- c3
      
      # if(t >= setdate){
      #   deltaS = setdelt
      # }
      
      n = S+E+IA+IS+V1+Ev1+IAv1+ISv1+V2+Ev2+IAv2+ISv2+V3+Ev3+IAv3+ISv3+Ev4+IAv4+ISv4+Q+R
      Ie = (IS+E1*IA+E2*ISv1+E3*IAv1+E4*ISv2+E5*IAv2+E6*ISv3+E7*IAv3+E8*IAv4+E9*ISv4)
      
      deltaS1 <- deltaS
      deltaS2 <- deltaS
      deltaS3 <- deltaS
      deltaS4 <- deltaS
      deltaA <- 0.25*deltaS
      deltaA1 <- 0.1*deltaS
      deltaA2 <- 0.1*deltaS
      deltaA3 <- 0.1*deltaS
      deltaA4 <- 0.1*deltaS

      # dS = A - (1-reallam)*beta*S*(Ie/n) - (mu*S) - tv1 - J 
      dS = A - (1-reallam)*beta*S*(Ie/n) - (mu*S) - tv1 - J 
      dE = (1-reallam)*beta*S*(Ie/n) - (mu + 1/tau)*E 
      dIA = c*E/tau - (mu + omega + deltaA + thetaA)*IA 
      dIS = (1-c)*E/tau + omega*IA - (mu + epsI + deltaS + thetaS)*IS
      
      dV1 = tv1 - k1*(1-reallam)*beta*V1*(Ie/n) - (mu*V1) - tv2
      dEv1 = k1*(1-reallam)*beta*V1*(Ie/n) - (mu + 1/tau)*Ev1 
      dIAv1 = c1*Ev1/tau - (mu + omega + deltaA1 + thetaA)*IAv1 
      dISv1 = (1-c1)*Ev1/tau + omega*IAv1 - (mu + epsIH + deltaS1 + thetaS )*ISv1 
      
      dV2 = tv2 - k2*(1-reallam)*beta*V2*(Ie/n) - (mu*V2) - tv3 + J
      dEv2 = k2*(1-reallam)*beta*V2*(Ie/n) - (mu + 1/tau)*Ev2 
      dIAv2 = c2*Ev2/tau - (mu + omega + deltaA2 + thetaA)*IAv2 
      dISv2 = (1-c2)*Ev2/tau + omega*IAv2 - (mu + deltaS2 + thetaS + epsHT)*ISv2 
      
      dV3 = tv3 - k3*(1-reallam)*beta*V3*(Ie/n) - mu*V3 
      dEv3 = k3*(1-reallam)*beta*V3*(Ie/n) - (mu + 1/tau)*Ev3 
      dIAv3 = c3*Ev3/tau - (mu + omega + deltaA3 + thetaA)*IAv3 
      dISv3 = (1-c3)*Ev3/tau + omega*IAv3 - (mu + deltaS3 + thetaS)*ISv3
      
      dEv4 = k4*(1-reallam)*beta*R*(Ie/n) - (mu + 1/tau)*Ev4 
      dIAv4 = c4*Ev4/tau - (mu + omega + deltaA4 + thetaA)*IAv4 
      dISv4 = (1-c4)*Ev4/tau + omega*IAv4 - (mu + deltaS4 + thetaS + epsHT)*ISv4  
      
      dQ = deltaA*IA + deltaS*IS + deltaA1*IAv1 + deltaS1*ISv1 + deltaA2*IAv2 + deltaS2*ISv2 + deltaA3*IAv3 + deltaS3*ISv3 + deltaA4*IAv4 + deltaS4*ISv4 - (mu + epsT + r)*Q 
      dC = deltaA*IA + deltaS*IS + deltaA1*IAv1 + deltaS1*ISv1 + deltaA2*IAv2 + deltaS2*ISv2 + deltaA3*IAv3 + deltaS3*ISv3 + deltaA4*IAv4 + deltaS4*ISv4
      dR = r*Q + thetaA*(IA+IAv1+IAv2+IAv3+IAv4) + thetaS*(IS+ISv1+ISv2+ISv3+ISv4) - (mu*R + k4*(1-reallam)*beta*R*(Ie/n))
      
      return(list(c(dS, dE, dIA, dIS, dV1, dEv1, dIAv1, dISv1,dV2, dEv2, dIAv2, dISv2, dV3, dEv3, dIAv3, dISv3, dEv4,dIAv4,dISv4, dQ, dC, dR)))
    })
  }
  
  scen = which(as.numeric(summaries_SVEIRS$area) == as.numeric(psgcInput) & summaries_SVEIRS$FittedTo == fitParamToUse)
  
  first_date = as.Date(as.character(summaries_SVEIRS[scen, 'first_date']))
  
  inputs = summaries_SVEIRS[scen, ]
  inputs = suppressWarnings(as.numeric(inputs))
  names(inputs) = colnames(summaries_SVEIRS)[1:ncol(summaries_SVEIRS)]
  
  N = inputs['pop']
  A = N*0.020177/365
  
  equar.l = as.numeric((lambdaDateInput - first_date )) #+ 1 works monthly

  pars = parGen_estimateLambda2()$parameters
  
  setdate = as.numeric(startHcDate - first_date) + 1
  setdelt = HcLevel

  
  intervStartDate = as.numeric(interventionStartDate - first_date) + 1
  intervEndDate = as.numeric(interventionEndDate - first_date) + 1
  switchtime = inputs['switch_time']

     istates <- c(
       S = as.numeric(inputs['S0']),
       E = as.numeric(inputs['E0']),
       IA = as.numeric(inputs['IA0']),
       IS = as.numeric(inputs['IS0']),
       V1 = as.numeric(inputs['V10']),
       Ev1 = as.numeric(inputs['Ev10']),
       IAv1 = as.numeric(inputs['IAv10']),
       ISv1 = as.numeric(inputs['ISv10']),
       V2 = as.numeric(inputs['V20']),
       Ev2 = as.numeric(inputs['Ev20']),
       IAv2 = as.numeric(inputs['IAv20']),
       ISv2 = as.numeric(inputs['ISv20']),
       V3 = as.numeric(inputs['V30']),
       Ev3 = as.numeric(inputs['Ev30']),
       IAv3 = as.numeric(inputs['IAv30']),
       ISv3 = as.numeric(inputs['ISv30']),
       Ev4 = as.numeric(inputs['Ev40']),
       IAv4 = as.numeric(inputs['IAv40']),
       ISv4 = as.numeric(inputs['ISv40']),
       Q = as.numeric(inputs['Q0']),
       C = as.numeric(inputs['C0']),
       R = as.numeric(inputs['R0']))


  
  projtime = seq(1, projectionDuration, by = 1)
  #checkfornegative = ode(y=istates,time=projtime,func=Covid19Model_SVEIRS,parms=pars)
  
  #if(switchtime <= projectionDuration){switchtime <- max(which(checkfornegative[, "S"] >= 0))}
         
  check = ode(y=istates,time=projtime,func=Covid19Model_SVEIRS,parms=pars)
 
  
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
  
  if (filterPSGCs == ""){ #if walang filter
    actualIncidenceCounts = getActiveCases_SVEIRS(psgcInput, fitParamToUse)
  }else{
    actualIncidenceCounts = getActiveCasesFilter_SVEIRS(psgcInput, fitParamToUse, filterPSGCs)
  }

  
  
  
  
  if(dateAdjustment){
    maxDateIndex = which(as.character(checkReturn$Date) == maxDate_SVEIR)
    
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
  psgcDetails = getPsgcDetails_SVEIRS(psgcInput)
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
