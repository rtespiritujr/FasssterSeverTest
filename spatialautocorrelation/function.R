lisa.packages <- c("tmap", "spdep", "dplyr", "sf", "readr", "tidyr", "spdyn", "jsonlite")
#install.packages(packages)
#install.packages("spdyn", repos="http://R-Forge.R-project.org")
lapply(lisa.packages, library, character.only = TRUE)

lisa_compute_moran <- function(city_psgc){
  city_psgc <- as.integer(city_psgc)
  
  # Extract province code from the city psgc
  province_code <- floor(city_psgc/100000)*100000 %>% as.integer()
  
  # Neighboring provinces
  province_nb.l <- c(province_code, read.csv(sprintf("spatialautocorrelation/Data/Province Neighborhood/%s.csv", province_code))) %>% unlist()
  
  # Subset of barangays in the province and neighboring provinces
  map_subset.sf <- subset(lisa_map.sf, Pro_Code %in% province_nb.l)
  
  # List of barangay neighbors for each barangay in the provinces
  expanded_nb.l <- poly2nb(map_subset.sf, queen = TRUE)
  
  names(expanded_nb.l) <- map_subset.sf$Bgy_Code
  
  # List of barangays in the chosen city
  bgy.l <- lisa_map.sf$Bgy_Code[lisa_map.sf$Mun_Code2 == city_psgc & !is.na(lisa_map.sf$Bgy_Code)] %>% unique()
  
  
  # Barangays at Most 2 Steps Away ------------------------------------------
  
  extension_bgy.l <- bgy.l
  
  # Get the neighbors of the barangays (1 step away)
  for (i in bgy.l){
    extension_bgy.l <- extension_bgy.l %>%
      c(unique(map_subset.sf$Bgy_Code)[expanded_nb.l[[as.character(i)]]])
  }
  
  extension_bgy.l <- extension_bgy.l %>% unique() # Remove duplicates
  extension_bgy.l <- extension_bgy.l[!is.na(extension_bgy.l)] # Remove NA
  
  # List of barangays in the city and 1 step outside (for Moran's I)
  extension1_bgy.l <- extension_bgy.l %>% unique()
  extension1_bgy.l <- extension1_bgy.l[!is.na(extension1_bgy.l)]
  
  # Get the neighbors of the neighbors of the barangays (2 steps away)
  for (i in unique(extension_bgy.l)){
    extension_bgy.l <- extension_bgy.l %>%
      c(unique(map_subset.sf$Bgy_Code)[expanded_nb.l[[as.character(i)]]])
  }
  
  extension_bgy.l <- extension_bgy.l %>% unique() # Remove duplicates
  extension_bgy.l <- extension_bgy.l[!is.na(extension_bgy.l)] # Remove NA 
  
  
  # Empirical Bayes Estimate ------------------------------------------------
  
  # Refine the filter to only those in within 2 steps outisde the boundary
  map_EBsubset.sf <- map_subset.sf %>% filter(Bgy_Code %in% extension_bgy.l)
  
  # Join the cases with the map data
  moran.df <- map_EBsubset.sf %>%
    dplyr::left_join(lisa_cases.df, by = "Bgy_Code") %>%
    dplyr::mutate(population = case_when(is.na(population) ~ 1,
                                         TRUE ~ population)) %>% 
    dplyr::mutate_at(vars(contains("cases"),
                          confirmed,
                          deaths,
                          recovered,
                          active), ~replace_na(., 0))
  
  # Neighborhood list of the expanded boundary (2 steps) to be used in EB
  EB_nb.l <- poly2nb(map_EBsubset.sf, queen = TRUE)
  
  # Empirical Baye's estimate to address outliers
  EB_results <- EBlocal(moran.df$cases_14,
                        moran.df$population,
                        EB_nb.l,
                        zero.policy = TRUE) 
  
  # Replace NaN density with 0
  moran.df$EB <- EB_results$est %>% replace_na(0) 
  
  # Scaled EB for easier classification
  moran.df$sEB <- scale(moran.df$EB) %>% replace_na(0)
  
  # Local Moran's I ---------------------------------------------------------
  
  # After computing EB estimates, setup dataframe for Moran's I
  moran.df <- moran.df %>% filter(Bgy_Code %in% extension1_bgy.l)
  
  # Map data should only include until 1 barangay outside the boundary
  map_moran.sf <- map_EBsubset.sf %>% filter(Bgy_Code %in% extension1_bgy.l)
  
  # Neighborhood list of 1 step outside the boundary
  moran_nb.l <- poly2nb(map_moran.sf)
  
  # Indices for barangays with neighbors
  moran_bgy.i <- unlist(lapply(moran_nb.l, sum)) != 0 
  
  # Remove barangays without neighbors
  map_moran.sf <- map_moran.sf[moran_bgy.i, ] 
  
  # Store data of neighborless barangays
  complement_df <- moran.df %>% filter(!(Bgy_Code %in% map_moran.sf$Bgy_Code))
  
  # Keep data of barangays with neighbors
  moran.df <- moran.df %>% filter(Bgy_Code %in% map_moran.sf$Bgy_Code)
  
  # Get neighborhood list for barangays with neighbors (input for spatial weights)
  moran_nb.l <- poly2nb(map_moran.sf)
  
  # Spatial weights
  weights.l <- nb2listw(moran_nb.l, style = "W", zero.policy = TRUE) # B for binary also can try W to be 1/(# of neighbors)
  
  # Spatially lagged values
  moran.df$lag_sEB <- lag.listw(weights.l, moran.df$sEB, zero.policy = TRUE) 
  
  # Local Moran's I with p-values from the empirical distribution
  local_I <- lisa.perm(moran.df$EB, weights.l, perm = 9999)
  
  # Local Moran's I value
  moran.df$local_I <- local_I[,1] 
  
  # Local Moran's I p-values using empirical distribution
  moran.df$local_p <- local_I[,2] %>% p.adjustSP(moran_nb.l, method = "fdr") 
  
  # Merge the dataframe with computed statistics to the neighborless barangays
  if (nrow(complement_df) > 0) {
    complement_df$lag_sEB <- NA
    complement_df$local_I <- NA
    complement_df$local_p <- NA
    moran.df <- rbind(moran.df, complement_df)
  }
  
  varsToReattach <- names(lisa_cases.df)[-1]
  
  moran.df <- moran.df %>%
    # After computing Moran's I, only retain rows that belong to the chosen city
    dplyr::filter(Mun_Code2 == city_psgc) %>% 
    # Make sure that the count for cases are there even if there is no available population data
    dplyr::select(-all_of(varsToReattach)) %>% 
    dplyr::left_join(lisa_cases.df, by = "Bgy_Code") %>% 
    dplyr::mutate_at(vars(all_of(varsToReattach)), funs(case_when(is.na(.) ~ 0,
                                                                  TRUE ~ .)))
  
  
  # Revert population = 1 back to 0
  moran.df$population[moran.df$population == 1] <- NA
  
  # Identify clusters
  moran.df$local_cluster <- as.factor(ifelse(moran.df$local_p <= 0.05,
                                             "Cluster", NA))
  
  # Assign classifications for easier visualization
  # HH = 1
  # HL = 2
  # LH = 3
  # LL = 4
  
  moran.df$classification <- ifelse(moran.df$local_cluster=="Cluster",
                                    ifelse(moran.df$sEB >= 0,
                                           ifelse(moran.df$lag_sEB >= 0, 1, 2), # 1 is HH, 2 is HL
                                           ifelse(moran.df$lag_sEB >= 0, 3, 4)), NA) # 3 is LH 4 is LL
  
  
  output = list(moran.df, weights.l)
  names(output) = c("data.frame", "contiguity")
  
  return(output)
}

# Import data and combine data frames
lisa_map.sf <- st_read("spatialautocorrelation/Data/shapefiles/municipality-shapefile.shp") %>% 
  dplyr::mutate(Mun_Code2 = case_when(Pro_Code == 133900000 ~ 133900000,
                                      TRUE ~ mun_code))

# Import population data, replace 0 population with 1 so that it won't cause errors in the computation
lisa_pop.df <- read_csv("spatialautocorrelation/Data/updated_psgc.csv") %>%
  dplyr::select(Code, Population) %>%
  dplyr::mutate(Code = as.integer(Code)) %>% 
  drop_na()
lisa_pop.df[lisa_pop.df == 0] <- 1 # replace the 0 population barangays with 1

# lisa_data.df <- read_csv(unzip("linelist/ConfirmedCases.zip", files = "ConfirmedCases.csv"), na = c("NA", ""))
source("linelist/read_linelist.R")
lisa_data.df <- get_linelist()

colnames(lisa_data.df) <- gsub(" ", ".", colnames(lisa_data.df))

lisa_data.df <- lisa_data.df %>%
  dplyr::filter(as.character(Report_Date) != "",
                tolower(Repatriate) != "yes" | is.na(Repatriate)) %>%
  dplyr::mutate_at(vars(contains("Date_")), as.Date) %>%
  dplyr::mutate_at(vars(contains("_Date")), as.Date) %>% 
  dplyr::mutate_at(vars(contains("PSGC")), as.numeric) %>% 
  dplyr::mutate(cityPSGC2 = case_when(provincePSGC == 133900000 ~ 133900000,
                                      TRUE ~ cityPSGC))

lisa_endDate <-  max(lisa_data.df$Report_Date, na.rm = TRUE)# can change to a different
lisa_startDate <- lisa_endDate - 13 # entire period is 2 weeks

lisa_cases.df <- lisa_data.df %>%
  dplyr::filter(Report_Date >= lisa_startDate, Report_Date <= lisa_endDate) %>%  # get cases that occurred within the time period
  dplyr::group_by(barangayPSGC) %>% # group according to barangays
  dplyr::summarize(cases_7 = sum(Report_Date >= lisa_startDate + 7),
                   cases_14 = n()) %>% # get the count per barangay
  tidyr::drop_na() %>% # to remove the count of NA's
  dplyr::rename(Code = barangayPSGC) # for easier joining

lisa_cumulativeCases.df <- lisa_data.df %>%
  dplyr::group_by(barangayPSGC) %>%
  dplyr::summarize(confirmed = n(),
                   deaths = sum(tolower(Status) == "died"),
                   recovered = sum(tolower(Status) == "recovered")) %>% 
  dplyr::mutate(active = confirmed - deaths - recovered) %>% 
  dplyr::rename(Code = barangayPSGC)

lisa_cases.df <- lisa_cases.df %>% 
  dplyr::left_join(lisa_cumulativeCases.df, by = "Code") %>%
  dplyr::mutate_at(vars(contains("cases"),
                        confirmed,
                        deaths,
                        recovered,
                        active), ~replace_na(., 0)) %>% 
  dplyr::left_join(lisa_pop.df, by = "Code") %>% # combine with the population 
  dplyr::rename(population = Population,
                Bgy_Code = Code) %>% 
  dplyr::mutate_at(vars(population), ~replace_na(., 1)) %>% 
  dplyr::mutate(infection_rate = cases_14/population * 10000,
                AR_7 = cases_7/population * 10000,
                AR_14 = cases_14/population * 10000,
                AR_cumulative = confirmed/population * 10000,
                AR_active = active/population * 10000)

lisa_bins <- 5

lisa_arQuintiles.df <- lisa_cases.df %>%
  dplyr::select(contains("AR")) %>%
  dplyr::summarize_all(~quantile(., seq(0, 1, 1/lisa_bins)))
lisa_arQuintiles.df[lisa_bins + 1,] <- Inf
