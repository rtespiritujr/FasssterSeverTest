#------------------------------------------------------------------------------
# Initialization
#------------------------------------------------------------------------------
# Install Packages
packages <- c("tmap", "spdep", "dplyr", "sf", "readr", "tidyr", "spdyn")
# # install.packages(packages)
#install.packages("spdyn", repos="http://R-Forge.R-project.org")
lapply(packages, require, character.only = TRUE)

# tmap_mode("view") # uncomment for interactive map in (html type)

# setwd("C:/Users/LS-MATH/Documents/FASSSTER/LISA and CT")


# Function for Local Moran's I --------------------------------------------

compute_moran <- function(city_psgc){
  city_psgc <- as.integer(city_psgc)
  
  # Extract province code from the city psgc
  province_code <- floor(city_psgc/100000)*100000 %>% as.integer()
  
  # Neighboring provinces
  province_nb.l <- c(province_code, read.csv(sprintf("spatialautocorrelation/Data/Province Neighborhood/%s.csv", province_code))) %>% unlist()
  
  # Subset of barangays in the province and neighboring provinces
  map_subset.sf <- subset(map.sf, Pro_Code %in% province_nb.l)
  
  # List of barangay neighbors for each barangay in the provinces
  expanded_nb.l <- poly2nb(map_subset.sf, queen = TRUE)
  
  names(expanded_nb.l) <- map_subset.sf$Bgy_Code
  
  # List of barangays in the chosen city
  bgy.l <- map.sf$Bgy_Code[map.sf$Mun_Code2 == city_psgc & !is.na(map.sf$Bgy_Code)] %>% unique()
  
  
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
  moran_df <- map_EBsubset.sf %>%
    dplyr::left_join(cases_df, by = "Bgy_Code") %>%
    dplyr::mutate(population = case_when(is.na(population) ~ 1,
                                         TRUE ~ population)) %>% 
    dplyr::mutate_at(vars(contains("cases"),
                          confirmed), ~replace_na(., 0))
  
  # Neighborhood list of the expanded boundary (2 steps) to be used in EB
  EB_nb.l <- poly2nb(map_EBsubset.sf, queen = TRUE)
  
  # Empirical Baye's estimate to address outliers
  EB_results <- EBlocal(moran_df$cases_14,
                        moran_df$population,
                        EB_nb.l,
                        zero.policy = TRUE) 
  
  # Replace NaN density with 0
  moran_df$EB <- EB_results$est %>% replace_na(0) 
  
  # Scaled EB for easier classification
  moran_df$sEB <- scale(moran_df$EB) %>% replace_na(0)
  
  # Local Moran's I ---------------------------------------------------------
  
  # After computing EB estimates, setup dataframe for Moran's I
  moran_df <- moran_df %>% filter(Bgy_Code %in% extension1_bgy.l)
  
  # Map data should only include until 1 barangay outside the boundary
  map_moran.sf <- map_EBsubset.sf %>% filter(Bgy_Code %in% extension1_bgy.l)
  
  # Neighborhood list of 1 step outside the boundary
  moran_nb.l <- poly2nb(map_moran.sf)
  
  # Indices for barangays with neighbors
  moran_bgy.i <- unlist(lapply(moran_nb.l, sum)) != 0 
  
  # Remove barangays without neighbors
  map_moran.sf <- map_moran.sf[moran_bgy.i, ] 
  
  # Store data of neighborless barangays
  complement_df <- moran_df %>% filter(!(Bgy_Code %in% map_moran.sf$Bgy_Code))
  
  # Keep data of barangays with neighbors
  moran_df <- moran_df %>% filter(Bgy_Code %in% map_moran.sf$Bgy_Code)
  
  # Get neighborhood list for barangays with neighbors (input for spatial weights)
  moran_nb.l <- poly2nb(map_moran.sf)
  
  # Spatial weights
  weights.l <- nb2listw(moran_nb.l, style = "W", zero.policy = TRUE) # B for binary also can try W to be 1/(# of neighbors)
  
  # Spatially lagged values
  moran_df$lag_sEB <- lag.listw(weights.l, moran_df$sEB, zero.policy = TRUE) 
  
  # Local Moran's I with p-values from the empirical distribution
  local_I <- lisa.perm(moran_df$EB, weights.l, perm = 9999)
  
  # Local Moran's I value
  moran_df$local_I <- local_I[,1] 
  
  # Local Moran's I p-values using empirical distribution
  moran_df$local_p <- local_I[,2] %>% p.adjustSP(moran_nb.l, method = "fdr") 
  
  # Merge the dataframe with computed statistics to the neighborless barangays
  if (nrow(complement_df) > 0) {
    complement_df$lag_sEB <- NA
    complement_df$local_I <- NA
    complement_df$local_p <- NA
    moran_df <- rbind(moran_df, complement_df)
  }
  
  
  # After computing Moran's I, only retain rows that belong to the chosen city
  moran_df <- moran_df %>% dplyr::filter(Mun_Code2 == city_psgc)
  
  # Identify clusters
  moran_df$local_cluster <- as.factor(ifelse(moran_df$local_p <= 0.05,
                                             "Cluster", NA))
  
  # Assign classifications for easier visualization
  # HH = 1
  # HL = 2
  # LH = 3
  # LL = 4
  
  moran_df$classification <- ifelse(moran_df$local_cluster=="Cluster",
                                    ifelse(moran_df$sEB >= 0,
                                           ifelse(moran_df$lag_sEB >= 0, 1, 2), # 1 is HH, 2 is HL
                                           ifelse(moran_df$lag_sEB >= 0, 3, 4)), NA) # 3 is LH 4 is LL
  
  
  output = list(moran_df, weights.l)
  names(output) = c("data.frame", "contiguity")
  
  
  
  return(output)
}


# Function to Export csv --------------------------------------------------

# export_moran <- function(results, city_psgc){
#   result_df <- results$data.frame %>% data.frame() %>% 
#     dplyr::select(Bgy_Code,
#                   contains("cases"),
#                   infection_rate,
#                   classification,
#                   contains("AR"))
#   file_name <- sprintf("%s from %s to %s.csv", city_psgc, start_date, end_date)
#   write_csv(result_df, file_name)
# }



# Import and Build Dataframe ----------------------------------------------

# Import map data and create a new variable so that Manila will be treated as a city
map.sf <- st_read("spatialautocorrelation/Data/shapefiles/municipality-shapefile.shp") %>% 
  dplyr::mutate(Mun_Code2 = case_when(Pro_Code == 133900000 ~ 133900000,
                                      TRUE ~ mun_code))

# Import population data, replace 0 population with 1 so that it won't cause errors in the computation
# pop_df <- read_csv("./Data/Brgy2015Pop.csv") %>%
#   dplyr::mutate_at(vars(contains("_Code")), list(~as.double(gsub("PH", "", .)))) %>%
#   dplyr::select(Bgy_Code, POP2015) %>%
#   drop_na()
# pop_df[pop_df == 0] <- 1 # replace the 0 population barangays with 1

pop_df <- read_csv("spatialautocorrelation/Data/brgy_pop.csv") %>%
  dplyr::select(Bgy_Code, POP2015) %>%
  drop_na()
pop_df[pop_df==0] <- 1 # replace the 0 population barangays with 1


data_df <- read.csv("spatialautocorrelation/Data/TKC_Linelist_Suspect_Probable.csv", na.strings = c("NA", ""))
# colnames(data_df) <- gsub(" ", ".", colnames(data_df))

data_df <- data_df %>%
  dplyr::mutate_at(vars(contains("_date")), as.Date) %>% 
  dplyr::mutate(cityPSGC2 = Res_CityMunPSGC) #Revisit for Manila

colnames(data_df)[1]<-"Report_Date" ##revisit this, 

end_date <-  max(data_df$Report_Date, na.rm = TRUE)# can change to a different
start_date <- end_date - 13 # entire period is 2 weeks

cases_df <- data_df %>%
  dplyr::filter(Report_Date >= start_date, Report_Date <= end_date) %>%  # get cases that occurred within the time period
  dplyr::group_by(Res_BarangayPSGC) %>% # group according to barangays
  dplyr::summarize(cases_7 = sum(Report_Date >= start_date + 7),
                   cases_14 = n()) %>% # get the count per barangay
  tidyr::drop_na() %>% # to remove the count of NA's
  dplyr::rename(Bgy_Code = Res_BarangayPSGC) # for easier joining

cumulative_cases <- data_df %>%
  dplyr::group_by(Res_BarangayPSGC) %>%
  dplyr::summarize(confirmed = n()) %>%
  dplyr::rename(Bgy_Code = Res_BarangayPSGC)

cases_df <- cases_df %>% 
  dplyr::left_join(cumulative_cases, by = "Bgy_Code") %>%
  dplyr::mutate_at(vars(contains("cases"),
                        confirmed), ~replace_na(., 0)) %>% 
  dplyr::left_join(pop_df, by = "Bgy_Code") %>% # combine with the population 
  dplyr::rename(population = POP2015) %>% 
  dplyr::mutate_at(vars(population), ~replace_na(., 1)) %>% 
  dplyr::mutate(infection_rate = cases_14/population * 10000,
                AR_7 = cases_7/population * 10000,
                AR_14 = cases_14/population * 10000,
                AR_cumulative = confirmed/population * 10000)

bins <- 5

ar_quintiles_df <- cases_df %>%
  dplyr::select(contains("AR")) %>%
  dplyr::summarize_all(~quantile(., seq(0, 1, 1/bins)))
ar_quintiles_df[bins + 1,] <- Inf

########Above code need only be run once#######################################


# Province Neighbors ------------------------------------------------------

# province_map.sf <- st_read("./Data/phl_admbnda_adm2_psa_namria_20200529.shp") %>% 
#   mutate_at(vars(contains("PCODE")), ~as.numeric(gsub("PH","",.))) %>% 
#   rename(Reg_Code = ADM1_PCODE,
#          Pro_Code = ADM2_PCODE)
# 
# province_nb.l <- province_map.sf %>% poly2nb()
# 
# 
# for (i in 1:length(unique(province_map.sf$Pro_Code))){
#   province <- unique(province_map.sf$Pro_Code)[i] %>% as.integer()
#   neighbors <- unique(province_map.sf$Pro_Code)[province_nb.l[[i]]]
#   data.frame(neighbors = neighbors) %>%
#     write_csv(sprintf("Spatial Association/Province Neighborhood/%s.csv",
#                       province))
# }

#------------------------------------------------------------------------------
# User inputs
#------------------------------------------------------------------------------
# PSGC of chosen city.
# city_psgc <- 133900000 # Manila
# city_psgc <- 137404000 # Quezon City
# city_psgc <- 72217000 # Cebu City
# city_psgc <- 54108000

# results <- compute_moran(city_psgc)
# 
#  tm_shape(results$data.frame) +
#     #tm_polygons("cases_14", id = "Bgy_Name", palette = "Reds", area = "areasqkm") +
#     #tm_polygons(col = "cases_14", id = "Bgy_Name", style = "quantile") +
#    tm_polygons(col = "classification", id = "Bgy_Name", title = "Classification",
#                style = "fixed",
#                breaks = c(0, 1.5, 2.5, 3.5, 4.5),
#                showNA = FALSE,
#                labels = c("High-High", "High-Low", "Low-High", "Low-Low"),
#                palette = c("brown", "lightpink", "skyblue2", "blue")) +
#    tm_legend(outside = TRUE)
# 
# output <- results$data.frame %>% data.frame() %>%
#   select(Bgy_Code,
#          contains("cases"),
#          infection_rate,
#          classification,
#          population,
#          contains("AR"))
# 
#  source("./Outputs.R")