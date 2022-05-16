
# Install packages (run only once) ----------------------------------------

# install.packages("drat")
# drat:::add("epiforecasts")
# install.packages("EpiNow")
# install.packages("NCoVUtils")
# install.packages("EpiSoon")


# Load packages -----------------------------------------------------------

packages <- c("EpiNow",
              "EpiSoon",
              "readr",
              "dplyr",
              "tidyr",
              "data.table",
              "lubridate",
              "future",
              "forecastHybrid",
              "NCoVUtils",
              "magrittr",
              "furrr",
              "future.apply",
              "fable",
              "fabletools",
              "feasts",
              "urca")

lapply(packages, require, character.only = TRUE)



# Prepare data and delays -------------------------------------------------

data.df <- read.csv("./Data/20200728.csv", na.strings = c("", "NA")) %>%
# data.df <- read.csv("EpiNow/Data/ConfirmedCases.csv") %>%
  as.data.frame() %>% 
  mutate_at(vars(contains("Date")), as.Date) %>% 
  mutate(cityPSGC2 = ifelse(provincePSGC == 133900000, 133900000, cityPSGC),
         CityMunicipality2 = ifelse(provincePSGC == 133900000, "City of Manila", CityMunicipality))

data.df <- data.df %>%
  transmute(X = seq_along(Case_Number),
            casenumber = paste0(X, "PH" ),
            currentregion = Region,
            currentmuncity = cityPSGC2,
            onset_date = Date_Onset,
            report_date = Report_Date,
            healthstatus = Status,
            died_date = Date_Died,
            age = Age,
            sex = Sex,
            repatriate = Repatriate) %>% 
  mutate(repatriate = case_when(tolower(repatriate) == "yes" ~ "Repatriate",
                                TRUE ~ "NA"))
data.df$repatriate[data.df$repatriate == "NA"] <- NA


# Update Delays -----------------------------------------------------------

data <- data.df
data$report_date <- lubridate::ymd(data$report_date)
data$onset_date <- lubridate::ymd(data$onset_date)
data$report_date <- lubridate::ymd(data$report_date)

#Process the line list for report delays
data <-  data %>%
  select(onset_date,report_date,currentmuncity) %>%
  rename(date_onset = onset_date,date_confirm = report_date,region = currentmuncity) %>%
  dplyr::filter(!is.na(date_onset)) %>%
  mutate(report_delay = as.numeric(date_confirm - date_onset)) 

#Get the delay distribution for all regions:
delay_defs <- EpiNow::get_dist_def(data$report_delay,
                                   bootstraps = 100, 
                                   samples = 1000)

saveRDS(delay_defs, "local_delays.rds")


# Function: Get Regional Cases  -------------------------------------------

get_phl_regional_cases <- function(country, regionType, dataSource, dateRun) {
  # Get cases ---------------------------------------------------------------
  # Only have regional data at the local level
    data <- data.df
    data$report_date <- lubridate::ymd(data$report_date)
    
    inc_obj <- incidence::incidence(data$report_date, groups = data$currentmuncity)
    cases <- as_tibble(inc_obj)
    #melt to transform to long format data:
    cases <- reshape2::melt(cases,id.vars = c('dates'))
    #covert factors back to characters:
    cases$variable <- levels(cases$variable)[cases$variable]
    cases <- cases %>%
      dplyr::rename(region = variable, confirm = value, date = dates) %>%
      mutate(import_status = "local") %>%
      # tidyr::drop_na(region) %>%
      dplyr::filter(region != "NA") %>%
      as_tibble()
    
    casesImport <- cases %>%
      dplyr::mutate(confirm = 0, import_status = "imported")
    
    cases <- dplyr::bind_rows(cases, casesImport) %>%
      arrange(region)
  return(cases)
}

# Defined variables -------------------------------------------------------

delay_defs <- readRDS("local_delays.rds")
country <- "Philippines"
regionType <- "regional" # "national" or "regional"
dataSource <- "local" # "ecdc", "jhu", "local"
dateRun <- NULL #"2020-05-28" # Run to this date: NULL = last date in data file
lineData <- "local" # "international, "local"


# Get cases ---------------------------------------------------------------

NCoVUtils::reset_cache()

unlink(sprintf("./regional", i), recursive = TRUE)
dir.create("./regional")

cases <- get_phl_regional_cases(country, regionType, dataSource, dateRun) %>%
  dplyr::filter(!is.na(confirm))

region_codes <- cases %>%
  dplyr::select(region) %>%
  unique()

saveRDS(region_codes, "Data/region_codes.rds")

cases <- as.data.table(cases)

# Set up cores ------------------------------------------------------------

if (!interactive()){
  options(future.fork.enable = TRUE)
}

future::plan("multiprocess", workers = round(future::availableCores() / 2))

# Run pipeline ------------------------------------------------------------

EpiNow::regional_rt_pipeline(
  cases = cases,
  delay_defs = delay_defs,
  # incubation_defs = incubation_defs,
  target_folder = "regional",
  case_limit = 20,
  approx_delay = TRUE,
  report_forecast = TRUE,
  horizon = 19, 
  nowcast_lag = 12,
  min_forecast_cases = 20,
  forecast_model = function(y, ...){EpiSoon::forecastHybrid_model(
    y = y[max(1, length(y) - 21):length(y)],
    model_params = list(models = "aefz", weights = "equal"),
    forecast_params = list(PI.combination = "mean"), ...)})


# Remove erroneous folders ------------------------------------------------

for(i in list.files(path = "./regional")){
  if (!("latest" %in% list.files(path = sprintf("./regional/%s", i)))){
    unlink(sprintf("./regional/%s", i), recursive = TRUE)
  }
}

# Summarize results -------------------------------------------------------

EpiNow::regional_summary(
  results_dir = "regional",
  summary_dir = ifelse(is.null(dateRun), "regional-summary/latest",
                       paste0("regional-summary", dateRun)),
  target_date = ifelse(is.null(dateRun), "latest", dateRun),
  region_scale = "Region")