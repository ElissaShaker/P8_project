###################
#### Hent data ####
###################
setwd("~/Desktop/P8/R")
library(readr)
library(dplyr)
library(magrittr)
library(kableExtra)
library(tidyquant)
library(tidyr)
library(timetk)
library(RQuantLib)
library(ggplot2)
library(scales)
options(scipen =999)

####################
#### Hente Data ####
####################
# OBS!! Only run once
# SPY <- function(strike, expiry) {
#   option_symbol <- paste0("O:SPY", expiry, "C00", strike, "000")
#   from_date <- "2025-04-01"
#   to_date   <- "2025-05-07"
#   
#   API <- paste0(
#     "https://api.polygon.io/v2/aggs/ticker/", option_symbol,
#     "/range/1/minute/", from_date, "/", to_date,
#     "?adjusted=true&sort=asc&limit=500&apiKey=kpGPo6pQ5Pmvta480pA9gBlyCQr07xzk"
#   )
#   
#   # get data from api
#   raw <- tryCatch(jsonlite::fromJSON(API), error = function(e) return(NULL))
#   if (is.null(raw) || is.null(raw$results)) return(NULL)
#   
#   Data <- raw$results
#   
#   Data$strike <- strike
#   Data$expiry <- expiry
#   
#   # save only is the data is a valid data.frame
#   if (is.data.frame(Data)) {
#     filename <- file.path("Data", paste0("SPY_", expiry, "_", strike, ".csv"))
#     readr::write_csv(Data, file = filename)
#     Sys.sleep(30)
#     return(Data)
#   } else {
#     return(NULL)
#   }
# }
# 
# strike <- seq(350, 750, 5)

# 3 months SPY call options
#data_3m_raw <- lapply(strike, function(k) SPY(k, "250815"))

###################
#### Load Data ####
###################
data_folder <- "Data"

# runs all CSV-files, with the matching format "SPY_[expiry]_[strike].csv"
csv_files_3m <- list.files(data_folder, pattern = "SPY_250815_", full.names = TRUE)
data_3m <- bind_rows(lapply(csv_files_3m, read_csv))

# Calculates the time in minutes from last trading day to maturity time
TTM <- function(maturity, last.traded) {
  time_difference <- difftime(maturity, last.traded, units = "mins") %>%
    as.numeric %>%
    return
}

