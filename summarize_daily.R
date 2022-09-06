library(tidyverse)
library(tsibble)
library(tsibbledata)
library(ggplot2)
library(plotly)
library(xts)
library(fable)

panamint_site_ids <- paste(as.character(seq(219, 243)), ".csv", sep="")
wah_wah_site_ids <- c("497.csv") #paste(as.character(seq(497, 498)), ".csv", sep="")
silver_peak_site_ids <- paste(as.character(seq(358, 361)), ".csv", sep="")
rawhide_site_ids <- paste(as.character(seq(154, 165)), ".csv", sep="")

find_loc <- function(x) {
    case_when(x %in% panamint_site_ids ~ "panamint",
              x %in% wah_wah_site_ids ~ "wah_wah",
              x %in% silver_peak_site_ids ~ "silver_peak",
              TRUE ~ "other")
}

daily_tbl <-
    list.files(pattern = "*.csv") %>%
    map_df(~read_csv(., col_types = cols())) %>%
#    mutate(date = as.Date(date)) %>%
    group_by(site)

daily_tbl <- mutate(daily_tbl, date = as.POSIXct(date, format = "%m/%d/%Y"))

tbl_xts <- as.xts(tbl)


find_melt_date <- function(x) {
    ts <- as_tsibble(x)
    span = 3 # number od days of persistent snow used to identify snow season when working backward from summer
    mdate <- 1 # set to 1 which evaluates to Jan 1 for years and places with no snow

    return("test")
}
