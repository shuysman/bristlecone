library(tidyverse)
library(ggplot2)
library(plotly)
library(xts)

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

tbl <-
    list.files(pattern = "*.csv") %>%
    map_df(~read_csv(.)) %>%
    group_by(site)

tbl <- mutate(tbl, date = as.POSIXct(date, format = "%m/%d/%Y"))

tbl_xts <- as.xts(tbl)
