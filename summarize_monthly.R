library(tidyverse)
library(ggplot2)
library(plotly)

panamint_site_ids <- paste(as.character(seq(219, 243)), ".csv", sep="")
wah_wah_site_ids <- paste(as.character(seq(497, 498)), ".csv", sep="")
silver_peak_site_ids <- paste(as.character(seq(358, 361)), ".csv", sep="")

tbl <-
    list.files(pattern = "*.csv") %>%
    map_df(~read_csv(., n_max = 41)) # only read 41 rows (For annual reports) because csvs contain nonsynctactic row of averages at bottom, which creates NAs in df

## tbl2 <- tbl %>%
##     mutate(year = strtoi(unlist(strsplit(`Row Labels`, split="_"))[1])) %>%
##     mutate(month = strtoi(unlist(strsplit(`Row Labels`, split="_"))[2]))


find_loc <- function(x) {
    case_when(x %in% panamint_site_ids ~ "panamint",
              x %in% wah_wah_site_ids ~ "wah_wah",
              x %in% silver_peak_site_ids ~ "silver_peak",
              TRUE ~ "other")
}
        
           
p <- tbl %>%
    mutate(year = strtoi(`Row Labels`)) %>%
    mutate(loc = find_loc(site)) %>%
    filter(year >= 2010) %>%
    group_by(site) %>%
    summarise(mean_t = mean(`Average of T`), mean_p = mean(`Sum of P`), mean_AET = mean(`Sum of AET`), mean_D = mean(`Sum of D`), loc = loc, site=site, n = n()) %>%
    ggplot() +
    geom_point(mapping = aes(x = mean_t, y = mean_AET, color = loc))

ggplotly(p)
