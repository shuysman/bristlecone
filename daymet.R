library(tidyverse)
library(WaterBalance)
library(raster)
library(lubridate)
library(geosphere)

points = read_csv("./sites.csv",
                  col_names = c("site",
                                "lat",
                                "lon",
                                "slope",
                                "aspect",
                                "whc",
                                "wind",
                                "shade",
                                "dro",
                                "t50",
                                "hock",
                                "elev",
                                "mt_range",
                                "owner"),
                  col_types = c("c", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "d", "c", "c"),
                  col_select = c("site":"owner"),
                  skip = 1
                  )

alldaymetdata <- list()
for (i in 1:length(points$site)) {
    daymetdata <- read_csv(file = paste("./daymet/", points$site[i], sep=""), skip = 8,
                           col_names = c("year", "yday", "dayl", "prcp", "srad", "swe", "tmax_C", "tmin_C", "vp"),
                           col_types = c("i", "i", "d", "d", "d", "d", "d", "d", "d"))
    site <- data.frame("site")
    site <- rbind(c(points$site[i]))
    alldaymetdata[[i]] <- cbind(site, daymetdata)
}
alldaymetdata <- as_tibble(do.call(rbind, alldaymetdata))


Soil.Init <- 0

wbdata <- alldaymetdata %>%
    left_join(points, by = "site") %>%
    mutate(wind = NULL,
           vp = vp/1000, #convert to kPa
           srad = srad * 60 * 60 * 24 / 1000000, # convert to MJ m-2 day-1
           tmean_C = (tmax_C + tmin_C) / 2,
           Date = as.Date(yday, origin = paste(as.numeric(year) - 1, "-12-31", sep="")),
           daylength = get_daylength(Date, lat),
           jtemp = get_jtemp(lon, lat),
           F = get_freeze(jtemp, tmean_C),
           RAIN = get_rain(prcp, F),
           SNOW = get_snow(prcp, F),
           MELT = get_melt(tmean_C, jtemp, hock = 4, SNOW),
           PACK = get_snowpack(jtemp, SNOW, MELT),
           W = MELT + RAIN) %>%
    group_by(site, year)

wbdata$ET_Hamon_daily <- ET_Hamon_daily(wbdata)
wbdata$ET_Penman_daily <- ET_PenmanMonteith_daily(wbdata, 2962, 39.9, wind = 1)

wbdata <- wbdata %>%
    mutate(PET = modify_PET(ET_Penman_daily, slope, aspect, lat, F, shade.coeff = 1),          
           W_PET = W - PET,
           SOIL = get_soil(W, Soil.Init, PET, W_PET, whc),
           DSOIL = diff(c(Soil.Init, SOIL)),
           AET = get_AET(W, PET, SOIL, Soil.Init),
           W_ET_DSOIL = W - AET - DSOIL,
           D = PET - AET,
           GDD = get_GDD(tmean_C, 0))

summary_tbl <- wbdata %>%
    summarise(ppt = sum(prcp), T = mean(tmean_C), AET = sum(AET), D = sum(D), GDD = sum(GDD)) %>%
    group_by(site) %>%
    summarise(ppt = mean(ppt), T = mean(T), AET = mean(AET), D = mean(D), GDD = mean(GDD))

find_loc <- function(x) {
    #' Lookup site.csv in locations to determine if in region of interest
    panamint_site_ids <- paste(as.character(seq(219, 243)), ".csv", sep="")
    wah_wah_site_ids <- c("497.csv") #paste(as.character(seq(497, 498)), ".csv", sep="")
    silver_peak_site_ids <- paste(as.character(seq(358, 361)), ".csv", sep="")
    rawhide_site_ids <- paste(as.character(seq(154, 165)), ".csv", sep="")
    case_when(x %in% panamint_site_ids ~ "panamint",
              x %in% wah_wah_site_ids ~ "wah_wah",
              x %in% silver_peak_site_ids ~ "silver_peak",
              x %in% rawhide_site_ids ~ "rawhide",
              TRUE ~ "other")
}

loc_colors <- c("red", "green", "aquamarine", "darkorchid1", "black")

data_long <- summary_tbl %>%
    pivot_longer(c("D", "AET", "ppt", "GDD")) %>%
    mutate(loc = fct_relevel(find_loc(site), c("panamint", "wah_wah", "silver_peak", "rawhide", "other"))) %>%
    group_by(loc) %>%
    arrange(desc(loc))

plts <- data_long %>%
    transform(name=factor(name,levels=c("D", "ppt", "AET"))) %>%
    ggplot() +
    geom_point(mapping = aes(x = T, y = value, color = loc)) +
    scale_color_manual(name = 'Location', values = loc_colors) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    ggtitle("Bio-climatic niche space using DAYMET")
