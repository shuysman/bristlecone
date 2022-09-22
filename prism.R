library(prism)
library(plotly)
library(raster)
library(ggplot2)
library(tidyverse)
library(lubridate)

#> Be sure to set the download folder using `prism_set_dl_dir()`.
prism_set_dl_dir("~/prismtmp")

## points<-data.frame(id = c(1, 2),
##                    Lat = c( 44.60315, 36.17919104),
##                    Lon = c( -113.01405, -117.085491))
## points.spdf <- SpatialPointsDataFrame(coords = points[,c('Lon', 'Lat')],
##                                       data = points, proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

points <- read_csv("./sites.csv")
points.spdf <- SpatialPointsDataFrame(coords = points[,c('Lon', 'Lat')],
                                      data = points, proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

get_prism_normals("ppt", "800m", annual = TRUE, keepZip = FALSE)
get_prism_normals("ppt", "800m", mon = 1:12, keepZip = FALSE)
get_prism_normals("tmean", "800m", annual = TRUE, keepZip = FALSE)
get_prism_normals("tmean", "800m", mon = 1:12, keepZip = FALSE)

res <- data.frame()
for (i in 1:12) {
    ppt_pd <- prism_archive_subset("ppt", "monthly normals", resolution = "800m", mon = i)
    ppt_pd <- pd_to_file(ppt_pd)
    ppt_pd_rast <- raster(ppt_pd)
    ppt <- raster::extract(ppt_pd_rast, points.spdf, fun=mean, na.rm=TRUE, sp=FALSE)
    tmean_pd <- prism_archive_subset("tmean", "monthly normals", resolution = "800m", mon = i)
    tmean_pd <- pd_to_file(tmean_pd)
    tmean_pd_rast <- raster(tmean_pd)
    tmean <- raster::extract(tmean_pd_rast, points.spdf, fun=mean, na.rm=TRUE, sp=FALSE)
    df <- data.frame(Site = points$Site, Lat = points$Lat, Lon = points$Lon, mon = i, ppt = ppt, tmean = tmean)
    res <- rbind(res, df)
}


get_f <- function (tmean) {
    f <- case_when(
        tmean <= 0 ~ 0,
        tmean > 0 & tmean < 6 ~ 0.167 * tmean,
        tmean >= 6 ~ 1)
    return(f)
}

get_rain <- function (ppt, F) {
    return(F * ppt)
}

get_snow <- function (ppt, F) {
    return( (1 - F) * ppt )
}

get_pack <- function (ppt, F, sp.0=NULL) {
    snowpack <- vector()
    sp.0 <- ifelse(!is.null(sp.0), sp.0, 0)
    for (i in 1:length(ppt)) {
        if (i == 1) {
            snowpack[i] = (1 - F[i])**2 * ppt[i] + (1 - F[i]) * sp.0
        } else {
            snowpack[i] = (1 - F[i])**2 * ppt[i] + (1 - F[i]) * snowpack[i - 1]
        }
    }
    return(snowpack)
}

get_melt <- function (snow, pack, F, sp.0=NULL) {
    sp.0 <- ifelse(!is.null(sp.0), sp.0, 0)
    melt <- vector()
    for (i in 1:length(snow)) {
        if ( i == 1 ) {
            melt[i] = F[i] * (snow[i] + sp.0)
        } else {
            melt[i] = F[i] * (snow[i] + pack[i-1])
        }
    }
    return(melt)
}

get_dl <- function (mon, days, Lat) {
    ## Get Daylength for all days in vector of months
    date <- paste("1980-", mon, "-", days, sep = "")
    yd <- yday(date)
    theta <- 0.2163108+2*atan(0.9671396*tan(0.00860*(yd-186)))
    P <- asin(0.39795 * cos(theta))
    dl <- 24 - (24/pi) * acos((sin((0.8333 * pi)/180) + sin((Lat * pi) / 180) * sin(P))/(cos((Lat*pi)/180)*cos(P)))
    return(dl)
}

get_hl <- function (Lat, slope, aspect_f) {
    ## calculate heat load index multiplier
    Lat.rad <- (pi/180) * Lat
    slope.rad <- (pi/180) * slope
    HL <- 0.339 + 0.808 * (cos(Lat.rad) * cos(slope.rad)) - 0.196 * (sin(Lat.rad) * sin(slope.rad)) - 0.482 * (cos(aspect_f) * sin(slope.rad))
    return(HL)
}

get_soil <- function (soil_max, w, pet, s.0=NULL) {
    s.0 = ifelse(!is.null(s.0), s.0, 0)
    soil <- vector()
    for (i in 1:length(pet)) {
        if ( i == 1 ) {
            soil[i] = pmin(soil_max[i],
                           if (w[i] > pet[i]) {
                               (w[i] - pet[i]) + s.0
                           } else {
                               s.0 * (1 - exp(-(pet[i]-w[i])/soil_max[i]))
                           })
        } else {
            soil[i] = pmin(soil_max[i],
                           if (w[i] > pet[i]) {
                               (w[i] - pet[i]) + soil[i-1]
                           } else {
                               soil[i-1] * (1 - exp(-(pet[i]-w[i])/soil_max[i]))
                           })
        }
    }
    return(soil)
}

get_d_soil <- function (soil, s.0=NULL) {
    s.0 = ifelse(!is.null(s.0), s.0, 0)
    d_soil = soil - lag(soil, default = s.0)
    return(d_soil)
}

get_aet <- function (pet, d_soil, w) {
    aet <- vector()
    for (i in 1:length(pet)) {
        a <- min(pet[i], d_soil[i] + w[i])
        aet[i] = if_else(a > 0,
                         a,
                         0)
    }
    return(aet)
}
                        
result <- res %>%
    left_join(points, by = c("Site" = "Site")) %>%
    mutate(Lat = Lat.x,
           Lon = Lon.x) %>%
    group_by(Site) %>%
    arrange(mon) %>%    
                                        # defaults
    mutate(slope = SLOPE_QGIS,
           aspect = ASPECT_QGIS,
           soil_max = 100,
           hock = 4) %>%    
    mutate(F = get_f(tmean),
           RAIN = get_rain(ppt, F),
           SNOW = get_snow(ppt, F),
           PACK = get_pack(ppt, F),
           MELT = get_melt(SNOW, PACK, F), 
           W = RAIN + MELT,
           Days = days_in_month(mon),
           DL = get_dl(mon, Days, Lat),
           A = abs(180 - abs(aspect - 225)), # folded aspect
           HL = get_hl(Lat, slope, A)) %>%
    mutate(e = 0.611 * exp((17.3 * tmean) / (tmean + 237.3)),
           PET = 29.8 * Days * DL * HL * (e / (tmean + 273.2))) %>%
           #PET = 29.8 * Days * DL * (e / (tmean + 273.2))) %>%
    mutate(SOIL = get_soil(soil_max, W, PET)) %>%
    mutate(dSOIL = get_d_soil(SOIL)) %>%
    mutate(AET = get_aet(PET, dSOIL, W)) %>%
    mutate(D = PET - AET)


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

data_long <- result %>%
    group_by(Site) %>%
    summarise(D = sum(D),
              AET = sum(AET),
              T = mean(tmean),
              ppt = sum(ppt)) %>%
    pivot_longer(c("D", "AET", "ppt")) %>%
    mutate(loc = fct_relevel(find_loc(Site), c("panamint", "wah_wah", "silver_peak", "rawhide", "other"))) %>%
    group_by(loc) %>%
    arrange(desc(loc))

plts <- data_long %>%
    transform(name=factor(name,levels=c("D", "ppt", "AET"))) %>%
    ggplot() +
    geom_point(mapping = aes(x = T, y = value, color = loc)) +
    scale_color_manual(name = 'Location', values = loc_colors) +
    facet_wrap(~ name, ncol = 1, scales = "free") +
    ggtitle("Bio-climatic niche space using PRISM 1991-2020 normals")

ggsave(filename = "bristlecone_aet_cwd_ppt_no-hl.png", plot = plts)


cwd_aet_plt <- result %>%
    group_by(Site) %>%
    filter(Elev_m != TRUE) %>%
    summarise(D = sum(D),
              AET = sum(AET),
              T = mean(tmean),
              ppt = sum(ppt),
              elev = mean(Elev_m)) %>%
    mutate(loc = fct_relevel(find_loc(Site), c("panamint", "wah_wah", "silver_peak", "rawhide", "other"))) %>%
    group_by(loc) %>%
    arrange(desc(loc)) %>%
    ggplot() +
    geom_point(mapping = aes(x = AET, y = D, color = loc)) +
    scale_color_manual(name = 'Location', values = loc_colors)

summary <- result %>%
    group_by(Site) %>%
    filter(Elev_m != TRUE) %>%
    summarise(D = sum(D),
              AET = sum(AET),
              T = mean(tmean),
              ppt = sum(ppt),
              elev = mean(as.double(Elev_m), na.rm = TRUE)) %>%
    mutate(loc = fct_relevel(find_loc(Site), c("panamint", "wah_wah", "silver_peak", "rawhide", "other"))) %>%
    group_by(loc)

fig<- plot_ly(summary, x = ~elev, y = ~D, z = ~AET, alpha = 0.75) %>%
    add_markers(color = ~loc)
