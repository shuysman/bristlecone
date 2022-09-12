library(tidyverse)
library(ggplot2)
library(plotly)
library(xts)
library(ggpubr)
library(readxl)
library(metR)
library(lubridate)

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


tbl <-
    list.files(pattern = "*.csv") %>%
    map_df(~read_csv(., n_max = 41, col_types = cols())) %>% # only read 41 rows (For annual reports) because csvs contain nonsynctactic row of averages at bottom, which creates NAs in df
    mutate(year = strtoi(`Row Labels`)) %>%
    group_by(site)

sites_tbl <- read_excel("sites.xlsx")

summary_tbl <- tbl %>%
    summarise(
        mean_t_pre_2010 = mean(`Average of T`[year <= 2010]),
        mean_P_pre_2010 = mean(`Sum of P`[year <= 2010]),
        mean_AET_pre_2010 = mean(`Sum of AET`[year <= 2010]),
        mean_D_pre_2010 = mean(`Sum of D`[year <= 2010]),
        mean_GDD_pre_2010 = mean(`Sum of GDD`[year <= 2010]),
        mean_SOIL_pre_2010 = mean(`Average of SOIL`[year <= 2010]),
###
        mean_t_post_2010 = mean(`Average of T`[year > 2010]),
        mean_P_post_2010 = mean(`Sum of P`[year > 2010]),
        mean_AET_post_2010 = mean(`Sum of AET`[year > 2010]),
        mean_D_post_2010 = mean(`Sum of D`[year > 2010]),
        mean_GDD_post_2010 = mean(`Sum of GDD`[year > 2010]),
        mean_SOIL_post_2010 = mean(`Average of SOIL`[year > 2010]),
###    Delta values pre and post 2010
        dt = mean_t_post_2010 - mean_t_pre_2010,
        dP = mean_P_post_2010 - mean_P_pre_2010,
        dAET = mean_AET_post_2010 - mean_AET_pre_2010,
        dD = mean_D_post_2010 - mean_D_pre_2010,
        dGDD = mean_GDD_post_2010 - mean_GDD_pre_2010,
        dSOIL = mean_SOIL_post_2010 - mean_SOIL_pre_2010,
###
    ) %>%
#    filter(mean_AET_pre_2010 <= 200 | mean_AET_post_2010 <=200) %>%
    mutate(loc = fct_relevel(find_loc(site), c("panamint", "wah_wah", "silver_peak", "rawhide", "other"))) %>%
    group_by(loc) %>%
    arrange(desc(loc)) %>%
    left_join(sites_tbl, by = c("site" = "Site")) 

loc_colors = c("red", "green", "aquamarine", "darkorchid1", "black")

cwd_plt_pre_2010 <- summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_pre_2010, y = mean_D_pre_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (1980-2010)", y = "CWD") +
    scale_color_manual(name = 'Location', values = loc_colors)
    
p_plt_pre_2010 <- summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_pre_2010, y = mean_P_pre_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (1980-2010)", y = "PPT") +
    scale_color_manual(name = 'Location', values = loc_colors)
    
aet_plt_pre_2010 <- summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_pre_2010, y = mean_AET_pre_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (1980-2010)", y = "AET") +
    scale_color_manual(name = 'Location', values = loc_colors)

gdd_plt_pre_2010 <-summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_boxplot(mapping = aes(x = loc, y = mean_GDD_pre_2010))
                 
soil_plt_pre_2010 <-summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_pre_2010, y = mean_SOIL_pre_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (1980-2010)", y = "SOIL") +
    scale_color_manual(name = 'Location', values = loc_colors)

soil_plt_pre_2010

figure_pre_2010 <- ggarrange(cwd_plt_pre_2010, p_plt_pre_2010, aet_plt_pre_2010, gdd_plt_pre_2010, soil_plt_pre_2010,
                             common.legend = TRUE,
                             labels = c("A", "B", "C", "D", "E"))

figure_pre_2010

cwd_plt_post_2010 <- summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_post_2010, y = mean_D_post_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (2011-2020)", y = "CWD") +
    scale_color_manual(name = 'Location', values = loc_colors)

p_plt_post_2010 <- summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_post_2010, y = mean_P_post_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (2011-2020)", y = "PPT") +
    scale_color_manual(name = 'Location', values = loc_colors)

aet_plt_post_2010 <- summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_post_2010, y = mean_AET_post_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (2011-2020)", y = "AET") +
    scale_color_manual(name = 'Location', values = loc_colors)

gdd_plt_post_2010 <-summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_post_2010, y = mean_GDD_post_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (2011-2020)", y = "GDD") +
    scale_color_manual(name = 'Location', values = loc_colors)

soil_plt_post_2010 <-summary_tbl %>%
    ggplot(mapping = aes(label = site)) +
    geom_point(mapping = aes(x = mean_t_post_2010, y = mean_SOIL_post_2010, color = loc)) +
    labs(x = "Mean Annual Temperature C (2011-2020)", y = "SOIL") +
    scale_color_manual(name = 'Location', values = loc_colors)

soil_plt_post_2010

figure_post_2010 <- ggarrange(cwd_plt_post_2010, p_plt_post_2010, aet_plt_post_2010, gdd_plt_post_2010, soil_plt_post_2010,
                             common.legend = TRUE,
                             labels = c("A", "B", "C", "D", "E"))

figure_post_2010

dcwd_plt <- summary_tbl %>%
    ggplot(aes(x = mean_D_pre_2010, y = mean_AET_pre_2010, color = loc)) +
    geom_arrow(aes(dx = dD, dy = dAET)) +
    labs(title = "Change in mean AET and CWD over periods from 2010-2021 and 1980-2010", x = "CWD (mm)", y = "AET (mm)") +
    scale_mag() +
    scale_color_manual(name = 'Location', values = loc_colors)

# sites_tbl %>% group_by(MT_range %>%  summarise(mean_elev = mean(Elev_m), mean_aspect = mean(ASPECT_QGIS)) %>% arrange(desc(mean_elev)) %>% print(n=100)

elev_plt <- summary_tbl %>%
    ungroup %>%
    group_by(MT_range) %>%
    arrange(desc(Elev_m)) %>%
    ggplot(aes(x = site, y = Elev_m, color = MT_range)) +
    geom_point()
