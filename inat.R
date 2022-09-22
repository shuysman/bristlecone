library(rinat)
library(sf)
library(tidyverse)
library(sp)
library(maptools)

pilo <- get_inat_obs(taxon_name = "Pinus longaeva", maxresults = 3000)

pilo_sf <- tibble(pilo) %>%
    filter(!is.na(latitude), !is.na(longitude)) %>%
    select(longitude, latitude, datetime, common_name, scientific_name, image_url, user_login, quality_grade) %>%
    st_as_sf(coords=c("longitude", "latitude"), crs=4326)


