library(tidyverse)
library(stringr)
library(readr)
library(rvest)

url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyc_pluto_21v4_csv.zip"

download.file(url = url, destfile = "nyc_pluto_21v4_csv.zip")

unzip("nyc_pluto_21v4_csv.zip")

ny_housing_raw <- read.csv('pluto_21v4.csv')

ny_housing <- ny_housing_raw
ny_housing <- ny_housing[which(unique(ny_housing$address) %in% ny_housing$address), ]

ny_housing <- ny_housing %>% 
  mutate(
    address_length = sapply(str_split(address, ' '), length)
  ) %>% 
  filter(address_length > 2) %>% 
  select(-address_length)


######################################################
ny_housing_sub <- ny_housing %>% 
  filter(bldgarea > 0) %>% 
  select(borough, zipcode, address, ownername, 
         yearbuilt, bldgclass, bldgarea, numfloors, 
         unitsres, unitstotal, assessland, assesstot,
         latitude, longitude) 

#### Recoding official building classes ####
bldg_classes <- c("commercial", "multifamily", "retail", 
               "office","industrial", "houses", "condos")

ny_housing_sub$bldgclass[which(str_starts(ny_housing_sub$bldgclass, regex("R[5,7,8]")))] <- "commercial"
ny_housing_sub$bldgclass[which(str_detect(ny_housing_sub$bldgclass, 
                                          regex("C7|D6|D7|G8|GU|GW|H[1-5]|H9|HB|HH|HR|HS|J|K[1-9]|P1|R5|R7|
                                    RB|RC|RH|RI|RK|RS")))] <- "commercial"

ny_housing_sub$bldgclass[which(str_starts(ny_housing_sub$bldgclass, regex("O[1-9]")))] <- "office"

ny_housing_sub$bldgclass[which(str_starts(ny_housing_sub$bldgclass, regex("R9|RM|RR|RX|RZ")))] <- "condos"
ny_housing_sub$bldgclass[which(str_starts(ny_housing_sub$bldgclass, regex("R[0-4,6]")))] <- "condos"
ny_housing_sub$bldgclass[which(str_starts(ny_housing_sub$bldgclass, regex("R9")))] <- "coops"
ny_housing_sub$bldgclass[which(str_detect(ny_housing_sub$bldgclass, regex("[A-B]")))] <- "houses"
ny_housing_sub$bldgclass[which(str_detect(ny_housing_sub$bldgclass, regex("C[0-6]|C[8-9]|D[0-5]|D[8-9]|H6|H7|R4|RD|CM")))] <- "multifamily"
ny_housing_sub$bldgclass[which(str_starts(ny_housing_sub$bldgclass, regex("[EFL]|(RW)")))] <- "industrial"
ny_housing_sub$bldgclass[which(!(ny_housing_sub$bldgclass %in% bldg_classes))] <- NA
######################################################

# Run price scraping script
source("scripts/prices_scraping.R")

# Merge real estate dataset with scraped price data
nyc_re_prices <- full_join(ny_housing_sub, price_by_class) %>% 
  filter(!is.na(bldgclass) & !is.na(est_sale_pr_sqft), !is.na(bldgarea)) %>% 
  mutate(
    numfloors = round(numfloors),
    numfloors = replace_na(numfloors, 0),
    est_sale_price = bldgarea * est_sale_pr_sqft
    ) %>% 
  rename(
    building_type = bldgclass,
    building_area_sqft = bldgarea,
    resid_units = unitsres,
    tot_units = unitstotal,
    assess_land_val = assessland,
    assess_taxlot_val = assesstot
  )

write.csv(nyc_re_prices, "nyc_re_prices.csv")


file.remove("nyc_pluto_21v4_csv.zip")
file.remove("PLUTODD21v4.pdf")
file.remove("PlutoReadme21v4.pdf")
file.remove("pluto_21v4.csv")

# rm(list=ls())







