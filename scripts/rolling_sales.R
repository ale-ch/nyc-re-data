library(readxl)
library(tidyverse)
library(stringr)

url <- "https://data.cityofnewyork.us/api/views/usep-8jbt/rows.csv?accessType=DOWNLOAD"
rolling_sales_ny <- read.csv(url)

names(rolling_sales_ny) <- tolower(names(rolling_sales_ny))
names(rolling_sales_ny) <- str_remove_all(names(rolling_sales_ny), "\\.")

bldg_classes <- c("commercial", "multifamily", "retail", 
                  "office","industrial", "houses", "condos")

rolling_sales_ny <- rolling_sales_ny %>% 
  rename(
    bldgclass = buildingclassatpresent,
    bldgarea = grosssquarefeet,
    unitsres = residentialunits,
    unitscomm = commercialunits,
    unitstotal = totalunits) %>% 
  select(borough, zipcode, address, yearbuilt, 
         bldgclass, bldgarea, unitsres, unitscomm,
         unitstotal, saleprice) %>% 
  filter(saleprice != 0) %>% 
  mutate(
    borough = as_factor(borough),
    saleprice = as.numeric(str_remove_all(saleprice, ","))) %>% 
  mutate(
    borough = fct_recode(borough,
                         "MN" = as.character(1),
                         "BX" = as.character(2),
                         "BK" = as.character(3),
                         "QN" = as.character(4),
                         "SI" = as.character(5)))

rolling_sales_ny$bldgclass[which(str_starts(rolling_sales_ny$bldgclass, regex("O[1-9]")))] <- "office"
rolling_sales_ny$bldgclass[which(str_starts(rolling_sales_ny$bldgclass, regex("R9|RM|RR|RX|RZ")))] <- "condos"
rolling_sales_ny$bldgclass[which(str_starts(rolling_sales_ny$bldgclass, regex("R[0-4,6]")))] <- "condos"
rolling_sales_ny$bldgclass[which(str_starts(rolling_sales_ny$bldgclass, regex("R9")))] <- "coops"
rolling_sales_ny$bldgclass[which(str_detect(rolling_sales_ny$bldgclass, regex("[A-B]|(Z0)")))] <- "houses"
rolling_sales_ny$bldgclass[which(str_detect(rolling_sales_ny$bldgclass, regex("C[0-6]|C[8-9]|D[0-5]|D[8-9]|H6|H7|R4|RD|CM")))] <- "multifamily"
rolling_sales_ny$bldgclass[which(str_starts(rolling_sales_ny$bldgclass, regex("[EFL]|(RW)")))] <- "industrial"
rolling_sales_ny$bldgclass[which(!(rolling_sales_ny$bldgclass %in% bldg_classes))] <- NA

rolling_sales_ny <- rolling_sales_ny %>% 
  filter(!is.na(bldgclass) & !is.na(bldgarea) & bldgarea > 0)









