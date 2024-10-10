library(tidyverse)

#Clean data----

#Load country list 
country_list <- read_csv("data/processed/country_list.csv") %>%
  select(iso3, wb_name, oecd_name)

#Load original data
raw <- read_csv("data/raw/global/OECD.csv") %>%
  select(iso3 = "REF_AREA",
         country = "Reference area",
         code = "ACTIVITY",
         label = "Economic activity",
         currency_type = "UNIT_MEASURE",
         currency_type_label = "Unit of measure",
         series_type = "PRICE_BASE",
         series_type_label = "Price base",
         year = "TIME_PERIOD",
         value = "OBS_VALUE",
         base = "REF_YEAR_PRICE",
         unit = "UNIT_MULT",
         unit_label = "Unit multiplier",
         currency = "CURRENCY",
         currency_label = "Currency",
         note = "Observation status") #All other variables are identical across all rows

sector <- unique(raw[!is.na(raw$code), c("code", "label")])

#Filter relevant years and join by iso3 codes to get wb_name

data <- raw %>%
  filter(year >= 1990) %>%
  left_join(country_list, by = c("iso3"), keep = FALSE) %>%
  filter(!is.na(wb_name)) %>%
  mutate(country = wb_name) %>%
  select(iso3, everything(), - c(wb_name, oecd_name))

#Save cleaned and ancillary datasets

write_csv(data, "data/temp/oecd.csv")
write_csv(sector, "data/temp/oecd_sector.csv")
