library(tidyverse)
library(openxlsx)

overview <- read.xlsx("Overview.xlsx")

#Get WDI data    
temp_file <- tempfile()
temp_dir <- tempdir()
url <- "https://databank.worldbank.org/data/download/WDI_EXCEL.zip" 
download.file(url, destfile = temp_file) 
unzip(temp_file, exdir = temp_dir)

#Country list
country_list <- read.xlsx(list.files(temp_dir, "WDI", full.names = TRUE), sheet = "Country") %>%
  select(iso3 = "Country Code",
         country = "Table Name",
         region = Region,
         income_group = "Income Group") %>%
  filter(!is.na(region) & iso3 %in% overview$iso3)

region_list <- read.xlsx(list.files(temp_dir, "WDI", full.names = TRUE), sheet = "Country") %>%
  select(reg3 = "Country Code",
         name = "Table Name",
         region = Region) %>%
  filter(is.na(region) & name %in% country_list$region) %>%
  select(reg3, name)

country_list <- country_list %>%
  left_join(region_list, by = c("region" = "name")) %>%
  select(iso3, country, reg3, region, income_group)

#ETD
ETD <- read.xlsx("1 - Data/1 - Raw/Global/ETD.xlsx", sheet = "Data") %>%
  select(iso3 = cnt, country, var, year, 
         agr = Agriculture, min = Mining, 
         man = Manufacturing, utl = Utilities,
         ctr = Construction, trd = "Trade services",
         tra = "Transport services",
         bus = "Business services", fin = "Financial services",
         real = "Real estate", gov = "Government services",
         oth = "Other services",
         gva = Total)

ETDTE <- read.xlsx("1 - Data/1 - Raw/Global/ETDTE.xlsx", sheet = "ETD_TE") %>%
  select(iso3 = cnt, country, var, year, 
         agr = Agriculture, min = Mining, 
         man = Manufacturing, utl = Utilities,
         ctr = Construction, trd = Trade,
         tra = Transport,
         bus = Business, fin = Finance,
         real = Realestate, gov = Government,
         oth = Other,
         gva = Total)

ETD_coverage <- ETD %>%
  bind_rows(ETDTE) %>%
  filter(!is.na(gva)) %>%
  group_by(iso3) %>%
  summarise(min_year = min(year),
            max_year = max(year))
  
country_list_ETD <- country_list %>%
  left_join(ETD_coverage, by = "iso3") %>%
  mutate(global = ifelse(!is.na(min_year), "ETD", NA)) %>%
  select(1:5, global, global_min_year = min_year, global_max_year = max_year)

#OECD

OECD <- read_csv("1 - Data/1 - Raw/Global/OECD.csv") %>%
  select(iso3 = REF_AREA, country = "Reference area", sector = "Economic activity", value = OBS_VALUE, year = TIME_PERIOD) %>%
  pivot_wider(id_cols = c(iso3, country, year), names_from = "sector", values_from = "value") %>%
  filter(!is.na(.$"Agriculture, forestry and fishing")) %>% #Making the assumption that if agriculture is missing there is a high likelihood that other sectors will be too
  group_by(iso3) %>%
  summarise(min_year = min(year),
            max_year = max(year))

country_list_ETD_OECD <- country_list_ETD %>%
  left_join(OECD, by = "iso3") %>%
  group_by(iso3) %>%
  mutate(global = ifelse(!is.na(min_year), ifelse(!is.na(global), "ETD, OECD", "OECD"), global),
         global_min_year = ifelse(!is.na(global_min_year) | !is.na(min_year),min(global_min_year, min_year, na.rm = TRUE), NA),
         global_max_year = ifelse(!is.na(global_max_year) | !is.na(max_year),max(global_max_year, max_year, na.rm = TRUE), NA)) %>%
  select(-c(min_year, max_year))

#UN
UN_curr <- read_csv("1 - Data/1 - Raw/Global/UNdata_current.csv") %>%
  filter(Series == 1000 & .$"SNA93 Item Code" == "A" & !is.na(Value)) %>%
  group_by(.$"Country or Area") %>%
  summarise(min_year_curr = min(Year),
            max_year_curr = max(Year)) #For some reasons, I'm missing Puerto Rico even though data is available


UN_const <- read_csv("1 - Data/1 - Raw/Global/UNdata_constant.csv") %>%
  select(year = "Fiscal Year", everything()) %>%
  filter(Series == 1000 & .$"SNA93 Item Code" == "A" & !is.na(Value)) %>%
  group_by(.$"Country or Area") %>%
  summarise(min_year_const = min(year),
            max_year_const = max(year))

UN <- UN_curr %>%
  full_join(UN_const, by = .$"Country or Area") %>%
  select(country = '.$"Country or Area"', everything())

country_list <- read.xlsx("Data summary.xlsx", sheet = "Summary", rows = 1:161, cols = 1:14)

#Check if there are any country with different spelling
diff_UN <- tibble(country = setdiff(UN$country, country_list$country))
diff_country_list <- tibble(country = setdiff(country_list$country, UN$country)) 

UN <- UN %>%
  mutate(country = case_when(country == "China, Hong Kong Special Administrative Region" ~ "Hong Kong SAR, China",
                             country == "Gambia" ~ "Gambia, The",
                             country == "Iran (Islamic Republic of)" ~ "Iran, Islamic Rep.",
                             country == "Lao People's Democratic Republic" ~ "Lao PDR",
                             country == "Republic of Korea" ~ "Korea, Rep.",
                             country == "Republic of Moldova" ~ "Moldova",
                             country == "Slovakia" ~ "Slovak Republic",
                             country == "State of Palestine" ~ "West Bank and Gaza",
                             country == "Tanzania - Mainland" ~ "Tanzania",
                             TRUE ~ country)) %>%
  drop_na() %>%
  group_by(country) %>%
  mutate(min_year = ifelse(min_year_const - min_year_curr != 1, max(min_year_const, min_year_curr), min_year_curr), #This basically assumes that if min_year_const - min_year_curr = 1, it is likely that min_year_curr is the base year
         max_year = min(max_year_curr, max_year_const)) %>%
  select(country, min_year, max_year)

country_list_ETD_OECD_UN <- country_list %>%
  select(iso3, country, reg3, region, income_group, sectors_12 = "12+ sectors", everything()) %>%
  left_join(UN, by = "country") %>%
  group_by(iso3) %>%
  mutate(global = ifelse(!is.na(min_year), 
                         case_when(global == "ETD" ~ "ETD, UN",
                                   global == "OECD" ~ "OECD, UN",
                                   global == "ETD, OECD" ~ "ETD, OECD, UN",
                                   TRUE ~ "UN"),
                         global),
         global_min_year = ifelse(!is.na(global_min_year) | !is.na(min_year), min(global_min_year, min_year, na.rm = TRUE), NA),
         global_max_year = ifelse(!is.na(global_max_year) | !is.na(max_year),max(global_max_year, max_year, na.rm = TRUE), NA),
         sectors_12 = ifelse(!is.na(min_year), "Y", sectors_12)) %>%
  select(-c(min_year, max_year))

write.xlsx(country_list_ETD_OECD_UN, "data_summary.xlsx")
