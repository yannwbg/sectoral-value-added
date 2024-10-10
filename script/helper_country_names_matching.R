library(purrr)
library(dplyr)
library(readr)

#Set parameters----
data_path <- "data/raw/global/"
url <- "https://databank.worldbank.org/data/download/WDI_EXCEL.zip" 
un_files <- list.files(path =  data_path, pattern = "UN", full.names = T)

#Get WDI country list (iso3 and names)----
temp_file <- tempfile()
temp_dir <- tempdir()
download.file(url, destfile = temp_file) 
unzip(temp_file, exdir = temp_dir)

country_list_wb <- read_xlsx(list.files(temp_dir, "WDI", full.names = TRUE), sheet = "Country") %>%
  select(iso3 = "Country Code",
         country = "Table Name",
         region = "Region") %>% #We keep that one as a way of discarding regions from the iso3 and country columns
  filter(!is.na(region)) %>%
  select(-region)

#Get UN country list----
country_list_un <- map(un_files, function(x) {
  read_csv(x, col_types = "c") %>%
    select(country = "Country or Area") %>%
    filter(!str_detect(country, "[0-9]") & country != "footnote_SeqID") %>%
    distinct()
}) %>%
  do.call(rbind, .) %>%
  distinct()

#Get OECD country list----  
country_list_oecd <- read_csv(paste(data_path, "OECD.csv", sep = "")) %>%
  select(iso3 = "REF_AREA",
         country = "Reference area") %>%
  filter(!str_detect(country, "[0-9]")) %>%
  distinct()

#Create matching dataset----

unique(country_list_oecd$iso3 %in% country_list_wb$iso3) #TRUE => We can use iso3 as unique id

countries_un_not_in_wb <- setdiff(country_list_un$country, country_list_wb$country)
countries_wb_not_in_un <- setdiff(country_list_wb$country, country_list_un$country)

sort(countries_un_not_in_wb)
sort(countries_wb_not_in_un)

country_list <- country_list_un %>%
  mutate(wb_name = case_when(!(country %in% countries_un_not_in_wb) ~ country,
                             country == "Bahamas" ~ "Bahamas, The",
                             country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                             country == "China, Hong Kong Special Administrative Region" ~ "Hong Kong SAR, China",
                             country == "China, Macao Special Administrative Region" ~ "Macao SAR, China",
                             country == "Congo" ~ "Congo, Rep.",
                             country == "Democratic Republic of the Congo" ~ "Congo, Dem. Rep.",
                             country == "Egypt" ~ "Egypt, Arab Rep.",
                             country == "Gambia" ~ "Gambia, The",
                             country == "Iran (Islamic Republic of)" ~ "Iran, Islamic Rep.",
                             country == "Kyrgyzstan" ~ "Kyrgyz Republic",
                             country == "Lao People's Democratic Republic" ~ "Lao PDR",
                             country == "Micronesia (Federated States of)" ~ "Micronesia, Fed. Sts.",
                             country == "Republic of Korea" ~ "Korea, Rep.",
                             country == "Republic of Moldova" ~ "Moldova",
                             country == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
                             country == "Saint Lucia" ~ "St. Lucia",
                             country == "Saint Vincent and the Grenadines" ~ "St. Vincent and the Grenadines",
                             country == "Sao Tome and Principe" ~ "São Tomé and Principe",
                             country == "Sint Maarten" ~ "Sint Maarten (Dutch part)",
                             country == "Slovakia" ~  "Slovak Republic",
                             country == "State of Palestine" ~ "West Bank and Gaza",
                             country == "Tanzania - Mainland" ~ "Tanzania",
                             country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela, RB",
                             country == "Yemen" ~ "Yemen, Rep.",
                             TRUE ~ NA),
         un_name = country) %>%
  select(un_name, wb_name) %>%
  filter(!is.na(wb_name)) %>%
  full_join(country_list_wb, by = c("wb_name" = "country"), keep = FALSE) %>%
  full_join(country_list_oecd, by = "iso3") %>%
  select(iso3, wb_name, un_name, oecd_name = country)

germany <-  map(un_files, function(x) {
  read_csv(x) %>%
    select(country = "Country or Area",
           everything()) %>%
    filter(country == "Germany, Federal Republic of")
})

# "Germany, Federal Republic of" check
unique(germany[[1]]$`Fiscal Year`)
unique(germany[[4]]$Year)
#Only present in UN3 data which has already been filtered to only include year >= 1990. "Germany, Federal Republic of" only has data for 1990:1994

#Note:
# 1) The rest of unmatched countries from UN are either French, Dutch or British -
# Overseas territories or do not exist anymore (Yugoslavia, Serbia and Montenegro, and Germany, Federal Republic of which refers to former West Germany) -
# with the exception of Cook Islands, Niue and Zanzibar (which will need to be -
# dealt with individually to merge with Tanzania).

#Save data----

write_csv(country_list, "data/processed/country_list.csv")
