library(tidyverse)
library(haven)
library(readxl)
library(writexl)
library(tools)

#original data (just changed column names for ease)
constant_orig <- read_csv("1 - Data/1 - Raw/Global/UNdata_constant.csv") %>%
  select(country = "Country or Area",
         table_code = "SNA93 Table Code",
         subgroup = "Sub Group",
         label = "Item",
         code = "SNA93 Item Code",
         year = "Fiscal Year",
         series_code = "Series",
         currency = "Currency",
         base = "Base Year",
         SNA = "SNA System",
         year_type = "Fiscal Year Type",
         value = "Value",
         note = "Value Footnotes")

#Get footnotes definitions
constant_footnote <- constant_orig %>%
  filter(str_detect(country, "[0-9]")) %>%
  select(note_code = country,
         note_label = table_code)

sectors <- unique(constant_orig[, c("code", "label")])

#Get data only (without footnotes) and filter relevant years
constant <- constant_orig %>%
  filter(!str_detect(country, "[0-9]") & year >= 1990) %>%
  mutate(country = case_when(country == "China, Hong Kong Special Administrative Region" ~ "Hong Kong SAR, China",
                             country == "Gambia" ~ "Gambia, The",
                             country == "Iran (Islamic Republic of)" ~ "Iran, Islamic Rep.",
                             country == "Kyrgyzstan" ~ "Kyrgyz Republic",
                             country == "Lao People's Democratic Republic" ~ "Lao PDR",
                             country == "Republic of Korea" ~ "Korea, Rep.",
                             country == "Republic of Moldova" ~ "Moldova",
                             country == "Slovakia" ~ "Slovak Republic",
                             country == "State of Palestine" ~ "West Bank and Gaza",
                             country == "Tanzania - Mainland" ~ "Tanzania",
                             TRUE ~ country))

constant_check <- constant %>%
  group_by(country, series_code, year, base) %>%
  mutate(sector = paste(sort(unique(code)), collapse = "+"),
         n_sector = length(unique(code)),
         full_sector = (sector == "A+B+B.1g+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T" | sector == "A+B+B.1g+C+D+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T" | sector == "A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T" | sector == "A+B+C+D+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T"),
         sum_sector = sum(value[code != "B.1g"]),
         total_value = sum(value[code == "B.1g"]),
         sum_check = (abs((sum_sector-total_value)/ total_value) <= 0.05 | total_value == 0),
         sum_check_10 = (abs((sum_sector-total_value)/ total_value) <= 0.1 | total_value == 0)) %>%
  group_by(country, series_code, base) %>%
  mutate(min_year = min(year),
         max_year = max(year),
         n_year = length(unique(year)),
         full_year = n_year == max_year - min_year + 1,
         unique_sector_group = length(unique(sector)) == 1, 
         all_check = (full_sector & sum_check & full_year & unique_sector_group),
         all_but_sector = (sum_check & full_year & unique_sector_group)) %>%
  ungroup() %>%
  mutate()

constant_growth <- constant_check %>%
  filter(all_but_sector == TRUE & code != "B.1g") %>%
  mutate(name = case_when(code == "A" ~ "agr",
                          code == "B" ~ "min",
                          code == "C" ~ "man",
                          code == "D" ~ "utl",
                          code == "E" ~ "utl",
                          code == "F" ~ "ctr",
                          code == "G" ~ "trd",
                          code == "H" ~ "con",
                          code == "I" ~ "hos",
                          code == "J" ~ "con",
                          code == "K" ~ "fin",
                          code == "L" ~ "bus",
                          code == "M+N" ~ "bus",
                          code == "O+P+Q" ~ "adm",
                          code == "R+S+T" ~ "oth")) %>%
  group_by(country, series_code, year, code, base) %>%
  mutate(quality = case_when(!("E" %in% unique(code)) ~ "2",
                             unique(all_check) == TRUE ~ "1")) %>%
  group_by(country, series_code, year, base) %>%
  mutate(quality = paste(sort(unique(quality[!is.na(quality)])), collapse = ", ")) %>%
  group_by(country, year, series_code, name, base, currency, SNA, year_type, sector, n_sector, sum_sector, total_value, all_check, quality) %>%
  summarise(value = sum(value)) %>%
  group_by(country, name, series_code) %>%
  arrange(year) %>%
  mutate(growth_to_next = ifelse(base != -1, (lead(value) - value)/value, NA),
         growth_to_prev = ifelse(base != -1, (value - lag(value))/lag(value), NA)) %>%
  ungroup()

base_1 <- constant_growth %>%
  filter(base == -1) %>%
  left_join(current_series, by = c("country", "year", "name", "series_code"), suffix = c("", "_current")) %>%
  group_by(country, name, series_code, base) %>%
  arrange(year) %>%
  mutate(growth_to_next = ifelse(sector == sector_current, (lead(value) - value_current)/value_current, NA), #making the assumption that series_codes match between current and constant and using sector disaggregation as a check.
         growth_to_prev = ifelse(sector == sector_current, (value - lag(value_current))/lag(value_current), NA),
         base = ifelse((!is.na(growth_to_next) | !is.na(growth_to_prev)), -9999, base)) %>%
  ungroup() %>%
  select(-ends_with("current"))

constant_series <- constant_growth %>%
  filter(base != -1) %>%
  bind_rows(base_1) %>%
  group_by(country, year, series_code, name) %>%
  mutate(base_n = n()) %>%
  group_by(country, year) %>%
  mutate(keep = (series_code == max(series_code) & ifelse(first(base_n[series_code == max(series_code)]) > 1,base == max(base[max(series_code)], na.rm = TRUE), TRUE))) %>%
  group_by(country) %>%
  mutate(main = series_code == max(series_code))

constant_series <- constant_series %>%
  group_by(country, year, series_code, name) %>%
  mutate(base_n = n()) %>%
  group_by(country, year) %>%
  mutate(keep = (series_code == max(series_code) & ifelse(first(base_n[series_code == max(series_code)]) > 1,base == max(base[max(series_code)], na.rm = TRUE), TRUE)))


write_xlsx(constant_series, "1 - Data/2 - Processed/1 - Global/un4_constant_growth.xlsx")
