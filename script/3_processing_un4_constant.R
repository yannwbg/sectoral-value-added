library(tidyverse)

#parameters
sd_threshold <- 0.5 #Threshold used to highlight outliers in terms of standard deviation

#Load data----
data <- read_csv("data/temp/un4_constant.csv")
footnote <- read_csv("data/temp/un4_constant_footnote.csv")
series_code <- read_csv("data/temp/code_constant_4.csv")
current <- read_csv("data/temp/un4_current.csv") #Will be used to compute base -1 constant growth => does not need to be processed as we want to compare same series_code
#sector <- read_csv("data/temp/un4_sector.csv")

#Generate quality check variables----
sector_list <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M+N", "O+P+Q", "R+S+T") 

data_coverage_check <- data %>% 
  #Check sector coverage and validity for each country-year-series
  group_by(country, series_code, year, base) %>%
  mutate(sector_missing_list = list(setdiff(sector_list, unique(code[code != "B.1g"]))), #check which sectors are missing
         sector_missing_n = length(first(sector_missing_list)), #check number of missing sectors
         sector_missing_label = paste(first(sector_missing_list), collapse = ", "),
         sector_full_coverage = ifelse(sector_missing_label %in% c("D", "E"), TRUE, (sector_missing_n == 0)), #We make the assumption that if only one of D or E is present, it includes the other
         sum_sector = sum(value[code != "B.1g"]),
         total_value = sum(value[code == "B.1g"]),
         sum_check = (abs((sum_sector-total_value)/ total_value) <= 0.005 | (total_value == 0 & sector_full_coverage))) %>% #We make the assumption (for now) that if B.1g is not present the sum of sectors is valid if sector_full_coverage is valid
  ungroup() %>%
  #Create sub-series based on unique coverage and base
  group_by(country, series_code, base) %>%
  mutate(subseries = dense_rank(sector_missing_label)) %>% #Create an index for unique sector coverage within a series
  ungroup() %>%
  group_by(country, series_code, subseries, base) %>%
  mutate(year_min = min(year),
         year_max = max(year),
         year_n = length(unique(year)),
         year_full_coverage = year_n == year_max - year_min + 1, #Check that the country-series-subseries does not have gap years
         check_all = (sector_full_coverage & sum_check & year_full_coverage), # = TRUE if all checks are valid
         check_all_but_sector_coverage = (sum_check & year_full_coverage)) %>% # = True if all checks but sector_full_coverage are valid (this allows us to keep corner cases where small economies don't have a specific sector)
  ungroup() 

nrow(data_coverage_check[data_coverage_check$check_all == TRUE,])/nrow(data_coverage_check) * 100 #33.93%
nrow(data_coverage_check[data_coverage_check$check_all_but_sector_coverage == TRUE,])/nrow(data_coverage_check) * 100 #40.11%

#Process data----

#When base is not -1
data_processed_excl_1 <- data_coverage_check %>%
  filter(check_all == TRUE & code != "B.1g" & base != -1) %>%
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
  group_by(iso3, country, year, series_code, subseries, base, name, currency, SNA, year_type, total_value) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(iso3, country, name, series_code, subseries, base) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(value) - value)/value,
         growth_to_prev = (value - lag(value))/lag(value)) %>%
  ungroup()

#When base is -1
current <- current %>%
  filter(code != "B.1g") %>%
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
  group_by(iso3, country, year, series_code, name, currency, SNA, year_type) %>%
  summarise(value = sum(value)) %>%
  ungroup()
  
data_processed_1 <- data_coverage_check %>% 
  filter(check_all == TRUE & code != "B.1g" & base == -1) %>%
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
  group_by(iso3, country, year, series_code, subseries, base, name, currency, SNA, year_type, total_value) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  left_join(current, by = c("country", "year", "name", "series_code", "year_type"), suffix = c("", "_current")) %>%
  group_by(country, name, series_code, subseries, base) %>%
  arrange(year) %>%
  mutate(growth_to_next = ifelse(sector == sector_current, (lead(value) - value_current)/value_current, NA), #making the assumption that series_codes match between current and constant and using sector disaggregation as a check.
         growth_to_prev = ifelse(sector == sector_current, (value - lag(value_current))/lag(value_current), NA),
         base = ifelse((!is.na(growth_to_next) | !is.na(growth_to_prev)), -9999, base)) %>%
  ungroup() %>%
  select(-ends_with("current"))

#Sanity checks----
check <- data_processed %>%
  mutate(mean_prev = mean(growth_to_prev, na.rm = TRUE),
         sd_prev = sd(growth_to_prev, na.rm = TRUE), 
         mean_next = mean(growth_to_next, na.rm = TRUE),
         sd_next = sd(growth_to_next, na.rm = TRUE), 
         check_outliers = !(abs(growth_to_prev - mean_prev) > sd_threshold*sd_prev | abs(growth_to_next - mean_next) > sd_threshold*sd_next)) %>% #Inversed, TRUE = no outliers, FALSE = outliers - makes it easier to filter in final step.
  group_by(iso3, series_code, subseries) %>%
  mutate(min_year = min(year),
         max_year = max(year)) %>%
  ungroup() %>%
  group_by(iso3, series_code, subseries, name) %>%
  arrange(year) %>%
  mutate(check_growth_next = (growth_to_next == lead(growth_to_prev)), #check that growth rate are consistent
         check_growth_prev = (growth_to_prev == lag(growth_to_next)),
         check_year_sector_min = min_year == min(year), #check that the sector coverage is the same of the series coverage
         check_year_sector_max = max_year == max(year)) %>%
  ungroup() %>%
  filter(if_any(all_of(starts_with("check")), ~ . == FALSE)) #44 (86 mirroring) observations are picked up (all for outliers with threshold being 0.5). Will be investigated at a later date

#Select series---- Would need to happen at the beginning once the series-select is finalized
data_selected <- data_processed %>%
  right_join(series_code, by = c("country", "year", "series_code" = "final_series")) %>%
  filter(!(overlap == TRUE & is.na(growth_to_next))) %>%
  select(-c(overlap, "...1"))

#Save----
write_csv(data_selected, "data/processed/un4_constant.csv")
