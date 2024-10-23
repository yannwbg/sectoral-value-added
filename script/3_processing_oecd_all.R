library(tidyverse)

#Parameters
sd_threshold <- 2 #Threshold used to highlight outliers in terms of standard deviation

#Load data----
data <- read_csv("data/temp/oecd.csv")
sector <- read_csv("data/temp/oecd_sector.csv")

#check that country, year, series_type, code is unique (ie do we need to consider other variables as index)
nrow(unique(data[,c("iso3", "code", "year", "series_type")])) == nrow(unique(data[,c("iso3", "code", "year", "series_type", "base", "currency_type", "unit", "currency", "note")])) #TRUE we can group by using country-year-series_type

#Helper functions----
#Function to check composite sectors
check_sectors <- function(sector_list, present_sectors) {
  
  # Check which sectors are missing
  missing_sectors <- setdiff(sector_list, present_sectors)
  
  return(missing_sectors)
}

# Function to generate acceptable combinations of missing sectors
generate_combinations <- function(acceptable_missing_sectors) {
  # Initialize an empty vector to store valid combinations
  valid_combinations <- c()
  
  # Loop through all possible combination lengths
  for (i in 1:length(sectors)) {
    # Generate combinations of length i
    combs <- combn(sectors, i, simplify = FALSE)
    
    # Filter out combinations that contain both D and E
    combs <- combs[!sapply(combs, function(x) all(c("D", "E") %in% x))]
    
    # Convert each combination to a comma-separated string and add to the result
    valid_combinations <- c(valid_combinations, sapply(combs, function(x) paste(sort(x), collapse = ", ")))
  }
  
  return(valid_combinations)
}

#Generate quality check variables----
sector_list <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U")

data_coverage_check <- data %>%
  #Check sector coverage and validity for each country-year-series
  group_by(country, year, series_type) %>%
  mutate(sector_missing_list = list(check_sectors(sector_list, unique(code))),
         sector_missing_n = length(first(sector_missing_list)),
         sector_missing_label = paste(first(sector_missing_list), collapse = ", "),
         sector_full_coverage = ifelse(sector_missing_label %in% generate_combinations(c("D", "E", "P", "Q", "T", "U")), TRUE, (sector_missing_n == 0)), 
         
         #We make the following assumptions: 
         #1) if one of D and E is missing it is included in the other
         #2) if P or Q or both are missing it is included in O
         #3) if T or U or both are missing it is included in S
         #Note: those assumptions are only taken into account if the sum of sectors matches _T when creating the check_all variable
         
         sum_sector = sum(value[code != "_T"]),
         total_value = sum(value[code == "_T"]),
         sum_check = (abs((sum_sector-total_value)/ total_value) <= 0.005 | (total_value == 0 & sector_full_coverage))) %>% #We make the assumption (for now) that if _T is not present the sum of sectors is valid if sector_full_coverage is valid
  ungroup() %>%
  #Create sub-series based on unique coverage
  group_by(country, series_type) %>%
  mutate(subseries = dense_rank(sector_missing_label)) %>% #Create an index for unique sector coverage within a series
  ungroup() %>%
  group_by(country, series_type, subseries) %>%
  mutate(year_min = min(year),
         year_max = max(year),
         year_n = length(unique(year)),
         year_full_coverage = year_n == year_max - year_min + 1, #Check that the country-series-subseries does not have gap years
         check_all = (sector_full_coverage & sum_check & year_full_coverage), # = TRUE if all checks are valid
         check_all_but_sector_coverage = (sum_check & year_full_coverage)) %>% # = True if all checks but sector_full_coverage are valid (this allows us to keep corner cases where small economies don't have a specific sector)
  ungroup() 

nrow(data_coverage_check[data_coverage_check$check_all == TRUE,])/nrow(data_coverage_check) * 100 #57.42% (compared to 26.4% without our assumptions)
nrow(data_coverage_check[data_coverage_check$check_all_but_sector_coverage == TRUE,])/nrow(data_coverage_check) * 100 #60.96% compared to 60.78% without our assumptions)

#Process data----
data_processed <- data_coverage_check %>%
  filter(check_all == TRUE & code != "_T") %>%
  group_by(iso3, series_type, subseries) %>%
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
                          code == "M" ~ "bus",
                          code == "N" ~ "bus",
                          code == "O" ~ "adm",
                          code == "P" ~ "adm",
                          code == "Q" ~ "adm",
                          code == "R" ~ "oth",
                          code == "S" ~ "oth",
                          code == "T" ~ "oth",
                          code == "U" ~ "oth")) %>%
  group_by(iso3, country, year, series_type, series_type_label, subseries, name, currency_label, unit, base, total_value) %>% 
  summarise(value = sum(value)) %>%
  ungroup()

#Compute growth rate for all but series type "Previous year prices" and "Deflator (rebased)" 
data_growth_excl <- data_processed %>%
  filter(!(series_type %in% c("Y", "DR")))  %>%
  group_by(iso3, name, series_type) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(value) - value)/value,
         growth_to_prev = (value - lag(value))/lag(value)) %>%
  ungroup()

data_growth_previous_year <- data_processed %>%
  filter(series_type == "Y") %>% 
  left_join(data_processed %>%
              filter(series_type == "V"),
            by = c("iso3", "year", "name"),
            keep = TRUE,
            suffix = c("", "_current")) %>%
  group_by(iso3, name) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(value) - value_current)/value_current,
         growth_to_prev = (value - lag(value_current))/lag(value_current),
         base = ifelse(!is.na(growth_to_next) | !is.na(growth_to_prev), -9999, -1)) %>%
  ungroup() %>%
  select(-ends_with("current"))

data_bind <- data_growth_excl %>%
  mutate(base = as.numeric(str_sub(base, 1,4))) %>% #Australia base is 2021-2022 => convert to 2021 for now
  bind_rows(data_growth_previous_year)

#Sanity checks----
check <- data_bind %>%
  mutate(mean_prev = mean(growth_to_prev, na.rm = TRUE),
         sd_prev = sd(growth_to_prev, na.rm = TRUE), 
         mean_next = mean(growth_to_next, na.rm = TRUE),
         sd_next = sd(growth_to_next, na.rm = TRUE), 
         check_outliers = !(abs(growth_to_prev - mean_prev) > sd_threshold*sd_prev | abs(growth_to_next - mean_next) > sd_threshold*sd_next)) %>% #Inversed, TRUE = no outliers, FALSE = outliers - makes it easier to filter in final step.
  group_by(iso3, series_type, subseries) %>%
  mutate(min_year = min(year),
         max_year = max(year)) %>%
  ungroup() %>%
  group_by(iso3, series_type, subseries, name) %>%
  arrange(year) %>%
  mutate(check_growth_next = (growth_to_next == lead(growth_to_prev)), #check that growth rate are consistent
         check_growth_prev = (growth_to_prev == lag(growth_to_next)),
         check_year_sector_min = min_year == min(year), #check that the sector year coverage is the same of the series year coverage
         check_year_sector_max = max_year == max(year)) %>%
  ungroup() %>%
  filter(if_any(all_of(starts_with("check")), ~ . == FALSE)) #10086 observations are picked up (all for outliers with threshold being 0.5 - 3391 if threshold == 1 - 864 if threshold == 2). Will be investigated at a later date

#Select series----

#current
data_current <- data_bind %>%
  filter(series_type == "V")

valid_current <- unique(data_current[, c("country", "year")])

#constant
priority <- c("LR" = 1, "L" = 2, "Y" = 3) #For each country-year, prioritize chained-volume (rebased) > chained-volume > previous year prices

data_constant <- data_bind %>%
  filter(series_type %in% c("LR", "L", "Y")) %>%
  mutate(priority = priority[series_type]) %>%
  arrange(country, year, priority) %>%
  group_by(country, year, name) %>%
  slice(1) %>%
  ungroup() %>%
  select(-priority)

nrow(data_constant %>%
       group_by(country, year) %>%
       mutate(n = length(unique(series_type))) %>%
       filter(n != 1)) #0. Making sure that only one series_type exists per country-year

nrow(unique(data_bind[data_bind$series_type %in% c("LR", "L", "Y"), c("country", "year")])) == nrow(unique(data_constant[, c("country", "year")])) #TRUE. Making sure that no country-year has been lost in the process

valid_constant <- unique(data_constant[, c("country", "year")])

#Save----
write_csv(data_current, "data/processed/oecd_current.csv")
write_csv(valid_current, "data/processed/oecd_current_list.csv")
write_csv(data_current, "data/processed/oecd_current.csv")
write_csv(valid_current, "data/processed/oecd_current_list.csv")
