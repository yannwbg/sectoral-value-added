library(tidyverse)

#Parameters
sd_threshold <- 0.5 #Threshold used to highlight outliers in terms of standard deviation

#Load data----
data <- read_csv("data/temp/un3_current.csv")
footnote <- read_csv("data/temp/un3_current_footnote.csv")
series_code <- read_csv("data/temp/code_current_3.csv")
sector <- read_csv("data/temp/un3_sector.csv")

#Helper functions----
#Function to check composite sectors
check_sectors <- function(sector_list, present_sectors) {
  # Split composite sectors into individual components
  split_sectors <- str_split(sector_list, "\\+")
  
  # Flatten the list and get unique sectors
  all_sectors <- unique(unlist(split_sectors))
  
  # Check which sectors are missing
  missing_sectors <- setdiff(all_sectors, present_sectors)
  
  # Check for composite sectors
  for (composite in sector_list) {
    components <- unlist(str_split(composite, "\\+"))
    if (all(components %in% present_sectors)) {
      missing_sectors <- setdiff(missing_sectors, components)
    } else if (composite %in% present_sectors) {
      missing_sectors <- setdiff(missing_sectors, components)
    }
  }
  
  return(missing_sectors)
}
#Function to filter composite sectors
filter_sectors_ISIC3 <- function(df) {
  # Get unique combinations of country, year, and series
  unique_combinations <- unique(df[, c("country", "year", "series_code")])
  
  # Initialize an empty data frame to store the results
  filtered_df <- data.frame()
  
  for (i in 1:nrow(unique_combinations)) {
    
    print(paste(round((i/nrow(unique_combinations))*100,2), "% completed."))
    
    # Extract the current combination
    combination <- unique_combinations[i, ]
    
    # Filter the data frame for the current combination
    combination_df <- subset(df, country == combination$country & year == combination$year & series_code == combination$series_code)
    
    #Select sectors that always need to be present
    filtered_df <- rbind(filtered_df, subset(combination_df, code %in% c("C", "D", "E", "F", "I", "L", "P", "B.1g")))
    
    # Check if both 'A' and 'B' are present
    if ('A' %in% combination_df$code & 'B' %in% combination_df$code) {
      # Keep both 'A' and 'B'
      filtered_df <- rbind(filtered_df, subset(combination_df, code %in% c('A', 'B')))
    } else {
      # Keep 'A+B'
      filtered_df <- rbind(filtered_df, subset(combination_df, code == 'A+B'))
    }
    
    # Check if both 'G' and 'H' are present
    if ('G' %in% combination_df$code & 'H' %in% combination_df$code) {
      # Keep both 'G' and 'H'
      filtered_df <- rbind(filtered_df, subset(combination_df, code %in% c('G', 'H')))
    } else {
      # Keep 'G+H'
      filtered_df <- rbind(filtered_df, subset(combination_df, code == 'G+H'))
    }
    
    # Check if both 'J' and 'K' are present
    if ('J' %in% combination_df$code & 'K' %in% combination_df$code) {
      # Keep both 'J' and 'K'
      filtered_df <- rbind(filtered_df, subset(combination_df, code %in% c('J', 'K')))
    } else {
      # Keep 'J+K'
      filtered_df <- rbind(filtered_df, subset(combination_df, code == 'J+K'))
    }
    
    # Check if both 'M', 'N' and 'O' are present
    if ('M' %in% combination_df$code & 'N' %in% combination_df$code & 'O' %in% combination_df$code) {
      # Keep both 'M', 'N' and 'O'
      filtered_df <- rbind(filtered_df, subset(combination_df, code %in% c('M', 'N', 'O')))
    } else {
      # Keep 'M+N+O'
      filtered_df <- rbind(filtered_df, subset(combination_df, code == 'M+N+O'))
    }
    
  }
  
  return(filtered_df)
}
#Function to compute composite sector growth
compositie_growth <- function(df) {
  
  df <- data_coverage_check %>%
    filter(check_all == TRUE & code != "B.1g")
  
  df_composite <- df %>%
    group_by(iso3, country, series_code, subseries) %>%
    mutate(composite_gh = all(c("G", "H", "G+H") %in% unique(code)),
           composite_jk = all(c("J", "K", "J+K") %in% unique(code)),
           composite_mno = all(c("M", "N", "O", "M+N+O") %in% unique(code))) %>%
    ungroup() %>%
    filter((composite_gh & code %in% c("G", "H", "G+H")) | (composite_jk & code %in% c("J", "K", "J+K")) | (composite_mno & code %in% c("M", "N", "O", "M+N+O"))) %>%
    mutate(name = case_when(code %in% c("G", "H", "G+H") ~ "G+H",
                            code %in% c("J", "K", "J+K") ~ "J+K",
                            code %in% c("M", "N", "O", "M+N+O") ~ "M+N+O"))
  
  df_composite_growth <- df_composite %>%
    group_by(iso3, country, year, series_code, subseries, name) %>%
    summarise(value = sum(value)) %>%
    ungroup() %>%
    group_by(iso3, country, name, series_code, subseries) %>%
    arrange(year) %>%
    mutate(growth_to_next = (lead(value) - value)/value,
           growth_to_prev = (value - lag(value))/lag(value)) %>%
    ungroup()
  
  df_composite_adjusted <- df_composite_growth %>%
    full_join(df_composite, by = c("iso3", "country", "year", "series_code", "subseries", "name")) %>%
    mutate(code_split = str_split(code, "\\+")) %>%
    unnest(code_split)
  
  
  
  
}
#Generate quality check variables----
sector_list <- c("A", "B", "A+B", "C", "D", "E", "F", "G", "H", "G+H", "I", "J", "K", "J+K", "L", "M", "N", "O", "M+N+O", "P")

data_filtered <- data %>% 
  filter_sectors_ISIC3() #Done separately as it takes several minutes
write_rds(data_filtered, "data/temp/data_filtered.rds")

data_coverage_check <- data_filtered %>%
  #Check sector coverage and validity for each country-year-series
  group_by(country, series_code, year) %>%
  mutate(sector_missing_list = list(check_sectors(sector_list, unique(code))),
         sector_missing_n = length(first(sector_missing_list)),
         sector_missing_label = paste(first(sector_missing_list), collapse = ", "),
         sector_full_coverage = ifelse(sector_missing_label %in% c("D", "E"), TRUE, (sector_missing_n == 0)), #We make the assumption that if only one of D or E is present, it includes the other
         sum_sector = sum(value[code != "B.1g"]),
         total_value = sum(value[code == "B.1g"]),
         sum_check = (abs((sum_sector-total_value)/ total_value) <= 0.005 | (total_value == 0 & sector_full_coverage))) %>% #We make the assumption (for now) that if B.1g is not present the sum of sectors is valid if sector_full_coverage is valid
  ungroup() %>%
  #Create sub-series based on unique coverage
  group_by(country, series_code) %>%
  mutate(subseries = dense_rank(sector_missing_label)) %>% #Create an index for unique sector coverage within a series
  ungroup() %>%
  group_by(country, series_code, subseries) %>%
  mutate(year_min = min(year),
         year_max = max(year),
         year_n = length(unique(year)),
         year_full_coverage = year_n == year_max - year_min + 1, #Check that the country-series-subseries does not have gap years
         check_all = (sector_full_coverage & sum_check & year_full_coverage), # = TRUE if all checks are valid
         check_all_but_sector_coverage = (sum_check & year_full_coverage)) %>% # = True if all checks but sector_full_coverage are valid (this allows us to keep corner cases where small economies don't have a specific sector)
  ungroup() 

nrow(data_coverage_check[data_coverage_check$check_all == TRUE,])/nrow(data_coverage_check) * 100 #39.04%
nrow(data_coverage_check[data_coverage_check$check_all_but_sector_coverage == TRUE,])/nrow(data_coverage_check) * 100 #82.17% 

#Process data----
data_composite <- data_coverage_check %>%
  filter(check_all == TRUE & code != "B.1g") %>%
  group_by(iso3, country, series_code, subseries) %>%
  mutate(composite_gh = all(c("G", "H", "G+H") %in% unique(code)),
         composite_jk = all(c("J", "K", "J+K") %in% unique(code)),
         composite_mno = all(c("M", "N", "O", "M+N+O") %in% unique(code))) %>%
  ungroup() %>%
  filter((composite_gh & code %in% c("G", "H", "G+H")) | (composite_jk & code %in% c("J", "K", "J+K")) | (composite_mno & code %in% c("M", "N", "O", "M+N+O"))) %>%
  select(iso3, year, series_code, subseries, code, value) %>%
  group_by(iso3, series_code, subseries) %>%
  group_modify(~ split_composite_series(.x)) %>%
  ungroup()

data_processed <- data_coverage_check %>%
  filter(check_all == TRUE & code != "B.1g") %>%
  mutate(name = case_when(code == "A+B" | code == "A" | code == "B" ~ "agr",
                          code == "C" ~ "min",
                          code == "D" ~ "man",
                          code == "E" ~ "utl",
                          code == "F" ~ "ctr",
                          code == "G" ~ "trd",
                          code == "H" ~ "hos",
                          code == "I" ~ "con",
                          code == "J" ~ "fin",
                          code == "K" ~ "bus",
                          code == "L" ~ "adm",
                          code == "M" ~ "adm",
                          code == "N" ~ "adm",
                          code == "O" ~ "oth",
                          code == "P" ~ "oth",
                          code == "M+N+O" ~ "oth",
                          code == "G+H" ~ "trd",
                          code == "J+K" ~ "bus")) %>%
  group_by(iso3, country, year, series_code, subseries, name, currency, SNA, year_type, total_value) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  group_by(iso3, country, name, series_code, subseries) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(value) - value)/value,
         growth_to_prev = (value - lag(value))/lag(value)) %>%
  ungroup()

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
         check_year_sector_min = min_year == min(year), #check that the sector year coverage is the same of the series year coverage
         check_year_sector_max = max_year == max(year)) %>%
  ungroup() %>%
  filter(if_any(all_of(starts_with("check")), ~ . == FALSE)) #333 observations are picked up. A large portion (303) due to sector coverage changing but across composite (eg. G+H for some year but G and H for other years)

#Select series----
data_selected <- data_processed %>%
  right_join(series_code, by = c("country", "year", "series_code" = "final_series")) %>%
  filter(!(overlap == TRUE & is.na(growth_to_next))) %>%
  select(-c(overlap, "...1"))

#Save----
write_csv(data_selected, "data/processed/un4_current.csv")
