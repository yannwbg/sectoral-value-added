library(tidyverse)

#parameters
sd_threshold <- 2 #Threshold used to highlight outliers in terms of standard deviation
always_keep <- c("C", "D", "E", "F", "I", "L", "P", "B.1g", "B.1*g") #Sectors that are never part of original composites
possible_combinations <- list(c("A", "B"), #Original composites
                              c("G", "H"),
                              c("J", "K"),
                              c("M", "N", "O"))

#Load data----
data <- read_csv("data/temp/un3_constant.csv")
footnote <- read_csv("data/temp/un3_constant_footnote.csv")
series_code <- read_csv("data/temp/code_constant_3.csv") %>%
  mutate(base = ifelse(base == -1, -9999, base))
current <- read_csv("data/temp/un3_current_gap_filler_data.csv") #Will be used to compute base -1 constant growth => does not need to be processed as we want to compare same series_code
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
filter_sectors <- function(df, always_keep, combinations) {
  # Function to check and filter combinations
  check_combinations <- function(data, comb) {
    if (all(comb %in% data$code)) {
      return(data %>% filter(code %in% comb))
    } else {
      combined_code <- paste(comb, collapse = "+")
      return(data %>% filter(code == combined_code))
    }
  }
  
  # Group by country, year, and series_code
  filtered_df <- df %>%
    group_by(country, year, series_code) %>%
    do({
      data <- .
      result <- data %>% filter(code %in% always_keep)
      for (comb in combinations) {
        result <- bind_rows(result, check_combinations(data, comb))
      }
      result
    }) %>%
    ungroup()
  
  return(filtered_df)
}

#Function to distribute composite sector value into its components based on components' proportion in closest available year
split_composite_series <- function(df) {
  
  df <- df %>%
    mutate(adjusted_composite = FALSE,
           old_code = code)
  
  # Identify composite series and their components
  composite_series <- unique(df$code[grepl("\\+", df$code)])
  
  # Function to find the closest reference year
  find_reference_year <- function(df, components) {
    years_with_components <- df %>%
      filter(code %in% components) %>%
      group_by(year) %>%
      summarise(count = n()) %>%
      filter(count == length(components)) %>%
      pull(year)
    
    if (length(years_with_components) == 0) {
      return(NA)
    }
    
    return(min(years_with_components))
  }
  
  # Split composite series into components for each composite
  for (comp in composite_series) {
    components <- unlist(strsplit(comp, "\\+"))
    
    # Find the closest reference year
    reference_year <- find_reference_year(df, components)
    
    if (!is.na(reference_year)) {# Calculate proportions for each component series
      reference_df <- subset(df, year == reference_year & code %in% components)
      total_value <- sum(reference_df$value)
      proportions <- sapply(components, function(comp) {
        sum(reference_df$value[reference_df$code == comp]) / total_value
      })
      
      # Apply proportions to split composite series in other years
      for (year in unique(df$year)) {
        if (year != reference_year) {
          composite_value <- df$value[df$year == year & df$code == comp]
          if (length(composite_value) > 0) {
            for (i in seq_along(components)) {
              new_row <- data.frame(
                iso3 = ifelse(length(unique(df$iso3[df$year == year & df$code == comp])) > 0, unique(df$iso3[df$year == year & df$code == comp]), NA),
                year = year,
                series_code = ifelse(length(unique(df$series_code[df$year == year & df$code == comp])) > 0, unique(df$series_code[df$year == year & df$code == comp]), NA),
                subseries = ifelse(length(unique(df$subseries[df$year == year & df$code == comp])) > 0, unique(df$subseries[df$year == year & df$code == comp]), NA),
                base = ifelse(length(unique(df$iso3[df$year == year & df$code == comp])) > 0, unique(df$base[df$year == year & df$code == comp]), NA),
                code = components[i],
                value = composite_value * proportions[i],
                adjusted_composite = TRUE, 
                old_code = comp
              )
              df <- rbind(df, new_row)
            }
            # Remove the original composite series row
            df <- df[!(df$year == year & df$code == comp), ]
          }
        }
      }} else {
        next
      }
  }
  
  return(df)
}

#Filter sectors to only keep the ones we need----
data_filtered <- data %>% 
  filter_sectors(always_keep = always_keep, combinations = possible_combinations)


#Generate quality check variables----
sector_list <- c("A", "B", "A+B", "C", "D", "E", "F", "G", "H", "G+H", "I", "J", "K", "J+K", "L", "M", "N", "O", "M+N+O", "P")

data_coverage_check <- data_filtered %>% 
  #Check sector coverage and validity for each country-year-series
  group_by(country, series_code, year, base) %>%
  mutate(sector_missing_list = list(check_sectors(sector_list, unique(code))),
         sector_missing_n = length(first(sector_missing_list)),
         sector_missing_label = paste(first(sector_missing_list), collapse = ", "),
         sector_full_coverage = ifelse(sector_missing_label == "P", TRUE, (sector_missing_n == 0)), #We make the assumption that if P is missing, it is included in O or M+N+O
         sum_sector = sum(value[code != "B.1g" & code != "B.1*g"]),
         total_value = sum(value[code == "B.1g"]),
         total_gdp = sum(value[code == "B.1*g"]),
         sum_check = (abs((sum_sector-total_value)/ total_value) <= 0.005 | abs((sum_sector-total_gdp)/ total_gdp) <= 0.005 | (total_value == 0 & total_gdp == 0 & sector_full_coverage))) %>% #We make the assumption (for now) that if B.1g is not present the sum of sectors is valid if sector_full_coverage is valid
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

nrow(data_coverage_check[data_coverage_check$check_all == TRUE,])/nrow(data_coverage_check) * 100 #63.58%
nrow(data_coverage_check[data_coverage_check$check_all_but_sector_coverage == TRUE,])/nrow(data_coverage_check) * 100 #74.29%

#Process data----

#Split composite sectors into their components when possible
data_composite_adjustement_prep <- data_coverage_check %>%
  filter(check_all == TRUE & code != "B.1g" & code != "B.1*g") %>%
  select(iso3, year, series_code, subseries, base, code, value) %>%
  mutate(combination_id = interaction(iso3, series_code, subseries, base, drop = TRUE))

data_composite_adjustement_prep_split <- lapply(split(data_composite_adjustement_prep, data_composite_adjustement_prep$combination_id), function(df) df %>% select(-combination_id))

data_composite_adjusted <- do.call(rbind, lapply(data_composite_adjustement_prep_split, split_composite_series))

nrow(unique(data_composite_adjusted[data_composite_adjusted$adjusted_composite == TRUE & data_composite_adjusted$old_code %in% c("J+K", "M+N+O"),
                                    c("iso3", "year", "series_code", "subseries", "base", "old_code")]))/nrow(unique(data_composite_adjusted[data_composite_adjusted$old_code %in% c("J+K", "M+N+O"), c("iso3", "year", "series_code", "subseries", "base", "old_code")])) * 100 #Only 3.94% of the relevant ("J+K", "M+N+O") composites could be distributed into their component


#Aggregate relevant sectors
data_processed <-  data_composite_adjusted %>%
  left_join(data_coverage_check %>%
              select(-value), by = c("iso3", "year", "series_code", "subseries", "base", "old_code" = "code")) %>%
  group_by(iso3, series_code, subseries, base) %>%
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
  group_by(iso3, country, year, series_code, subseries, base, name, currency, SNA, year_type, total_value) %>%
  summarise(value = sum(value),
            adjusted = (any(unique(adjusted_composite) == TRUE & unique(name) %in% c("trd", "hos", "fin", "bus", "adm", "oth")))) %>%
  ungroup()

#When base is not -1
data_growth_excl_1 <- data_processed %>%
  filter(base != -1) %>%
  group_by(iso3, country, name, series_code, subseries, base) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(value) - value)/value,
         growth_to_prev = (value - lag(value))/lag(value)) %>%
  ungroup()

#When base is -1
data_growth_1 <- data_processed %>% 
  filter(base == -1) %>%
  left_join(current, by = c("country", "year", "name", "series_code", "year_type", "adjusted"), keep = TRUE, suffix = c("", "_current")) %>%
  group_by(country, name, series_code, subseries, base) %>%
  arrange(year) %>%
  mutate(growth_to_next = ifelse(name == name_current, (lead(value) - value_current)/value_current, NA), #making the assumption that series_codes match between current and constant and using sector disaggregation as a check.
         growth_to_prev = ifelse(name == name_current, (value - lag(value_current))/lag(value_current), NA),
         base = ifelse((!is.na(growth_to_next) | !is.na(growth_to_prev)), -9999, base)) %>%
  ungroup() %>%
  select(-ends_with("current"))

data_bind <- data_growth_1 %>%
  bind_rows(data_growth_excl_1)

#Sanity checks----
check <- data_bind %>%
  mutate(mean_prev = mean(growth_to_prev, na.rm = TRUE),
         sd_prev = sd(growth_to_prev, na.rm = TRUE), 
         mean_next = mean(growth_to_next, na.rm = TRUE),
         sd_next = sd(growth_to_next, na.rm = TRUE), 
         check_outliers = !(abs(growth_to_prev - mean_prev) > sd_threshold*sd_prev | abs(growth_to_next - mean_next) > sd_threshold*sd_next)) %>% #Inversed, TRUE = no outliers, FALSE = outliers - makes it easier to filter in final step.
  group_by(iso3, series_code, subseries, base) %>%
  mutate(min_year = min(year),
         max_year = max(year)) %>%
  ungroup() %>%
  group_by(iso3, series_code, subseries, base, name) %>%
  arrange(year) %>%
  mutate(check_growth_next = (growth_to_next == lead(growth_to_prev)), #check that growth rate are consistent
         check_growth_prev = (growth_to_prev == lag(growth_to_next)),
         check_year_sector_min = min_year == min(year), #check that the sector coverage is the same of the series coverage
         check_year_sector_max = max_year == max(year)) %>%
  ungroup() %>%
  filter(if_any(all_of(starts_with("check")), ~ . == FALSE)) #3710 observations are picked up (all for outliers with threshold being 0.5 - 1104 if threshold == 1 - 328 if threshold == 2). Will be investigated at a later date

#Select series---- Would need to happen at the beginning once the series-select is finalized
data_selected <- data_bind %>%
  right_join(series_code, by = c("country", "year", "base", "series_code" = "final_series")) %>%
  filter(!(overlap == TRUE & is.na(growth_to_next))) %>%
  select(-c(overlap, "...1"))

valid_data_list <- unique(data_selected[, c("country", "year")])

#Highlight potential country-year where data_processed could fill gap if we don't find other sources
gap_filler <- setdiff(unique(data_bind[, c("country", "year")]), valid_data_list)

#Save----
write_csv(data_selected, "data/processed/un3_constant.csv")
write_csv(valid_data_list, "data/processed/un3_constant_list.csv")
write_csv(gap_filler, "data/temp/un3_constant_gap_filler_list.csv")
write_csv(data_bind, "data/temp/un3_constant_gap_filler_data.csv")
