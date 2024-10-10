library(tidyverse)
library(haven)
library(readxl)
library(writexl)

#Anxilliary#####################################################################

country_list <- read_xlsx("Data summary.xlsx", skip = 4, col_names = TRUE) %>%
  select(iso3, country) %>%
  add_row(iso3 = c("LUX", "ISL", "MLT", "CPV"),
          country = c("Luxembourg", "Iceland", "Malta", "Cabo Verde"))

coverage_wide <- read_xlsx("1 - Data/2 - Processed/coverage.xlsx") 

coverage_long <- coverage_wide %>%
  pivot_longer(cols = -c(country, series), names_to = "year", values_to = "dataset") %>%
  mutate(year = as.numeric(year)) %>%
  left_join(country_list, by = "country")

#National#######################################################################

national <- read_xlsx("1 - Data/2 - Processed/2 - National//national_all.xlsx") %>%
  filter(sector != "tot") %>%
  left_join(country_list, by = "iso3") %>%
  select(iso3, country, year, series, base, currency, sector, value, growth_to_prev, growth_to_next) %>%
  mutate(dataset = "national",
         base = as.numeric(base),
         main = TRUE)

#OECD###########################################################################

oecd_current <- read_xlsx("1 - Data/2 - Processed/1 - Global/OECD_current.xlsx") %>%
  mutate(series = "current",
         base = as.numeric(NA)) %>%
  filter(quality_cat != 4) %>%
  select(iso3, country, year, series, base, currency, sector = name, value, growth_to_prev, growth_to_next) %>%
  mutate(dataset = "oecd",
         main = TRUE)

oecd_constant <- read_xlsx("1 - Data/2 - Processed/1 - Global/OECD_constant.xlsx") %>%
  mutate(series = "constant") %>%
  filter(quality_cat != 4) %>%
  select(iso3, country, year, series, base, currency, sector = name, value, growth_to_prev, growth_to_next) %>%
  mutate(dataset = "oecd",
         main = TRUE)

#UN3############################################################################

un3_current <- read_xlsx("1 - Data/2 - Processed/1 - Global/un3_current_growth.xlsx") %>%
  filter(str_detect(quality, "1") & keep == 1 & !is.na(name)) %>%
  mutate(series = "current",
         base = as.numeric(NA)) %>%
  right_join(country_list, by = "country") %>%
  select(iso3, country, year, series, base, currency, sector = name, value, growth_to_prev, growth_to_next, main) %>%
  mutate(dataset = "un3")

un3_constant <- read_xlsx("1 - Data/2 - Processed/1 - Global/un3_constant_growth.xlsx") %>%
  filter(str_detect(quality, "1") & keep == 1 & !is.na(name)) %>%
  mutate(series = "constant") %>%
  right_join(country_list, by = "country") %>%
  select(iso3, country, year, series, base, currency, sector = name, value, growth_to_prev, growth_to_next, main) %>%
  mutate(dataset = "un3")

#UN4############################################################################

un4_current <- read_xlsx("1 - Data/2 - Processed/1 - Global/un4_current_growth.xlsx") %>%
  filter(str_detect(quality, "1") & keep == 1 & !is.na(name)) %>%
  mutate(series = "current",
         base = as.numeric(NA)) %>%
  right_join(country_list, by = "country") %>%
  select(iso3, country, year, series, base, currency, sector = name, value, growth_to_prev, growth_to_next, main) %>%
  mutate(dataset = "un4")

un4_constant <- read_xlsx("1 - Data/2 - Processed/1 - Global/un4_constant_growth.xlsx") %>%
  filter(str_detect(quality, "1") & keep == 1 & !is.na(name)) %>%
  mutate(series = "constant") %>%
  right_join(country_list, by = "country") %>%
  select(iso3, country, year, series, base, currency, sector = name, value, growth_to_prev, growth_to_next, main) %>%
  mutate(dataset = "un4")

#Compile########################################################################

data <- bind_rows(national, oecd_current, oecd_constant, un3_current, un3_constant, un4_current, un4_constant) %>%
  right_join(coverage_long, by = c("country", "iso3", "series", "year", "dataset"))

main_coverage <- coverage_long %>%
  mutate(priority = case_when(dataset == "un4" ~ 1,
                              dataset == "un3" ~ 2,
                              dataset == "oecd" ~ 3,
                              dataset == "national" ~ 4)) %>%
  group_by(country, series, iso3) %>%
  summarise(main_dataset = case_when(min(priority, na.rm = TRUE) == 1 ~ "un4",
                                  min(priority, na.rm = TRUE) == 2 ~ "un3",
                                  min(priority, na.rm = TRUE) == 3 ~ "oecd",
                                  min(priority, na.rm = TRUE) == 4 ~ "national",
                                  TRUE ~ NA))

#Current########################################################################

current <- data[0,]
current$main_dataset <- logical(0)
current$base_value <- logical(0)
current$min <- numeric(0)
current$max <- numeric(0)
current$value_adjusted <- numeric(0)
  

for (i in 1:length(sort(unique(data$country)))) {
  
  country_i <- sort(unique(data$country))[i]
  
  print(paste(country_i," : ", round(i/length(sort(unique(data$country))) * 100, 2), "% completed."))
  
  main_dataset_i <- main_coverage[main_coverage$country == country_i & main_coverage$series == "current",][["main_dataset"]]
  
  data_i <- data %>%
    filter(country == country_i & series == "current") %>%
    mutate(main_dataset = dataset == main_dataset_i,
           base_value = (main_dataset == TRUE & main == TRUE),
           min = min(year[base_value == TRUE], na.rm = TRUE),
           max = max(year[base_value == TRUE], na.rm = TRUE),
           value_adjusted = ifelse(base_value, value, NA))
  
  previous_y <- data_i %>%
    filter(year < min)
  
  next_y <- data_i %>%
    filter(year > max)
  
  j_i <- 1
  
  for (j in sort(unique(previous_y$year), decreasing = TRUE)) {
    
    data_i <- data_i %>%
      group_by(sector) %>%
      arrange(year) %>%
      mutate(value_adjusted = ifelse(year == j & j_i == 1, lead(value)/(1+growth_to_next), ifelse(year == j, lead(value_adjusted)/(1+growth_to_next),value_adjusted)))
    
    j_i <- j_i + 1
    
  }
  
  k_i <- 1
  
  for (k in sort(unique(next_y$year), decreasing = FALSE)) {
    data_i <- data_i %>%
      group_by(sector) %>%
      arrange(year) %>%
      mutate(value_adjusted = ifelse(year == k & k_i == 1, lag(value)*(1+growth_to_prev), ifelse(year == k, lag(value_adjusted)*(1+growth_to_prev), value_adjusted)))
    
    k_i <- k_i + 1
   
  }
  
  #data_i <- data_i keep for now to add check
  
  current <- current %>% 
    bind_rows(data_i)
  
}

final_current <- current %>% 
  group_by(country) %>%
  mutate(currency = first(currency[base_value == TRUE]))


#constant########################################################################

#First deal with base = -1 
base_1 <- data %>% 
  filter(base == -1) %>%
  mutate(previous_year = year - 1) %>%
  left_join(final_current %>%
              rename_with(~ paste0(., "_current"), -c(country, iso3, year, sector)), by = c("country", "iso3", "previous_year" = "year", "sector")) %>%
  mutate(growth_to_prev_cor = (value - value_current)/value_current) %>%
  group_by(iso3, country, sector) %>%
  arrange(year) %>%
  mutate(growth_to_next_cor = lead(growth_to_prev_cor)) %>%
  ungroup() %>%
  mutate(base = ifelse((!is.na(growth_to_next_cor) | !is.na(growth_to_prev_cor)), -9999, base),
         growth_to_prev = growth_to_prev_cor,
         growth_to_next = growth_to_next_cor) %>%
  select(-c(ends_with("current"), ends_with("cor"), previous_year))

data <- data %>%
  filter(base != -1) %>%
  bind_rows(base_1)

constant <- data[0,]
constant$main_dataset <- logical(0)
constant$base_value <- logical(0)
constant$min <- numeric(0)
constant$max <- numeric(0)
constant$value_adjusted <- numeric(0)


for (i in 1:length(sort(unique(data$country)))) {
  
  country_i <- sort(unique(data$country))[i]
  
  print(paste(country_i," : ", round(i/length(sort(unique(data$country))) * 100, 2), "% completed."))
  
  main_dataset_i <- main_coverage[main_coverage$country == country_i & main_coverage$series == "constant",][["main_dataset"]]
  
  data_i <- data %>%
    filter(country == country_i & series == "constant") %>%
    mutate(main_dataset = dataset == main_dataset_i,
           base_value = (main_dataset == TRUE & main == TRUE),
           min = min(year[base_value == TRUE], na.rm = TRUE),
           max = max(year[base_value == TRUE], na.rm = TRUE),
           value_adjusted = ifelse(base_value, value, NA))
  
  previous_y <- data_i %>%
    filter(year < min)
  
  next_y <- data_i %>%
    filter(year > max)
  
  j_i <- 1
  
  for (j in sort(unique(previous_y$year), decreasing = TRUE)) {
    
    data_i <- data_i %>%
      group_by(sector) %>%
      arrange(year) %>%
      mutate(value_adjusted = ifelse(year == j & j_i == 1, lead(value)/(1+growth_to_next), ifelse(year == j, lead(value_adjusted)/(1+growth_to_next),value_adjusted)))
    
    j_i <- j_i + 1
    
  }
  
  k_i <- 1
  
  for (k in sort(unique(next_y$year), decreasing = FALSE)) {
    data_i <- data_i %>%
      group_by(sector) %>%
      arrange(year) %>%
      mutate(value_adjusted = ifelse(year == k & k_i == 1, lag(value)*(1+growth_to_prev), ifelse(year == k, lag(value_adjusted)*(1+growth_to_prev), value_adjusted)))
    
    k_i <- k_i + 1
    
  }
  
  #data_i <- data_i keep for now to add check
  
  constant <- constant %>% 
    bind_rows(data_i)
  
}

final_constant <- constant %>% 
  group_by(country) %>%
  mutate(currency = first(currency[base_value == TRUE]))

write_dta(final_current, "1 - Data/0 - Temp/final_current.dta")
write_dta(final_constant, "1 - Data/0 - Temp/final_constant.dta")
