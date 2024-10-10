# Function to identify expected NAs
identify_expected_nas <- function(df) {
  df <- df %>%
    arrange(year) %>%
    mutate(is_na = is.na(dataset))
  
  # Identify gaps
  df <- df %>%
    group_by(country, series) %>%
    mutate(
      non_na_lead = lead(!is_na),
      non_na_lag = lag(!is_na),
      expected_na = is_na | (is_na & (non_na_lead | non_na_lag))
    ) %>%
    ungroup()
  
  return(df)
}

# Apply the function to the data
result <- identify_expected_nas(coverage_long)

# Filter to show only expected NAs
expected_nas <- result %>%
  filter(expected_na) %>%
  select(country, series, year)

current_expected_nas <- expected_nas %>% 
  filter(series == "current")

current_unexpected_nas <- final_current %>%
  group_by(country, year) %>%
  mutate(na_n = sum(is.na(value_adjusted))) %>%
  ungroup() %>%
  filter(na_n > 5) %>%
  distinct(country, year) %>%
  anti_join(current_expected_nas, by = c("country", "year"))

constant_expected_nas <- expected_nas %>% 
  filter(series == "constant")

constant_unexpected_nas <- final_current %>%
  group_by(country, year) %>%
  mutate(na_n = sum(is.na(value_adjusted))) %>%
  ungroup() %>%
  filter(na_n > 5) %>%
  distinct(country, year) %>%
  anti_join(constant_expected_nas, by = c("country", "year"))

current_coverage_issues <- coverage_wide %>%
  filter(series == "current" & country %in% current_unexpected_nas$country)

#Need to change the way expected_na is created as:
#1) it doesn't seem to pick up when the gap in coverage is larger than 1 year (eg Angola, Cameroon, Gambia, Indonesia, Myanmar, Timor-Leste)
#2) issues when the latest year available is not part of un data (Benin, Canada, Chile, Cote d'Ivoire, Hungary, Malawi)
#Egypt, Pakistan, Slovak Republic and Lesotho can't be explained that way, need to dive deeper as they should not be missing
#Bahrain and Quatar can't be explained that way but only happens for a couple years

#check

check_value_diff <- final_current %>%
  ungroup() %>%
  filter(!is.na(value) & !is.na(value_adjusted) & value_adjusted != 0) %>%
  mutate(diff = (value_adjusted - value) / value_adjusted) 

nrow(check_value_diff %>%
       filter(abs(diff) < 1))/nrow(check_value_diff)

check_value_diff %>%
  filter(abs(diff) < 1) %>%
  summarise(mean = mean(diff))