library(tidyverse)

#Load and merge the data----
country_list <- read_csv("data/processed/country_list.csv") %>%
  select(iso3, country = wb_name)

files_list <- list.files(path = "data/processed", pattern = "current_list", full.names = TRUE)

# Function to read a file and add a new column
read_and_label <- function(file_path) {
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Extract the label from the file path
  label <- sub("data/processed/(.*)_current_list.csv", "\\1", file_path)
  
  # Add the new column
  df$source <- label
  
  return(df)
}

data_bind <- bind_rows(lapply(files_list, read_and_label)) %>%
  left_join(country_list, by = "iso3")

#Select best source per year----
priority <- c("un4" = 1, "oecd" = 2, "un3" = 3, "national" = 4, "un4_gap_filler" = 5, "un3_gap_filler" = 6)

coverage_long <- data_bind %>%
  mutate(priority = priority[source]) %>%
  arrange(iso3, year) %>%
  group_by(iso3, year) %>%
  slice(1) %>%
  ungroup() %>%
  select(iso3, country, year, source)

complete_grid <- expand.grid(iso3 = unique(country_list$iso3), year = 1990:2023) %>%
  left_join(country_list, by = "iso3")
  
coverage_wide <- complete_grid %>%
  left_join(coverage_long, by = c("iso3", "country", "year")) %>%
  pivot_wider(id_cols = c(iso3, country), names_from = "year", values_from = "source")

#Save----
write_csv(coverage_long, "data/processed/coverage_long_current.csv")
write_csv(coverage_wide, "data/processed/coverage_wide_current.csv")

