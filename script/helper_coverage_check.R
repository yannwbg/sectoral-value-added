library(tidyverse)
library(haven)
library(readxl)
library(writexl)

################################################################################
country_list <- read_xlsx("Data summary.xlsx", skip = 4, col_names = TRUE) %>%
  select(iso3, country)

################################################################################
national <- read_xlsx("1 - Data/2 - Processed/2 - National//national_all.xlsx") %>%
  left_join(country_list, by = "iso3") %>%
  select(country, year, series) %>%
  distinct() %>%
  mutate(dataset = "national")
################################################################################
oecd <- read_xlsx("1 - Data/2 - Processed/1 - Global/OECD_current.xlsx") %>%
  filter(quality_cat != 4) %>%
  mutate(series = "current") %>%
  select(country, year, series) %>%
  bind_rows(read_xlsx("1 - Data/2 - Processed/1 - Global/OECD_constant.xlsx") %>%
              filter(quality_cat != 4) %>%
              mutate(series = "constant") %>%
              select(country, year, series)) %>%
  distinct() %>%
  mutate(dataset = "oecd")
################################################################################
un3 <- read_xlsx("1 - Data/2 - Processed/1 - Global/un3_current_growth.xlsx") %>%
  filter(str_detect(quality, "1")) %>%
  mutate(series = "current") %>%
  right_join(country_list, by = "country") %>%
  select(country, year, series) %>%
  bind_rows(read_xlsx("1 - Data/2 - Processed/1 - Global/un3_constant_growth.xlsx") %>%
              filter(str_detect(quality, "1")) %>%
              mutate(series = "constant") %>%
              right_join(country_list, by = "country") %>%
              select(country, year, series)) %>%
  distinct() %>%
  mutate(dataset = "un3")
################################################################################
un4 <- read_xlsx("1 - Data/2 - Processed/1 - Global/un4_current_growth.xlsx") %>%
  filter(str_detect(quality, "1")) %>%
  mutate(series = "current") %>%
  right_join(country_list, by = "country") %>%
  select(country, year, series) %>%
  bind_rows(read_xlsx("1 - Data/2 - Processed/1 - Global/un4_constant_growth.xlsx") %>%
              filter(str_detect(quality, "1")) %>%
              mutate(series = "constant") %>%
              right_join(country_list, by = "country") %>%
              select(country, year, series)) %>%
  distinct() %>%
  mutate(dataset = "un4")
################################################################################
coverage <- bind_rows(national, oecd, un3, un4) %>%
  filter(!is.na(year)) %>%
  mutate(priority = case_when(dataset == "un4" ~ 1,
                              dataset == "un3" ~ 2,
                              dataset == "oecd" ~ 3,
                              dataset == "national" ~ 4)) %>%
  group_by(country, year, series) %>%
  summarise(dataset = case_when(min(priority) == 1 ~ "un4",
                                min(priority) == 2 ~ "un3",
                                min(priority) == 3 ~ "oecd",
                                min(priority) == 4 ~ "national")) %>%
  ungroup() %>%
  arrange(year) %>%
  pivot_wider(id_cols = c(country, series), names_from = "year", values_from = "dataset")

write_xlsx(coverage, "1 - Data/2 - Processed/coverage.xlsx")

