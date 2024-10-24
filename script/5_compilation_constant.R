library(tidyverse)

#Parameters----
year_range <- 2017:2019
priority <- c("un4" = 1, "oecd" = 2, "un3" = 3, "national" = 4, "un4_gap_filler" = 5, "un3_gap_filler" = 6)

#Load data----
coverage_long <- read_csv("data/processed/coverage_long_constant.csv") %>%
  select(-country)

coverage_wide <- read_csv("data/processed/coverage_wide_constant.csv")

un4 <- read_csv("data/processed/un4_constant.csv") %>%
  select(iso3, year, sector = name, currency, base, value, starts_with("growth"))

un3 <- read_csv("data/processed/un3_constant.csv") %>%
  select(iso3, year, sector = name, currency, base, value, starts_with("growth"))

oecd <- read_csv("data/processed/oecd_constant.csv") %>%
  mutate(value = value*1000000) %>% #Unit is always 6
  select(iso3, year, sector = name, currency = currency_label, base, value, starts_with("growth"))

national <- read_csv("data/processed/national_constant.csv") %>%
  select(iso3, year, sector, currency, base, value, starts_with("growth"))

un4_gap_filler <- read_csv("data/temp/un4_constant_gap_filler_data.csv") %>% 
  filter(!(is.na(growth_to_next) & is.na(growth_to_prev))) %>%
  group_by(iso3, year) %>%
  mutate(keep = (series_code == max(series_code))) %>% 
  filter(keep == TRUE) %>%
  ungroup() %>%
  group_by(iso3, year) %>%
  mutate(keep = (base == max(base))) %>%
  filter(keep == TRUE) %>%
  select(iso3, year, sector = name, currency, base, value, starts_with("growth"))

un3_gap_filler <- read_csv("data/temp/un3_constant_gap_filler_data.csv") %>% 
  filter(!(is.na(growth_to_next) & is.na(growth_to_prev))) %>%
  group_by(iso3, year) %>%
  mutate(keep = (series_code == max(series_code))) %>% 
  filter(keep == TRUE) %>%
  ungroup() %>%
  group_by(iso3, year) %>%
  mutate(keep = (base == max(base))) %>%
  filter(keep == TRUE) %>%
  select(iso3, year, sector = name, currency, base, value, starts_with("growth"))

#Compilation----
data_combined <- bind_rows(
  un4 %>% mutate(source = "un4"),
  un3 %>% mutate(source = "un3"),
  oecd %>% mutate(source = "oecd"),
  national %>% mutate(source = "national"),
  un4_gap_filler %>% mutate(source = "un4_gap_filler"),
  un3_gap_filler %>% mutate(source = "un3_gap_filler")
)

data_compiled <- coverage_long %>%
  left_join(data_combined, by = c("iso3", "year", "source"))

#Select best source per country----
coverge_ranked <- coverage_long %>%
  mutate(rank = priority[source])

coverage_inrange <-  coverge_ranked  %>%
  filter(year %in% year_range)

# Determine the best source for each iso3 within year_range
best_source_inrange <- coverage_inrange %>%
  group_by(iso3) %>%
  arrange(rank) %>%
  slice(1) %>%
  ungroup()

#For missing iso3s, find the closest available year and select the best source
missing_iso3s <- setdiff(coverge_ranked$iso3, best_source_inrange$iso3)

closest_year_best_source <- coverge_ranked %>%
  filter(iso3 %in% missing_iso3s) %>%
  group_by(iso3) %>%
  arrange(abs(year - ((min(year_range) + max(year_range))/2)), rank) %>%
  slice(1) %>%
  ungroup()

best_source <- bind_rows(best_source_inrange, closest_year_best_source)

data_sourced <- data_compiled %>%
  left_join(best_source %>%
              select(iso3, best_source = source),
            by = "iso3")
#Save----
write_csv(data_sourced, "data/temp/constant_compiled.csv")


