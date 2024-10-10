library(tidyverse)
library(haven)
library(readxl)
library(writexl)

print_unique_and_count <- function(x) {
  unique_values <- unique(x)
  cat("Unique values:\n")
  print(sort(unique_values))
  cat("Number of unique values:", length(unique_values), "\n")
}

collapse_strings <- function(strings, sep = ", ") {
  elements <- unlist(strsplit(strings, sep))
  elements <- trimws(elements)
  unique_elements <- unique(elements)
  sorted_elements <- sort(unique_elements)
  result <- paste(sorted_elements, collapse = sep)
  return(result)
}

#Program########################################################################

#OECD data current################################################################

#original data (just changed column names for ease)
oecd_orig <- read_csv("1 - Data/1 - Raw/Global/OECD.csv") 

#Explore data 
str(oecd_orig)

single_unique_cols <- sapply(oecd_orig, function(col) length(unique(col)) == 1)
names(oecd_orig)[single_unique_cols]

#Rename and select relevant columns
oecd <- oecd_orig %>%
  select(iso3 = "REF_AREA",
         country = "Reference area",
         code = "ACTIVITY",
         label = "Economic activity",
         series_currency = "Unit of measure",
         series_type = "Price base",
         year = "TIME_PERIOD",
         value = "OBS_VALUE",
         base = "REF_YEAR_PRICE",
         unit = "Unit multiplier",
         currency = "Currency",
         note = "Observation status")

#Current
oecd_current <- oecd %>%
  filter(series_type == "Current prices")

nrow(unique(oecd_current[, c("country", "code", "year")])) #no duplicata
sort(unique(oecd_current$code))

#Check all sectors are available and explore where it is not
sector <- oecd_current %>%
  filter(code != "_T") %>%
  group_by(country, year) %>%
  mutate(sectors = paste(sort(unique(code)), collapse = "_"))

sector_tot <- oecd_current %>%
  filter(code == "_T")

check_total <- sector %>%
  group_by(country, year) %>%
  summarise(sum = sum(value)) %>%
  left_join(sector_tot, by = c("country", "year")) %>%
  mutate(diff = value - sum,
         perc = diff/value*100,
         keep = perc <= 0.05) %>%
  select(country, year, perc, keep, tot = value)

check_total_country <- as.tibble(unique(check_total[check_total$keep == FALSE,][, c("country", "year")]))

sort(unique(sector$sectors))

#Brazil################
check_BRA <- check_total[check_total$keep == FALSE & check_total$country == "Brazil",] %>%
  left_join(sector[, c("country", "year", "sectors")], by = c("country", "year")) %>%
  distinct()
print_unique_and_count(check_BRA$year) #1995-1999 and 2016
unique(check_BRA$sectors) #"A_F_J_K_L" 1995-1999 "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_S_T" 2016 (missing R and U)

check_BRA_RU <- oecd_current %>% #no U for Brazil
  filter(country == "Brazil" & year %in% c(2000:2015, 2017:2023) & (code == "R" | code == "U" | code == "_T")) %>%
  pivot_wider(id_cols = c(country, year), names_from = "code", values_from = "value") %>%
  mutate(perc_RU = round((.$"R")/.$"_T"*100, 2))

#Difference too large for 1995-1999
#Difference for 2016 (0.37%) = R contribution in other years (0.37% in 2015) => keep

#Colombia################
check_COL <- check_total[check_total$keep == FALSE & check_total$country == "Colombia",] %>%
  left_join(sector[, c("country", "year", "sectors")], by = c("country", "year")) %>%
  distinct()
print_unique_and_count(check_COL$year) #2022
unique(check_COL$sectors) #"A_F_J_K_L" 

#Difference too large => discard

#Korea################
check_KOR <- check_total[check_total$keep == FALSE & check_total$country == "Korea",] %>%
  left_join(sector[, c("country", "year", "sectors")], by = c("country", "year")) %>%
  distinct()
print_unique_and_count(check_KOR$year) #all years
unique(check_KOR$sectors) #A_B_C_F_H_J_K_L_O_P_Q"

#Difference too large => discard

#Malta################
check_MAL <- check_total[check_total$keep == FALSE & check_total$country == "Malta",] %>%
  left_join(sector[, c("country", "year", "sectors")], by = c("country", "year")) %>%
  distinct()
print_unique_and_count(check_MAL$year) #all years
unique(check_MAL$sectors) #"A_C_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U" missing B and D

check_MAL_BD <- oecd_current %>% #no B nor D for Malta
  filter(country == "Malta" & (code == "B" | code == "D" | code == "_T"))

#Would assume that missing = B contributions but no way of checking => keep for now

#Türkiye################
check_TUR <- check_total[check_total$keep == FALSE & check_total$country == "Türkiye",] %>%
  left_join(sector[, c("country", "year", "sectors")], by = c("country", "year")) %>%
  distinct()
print_unique_and_count(check_TUR$year) #2013-2022
unique(check_TUR$sectors) #A_B_C_D_E_F_G_H_I_J_K_M_N_O_P_Q_R_S_T_U" only missing L

check_TUR_L <- oecd_current %>% 
  filter(country == "Türkiye" & year < 2013 & (code == "L" | code == "_T")) %>%
  pivot_wider(id_cols = c(country, year), names_from = "code", values_from = "value") %>%
  mutate(perc_L = round((.$"L")/.$"_T"*100, 2))

#Difference in 2013 (9.32%) close to L contribution in 2012 (9.76%) => keep

quality <- tibble(code = integer(), label = character())

code <- c(1:8)

label <- c("1. No data issue", #1
           "2. Sum of sectors do not add up to B.1g (0.5% threshold)", #4
           "3. R missing but can be inferred from total", #2
           "4. B missing but can be inferred from total", #2
           "5. L missing but can be inferred from total", #2
           "6. M+N missing but total matches", #3
           "7. B is missing but total matches", #3,
           "8. Preliminary/estimated data") #1

for (i in seq_along(code)) {
  quality <- add_row(quality, code = code[i], label = label[i])
}

write_xlsx(quality, "1 - Data/2 - Processed/1 - Global/OECD_quality_codebook.xlsx")

oecd_current_issues <- sector %>%
  left_join(check_total, by = c("country", "year")) %>%
  mutate(quality = case_when(country == "Brazil" & year %in% c(1995:1999) ~ 2,
                             country == "Brazil" & year == 2016 ~ 3,
                             country == "Colombia" & year == 2022 ~ 2,
                             country == "Korea" ~ 2,
                             country == "Malta" ~ 4,
                             country == "Türkiye" & year %in% c(2013:2022) ~ 5,
                             sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" | 
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U" |
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_T" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_O_P_Q_R_S_T" | 
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R" |
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_N_O_P_Q_R_S" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_O_P_Q_R" | 
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_O_P_Q_S" ~ 1, 
                             sectors == "A_B_C_D_E_F_G_H_I_J_K_L_O_P_Q_R_S_T" ~ 6,
                             sectors == "A_C_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U" ~ 7),
         quality_cat = case_when(quality == 1 ~ 1,
                                 quality %in% c(3:5) ~ 2,
                                 quality %in% c(6:7) ~ 3,
                                 quality == 2 ~ 4))

#I'm making the following assumptions for the following missing sectors (when total matches):
#If E is missing it is included in D
#If one of R+S+T+U is missing it is included in one of R+S+T+U
#If N is missing it is included in M

oecd_current_quality_3 <- oecd_current_issues %>%
  filter(quality == 3) %>%
  group_by(iso3, country, year, currency, unit, quality, quality_cat, tot) %>%
  summarise(value_sum = sum(value),
            tot = first(tot),
            name = "oth",
            value = tot - value_sum) %>%
  ungroup() %>%
  select(iso3, country, name, year, currency, unit, quality, quality_cat, value, tot)

oecd_current_quality_4 <- oecd_current_issues %>%
  filter(quality == 4) %>%
  group_by(iso3, country, year, currency, unit, quality, quality_cat, tot) %>%
  summarise(value_sum = sum(value),
            tot = first(tot),
            name = "min",
            value = tot - value_sum) %>%
  ungroup() %>%
  select(iso3, country, name, year, currency, unit, quality, quality_cat, value, tot)

oecd_current_quality_5 <- oecd_current_issues %>%
  filter(quality == 5) %>%
  group_by(iso3, country, year, currency, unit, quality, quality_cat, tot) %>%
  summarise(value_sum = sum(value),
            tot = first(tot),
            name = "bus",
            value = tot - value_sum) %>%
  ungroup() %>%
  select(iso3, country, name, year, currency, unit, quality, quality_cat, value, tot)

#Final

final_current <- oecd_current_issues %>%
  filter(country != "European Union (27 countries from 01/02/2020)") %>%
  mutate(country = case_when(country == "Russia" ~ "Russian Federation",
                              country == "Hong Kong (China)" ~ "Hong Kong SAR, China",
                              country == "Korea" ~ "Korea, Rep.",
                              TRUE ~ country),
         name = case_when(code == "A" ~ "agr",
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
                          code == "U" ~ "oth"),
         quality = ifelse(note %in% c("Provisional value", "Estimated value"), paste(quality, ", 8", sep = ""), as.character(quality))) %>%
  select(iso3, country, name, year, currency, unit, quality, quality_cat, value, tot) %>%
  bind_rows(oecd_current_quality_3 %>%
              mutate(quality = as.character(quality)),
            oecd_current_quality_4 %>%
              mutate(quality = as.character(quality)),
            oecd_current_quality_5 %>%
              mutate(quality = as.character(quality))) %>%
  group_by(iso3, country, name, year, currency, unit, quality_cat) %>%
  summarise(quality = collapse_strings(quality),
            value = sum(value))

growth_final <- final_current %>% 
  filter(year >= 1990) %>%
  group_by(iso3, country, name, currency, unit) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(value) - value)/value,
         growth_to_prev = (value - lag(value))/lag(value)) %>%
  ungroup()

check_all_growth_computed <- growth_final %>%
  group_by(iso3) %>%
  mutate(end_series = max(year[quality_cat < 4], na.rm = TRUE),
         start_series = min(year[quality_cat < 4], na.rm = TRUE)) %>%
  filter(((is.na(growth_to_next) & year != end_series) |
           (is.na(growth_to_prev) & year != start_series)) & quality_cat < 4)

#for some reasons L and R have not been renamed for TUR 2013 and BRA 2016. Found issue, bind_rows happens after the renaming

write_xlsx(growth_final, "1 - Data/2 - Processed/1 - Global/OECD_current.xlsx")

coverage_no_issue_curr <- final_current %>%
  filter(quality_cat <= 2) %>%
  group_by(country) %>%
  summarize(min_year = min(year),
            max_year = max(year),
            full = (max_year - min_year + 1) == length(unique(year)),
            missing = paste(setdiff(min_year:max_year, unique(year)), collapse = ", "))

coverage_issue_default_curr <- final_current %>%
  filter(quality_cat <= 3) %>%
  group_by(country) %>%
  summarize(min_year = min(year),
            max_year = max(year),
            full = (max_year - min_year + 1) == length(unique(year)),
            missing = paste(setdiff(min_year:max_year, unique(year)), collapse = ", "))

objects <- ls()
objects_to_keep <- objects[grep("^coverage", objects)]
rm(list = setdiff(objects, objects_to_keep))

