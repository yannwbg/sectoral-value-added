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

#OECD data constant#############################################################

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

#Constant
oecd_constant <- oecd %>%
  filter(series_type != "Current prices")

nrow(unique(oecd_constant[, c("country", "code", "year")])) #duplicata
sort(unique(oecd_constant$code))


#When duplicata, prioritize rebased values then previous years then chain linked
priority_order <-  c("Chain linked volume (rebased)", "Chained linked volume",  "Previous year prices")

#check sector coverage based on series - and select series with the highest average of sectors
series_to_keep <- oecd_constant %>%
  filter(code != "_T") %>%
  group_by(country, year, series_type) %>%
  summarize(sectors = paste(sort(unique(code)), collapse = "_"),
            n = n()) %>%
  group_by(country, series_type) %>%
  mutate(min_year = min(year),
         max_year = max(year),
         n_year = max_year - min_year + 1,
         average_n = mean(n)) %>%
  group_by(country) %>%
  filter(average_n == max(average_n)) %>%
  group_by(country, year) %>%
  arrange(match(series_type, priority_order)) %>% 
  slice(1) %>%  
  ungroup() %>%
  select(country, year, series_type)

nrow(unique(oecd_constant[, c("country", "year")])) #we're losing two country-year


oecd_constant_series <- series_to_keep %>%
  left_join(oecd_constant, by = c("country", "year", "series_type")) %>%
  filter(code != "_T")
  
sector_tot <- series_to_keep %>%
  left_join(oecd_constant, by = c("country", "year", "series_type")) %>%
  filter(code == "_T")

check_total <- oecd_constant_series %>%
  group_by(country, year) %>%
  summarise(sum = sum(value), 
            sectors = paste(sort(unique(code)), collapse = "_")) %>%
  left_join(sector_tot, by = c("country", "year")) %>%
  mutate(diff = value - sum,
         perc = diff/value*100,
         keep = (perc <= 0.05 & perc >= -5)) %>%
  select(country, year, perc, series_type, keep, sectors, tot = value, sum)

check_total_country <- as.tibble(unique(check_total[check_total$keep == FALSE,][, c("country", "year")]))


#Albania################
check_ALB <- check_total[check_total$keep == FALSE & check_total$country == "Albania",]
print_unique_and_count(check_ALB$year) #1996-2000 2007-2009 2014
unique(check_ALB$sectors) #"A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R" missing S+T+U

#Difference less than 1% between 2007-2009 and 2014 and chained linked volume so would keep - discard rest

#Australia################
check_AUS <- check_total[check_total$keep == FALSE & check_total$country == "Australia",]
print_unique_and_count(check_AUS$year) #1990-2014 2018-2019
unique(check_AUS$sectors) #"A_B_C_D_F_G_H_I_J_K_L_M_N_O_P_Q_R_S" missing E+T+U

#Difference less than 1% and chained linked volume so would keep

#Cameroon################
check_CAM <- check_total[check_total$keep == FALSE & check_total$country == "Cameroon",]
print_unique_and_count(check_CAM$year) #1993-2009
unique(check_CAM$sectors) #"A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference too large - discard

#Canada################
check_CAN <- check_total[check_total$keep == FALSE & check_total$country == "Canada",]
print_unique_and_count(check_CAN$year) #2007
unique(check_CAN$sectors) #"A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference less than 1% and chained linked volume so would keep

#Colombia################
check_COL <- check_total[check_total$keep == FALSE & check_total$country == "Colombia",]
print_unique_and_count(check_COL$year) #2006 2011-2014 2017-2020
unique(check_COL$sectors) #"A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_T" missing S+U

#Difference less than 1% and chained linked volume so would keep

#Costa Rica################
check_COS <- check_total[check_total$keep == FALSE & check_total$country == "Costa Rica",]
print_unique_and_count(check_COS$year) #1991-1994
unique(check_COS$sectors) #"A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference too large - discard

#Finland################
check_FIN <- check_total[check_total$keep == FALSE & check_total$country == "Finland",]
print_unique_and_count(check_FIN$year) #1990-1997 2007-2008
unique(check_FIN$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference less than 1% for 2007-2008 and chained linked volume so would keep => discard rest

#Hungary################
check_HUN <- check_total[check_total$keep == FALSE & check_total$country == "Hungary",]
print_unique_and_count(check_HUN$year) #1995-1996
unique(check_HUN$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference too large - discard

#Korea################
check_KOR <- check_total[check_total$keep == FALSE & check_total$country == "Korea",]
print_unique_and_count(check_KOR$year) #1995-2021
unique(check_KOR$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_O_P_Q_T" missing N+R+S+U

#Difference too large - discard

#Lithuania################
check_LIT <- check_total[check_total$keep == FALSE & check_total$country == "Lithuania",]
print_unique_and_count(check_LIT$year) #2012-2013
unique(check_LIT$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference less than 1% and chained linked volume so would keep

#Malta################
check_MAL <- check_total[check_total$keep == FALSE & check_total$country == "Malta",]
print_unique_and_count(check_MAL$year) #2000-2011 2013-2021
unique(check_MAL$sectors) # "A_C_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U" missing B

#Discard 2000
# for the other years I would assume that difference == B contribution but no way to check

#Romania################
check_ROU <- check_total[check_total$keep == FALSE & check_total$country == "Romania",]
print_unique_and_count(check_ROU$year) #1995
unique(check_ROU$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U" no missing

#Difference too large - discard

#Serbia################
check_SRB <- check_total[check_total$keep == FALSE & check_total$country == "Serbia",]
print_unique_and_count(check_SRB$year) #1996-2007 2019-2021
unique(check_SRB$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference greater than 1% at random places but less than 1% after 2006 (included) would discard the rest

#Türkiye################
check_TUR <- check_total[check_total$keep == FALSE & check_total$country == "Türkiye",]
print_unique_and_count(check_TUR$year) #2007-2010 2021-2022
unique(check_TUR$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

#Difference less than 1% and chained linked volume so would keep

#United Kingdom################
check_GBR <- check_total[check_total$keep == FALSE & check_total$country == "United Kingdom",]
print_unique_and_count(check_GBR$year) #1995-1999
unique(check_GBR$sectors) # "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" missing U

##Difference too large - discard

oecd_constant_issues <- oecd_constant_series %>%
  left_join(check_total, by = c("country", "year", "series_type")) %>%
  mutate(quality = case_when(country == "Albania" & year %in% c(1996:2000) ~ 2,
                             country == "Cameroon" & year %in% c(1993:2009) ~ 2,
                             country == "Costa Rica" & year %in% c(1991:1994) ~ 2,
                             country == "Finland" & year %in% c(1990:1997) ~ 2,
                             country == "Hungary" & year %in% c(1995:1996) ~ 2,
                             country == "Malta" & year == 2000 ~ 2,
                             country == "Romania" & year == 1995 ~ 2,
                             country == "Serbia" & year %in% c(1995:2005) ~ 2,
                             country == "United Kingdom" & year %in% c(1995:1999) ~ 2,
                             country == "Korea" ~ 2,
                             country == "Malta" ~ 4,
                             sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T" | 
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U" |
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_T" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_O_P_Q_R_S_T" | 
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R" |
                               sectors == "A_B_C_D_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_N_O_P_Q_R_S" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_O_P_Q_R" | 
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_O_P_Q_S" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_M_O_P_Q_S_T" ~ 1, 
                             sectors == "A_B_C_D_E_F_G_H_I_J_K_L_O_P_Q_R_S_T" |
                               sectors == "A_B_C_D_F_G_H_I_J_K_L_O_P_Q_R_T" ~ 6,
                             sectors == "A_C_E_F_G_H_I_J_K_L_M_N_O_P_Q_R_S_T_U" ~ 7),
         quality_cat = case_when(quality == 1 ~ 1,
                                 quality %in% c(3:5) ~ 2,
                                 quality %in% c(6:7) ~ 3,
                                 quality == 2 ~ 4))

#I'm making the following assumptions for the following missing sectors (when total matches):
#If E is missing it is included in D
#If one of R+S+T+U is missing it is included in one of R+S+T+U
#If N is missing it is included in M

oecd_constant_quality_4 <- oecd_constant_issues %>%
  filter(quality == 4) %>%
  group_by(iso3, country, year, currency, unit, series_type, base, quality_cat, quality) %>%
  summarise(value_sum = sum(value),
            tot = first(tot),
            name = "min",
            value = tot - value_sum) %>%
  ungroup() %>%
  select(iso3, country, name, year, currency, unit, series_type, base, quality, quality_cat, value, tot)

final_constant <- oecd_constant_issues %>%
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
  select(iso3, country, name, year, currency, unit, series_type, base, quality, quality_cat, value) %>%
  bind_rows(oecd_constant_quality_4 %>%
              mutate(quality = as.character(quality))) %>%
  group_by(iso3, country, name, year, currency, unit, series_type, base, quality_cat) %>%
  summarise(value = sum(value),
            quality = collapse_strings(quality)) %>%
  ungroup() %>%
  mutate(base = ifelse(series_type == "Previous year prices", -1, as.numeric(base)))

growth <- final_constant %>% 
  filter(year >= 1990) %>%
  group_by(iso3, country, name, currency, unit, base) %>%
  arrange(year) %>%
  mutate(growth_to_next = ifelse(base != -1, (lead(value) - value)/value, NA),
         growth_to_prev = ifelse(base != -1, (value - lag(value))/lag(value), NA)) %>%
  ungroup()

base1 <- growth %>%
  filter(base == -1)

growth_base1 <- read_xlsx("1 - Data/2 - Processed/1 - Global/OECD_current.xlsx") %>%
  filter(quality_cat < 4 & iso3 %in% base1$iso3) %>%
  select(iso3, country, name, year, currency, unit, quality, quality_cat, current = value) %>%
  left_join(base1 %>%
              select(iso3, country, name, year, currency, base, unit, quality, quality_cat, constant = value), by = c("iso3", "country", "name", "year", "currency", "unit"), suffix = c("_current", "_constant")) %>%
  group_by(iso3, name) %>%
  complete(year = full_seq(year, 1)) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(constant) - current) / current,
         growth_to_prev = (constant - lag(current)) / lag(current)) %>%
  ungroup() %>%
  mutate(quality_constant = ifelse(is.na(quality_constant) & is.na(constant), quality_current, quality_constant),
         quality_cat_constant = ifelse(is.na(quality_cat_constant) & is.na(constant), quality_cat_current, quality_cat_constant),
         base = ifelse((!is.na(growth_to_next) | !is.na(growth_to_prev)), -9999, base)) %>% 
  filter(!if_all(c(constant, growth_to_prev, growth_to_next), is.na)) %>%
  select(iso3, country, name, year, currency, unit, base, quality = quality_constant, quality_cat = quality_cat_constant, value = constant, growth_to_next, growth_to_prev)
  
growth_final <- growth %>%
  filter(base != -1) %>%
  bind_rows(growth_base1)

check_base <- growth_final %>%
  group_by(iso3) %>%
  mutate(unique_base = as.numeric(length(unique(base[quality_cat < 4])) == 1)) %>%
  filter(unique_base == 0 & quality_cat < 4) %>%
  select(iso3, year) %>%
  distinct()

#CHL : in 2018 constant values uses base 2018 while the rest uses -1 - discard
#CYP, SVN : in 1995 base = 2010, - 1 for rest - discard for now, needs to be dealt with later (low priority)
#ESP, GRC, ITA, LUX, POL : in 1995 base = 2015, - 1 for rest - discard for now, needs to be dealt with later (low priority)
#PRT : in 1995 base = 2016, - 1 for rest - discard for now, needs to be dealt with later (low priority)

growth_trimmed <- growth_final %>%
  filter(!(iso3 %in% c("CYP", "SVN", "ESP", "GRC", "ITA", "LUX", "POL", "PRT") & year == 1995) & !(iso3 == "CHL" & year == 2018))

check_base <- growth_trimmed %>%
  group_by(iso3) %>%
  mutate(unique_base = as.numeric(length(unique(base[quality_cat < 4])) == 1)) %>%
  filter(unique_base == 0 & quality_cat < 4) %>%
  select(iso3, year) %>%
  distinct()

write_xlsx(growth_trimmed, "1 - Data/2 - Processed/1 - Global/OECD_constant.xlsx")

coverage_no_issue_const <- final_constant %>%
  filter(quality_cat <= 2) %>%
  group_by(country) %>%
  summarize(min_year = min(year),
            max_year = max(year),
            full = (max_year - min_year + 1) == length(unique(year)),
            missing = paste(setdiff(min_year:max_year, unique(year)), collapse = ", "))

coverage_issue_default_const <- final_constant %>%
  filter(quality_cat <= 3) %>%
  group_by(country) %>%
  summarize(min_year = min(year),
            max_year = max(year),
            full = (max_year - min_year + 1) == length(unique(year)),
            missing = paste(setdiff(min_year:max_year, unique(year)), collapse = ", "))

objects <- ls()
objects_to_keep <- objects[grep("^coverage", objects)]
rm(list = setdiff(objects, objects_to_keep))


coverage_no_issue_final <- coverage_no_issue_const %>%
  full_join(coverage_no_issue_curr, by = "country", suffix = c("_constant", "_current"))


write_xlsx(coverage_no_issue_final, "1 - Data/2 - Processed/1 - Global/OECD_coverage_quality_cat_1_2.xlsx")

coverage_issue_default_final <- coverage_issue_default_const %>%
  full_join(coverage_issue_default_curr, by = "country", suffix = c("_constant", "_current"))

write_xlsx(coverage_issue_default_final, "1 - Data/2 - Processed/1 - Global/OECD_coverage_quality_cat_1_2_3.xlsx")
