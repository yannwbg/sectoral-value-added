library(tidyverse)
library(stringr)
library(writexl)
library(readxl)


#BIH############################################################################
data <- read_xlsx("1 - Data/1 - Raw/BIH_national_2006-2023.xlsx") %>%
  filter(str_detect(INDICATOR, "NGDPVA_NACE2|NGDPVA_R_CH_NACE2")) %>%
  mutate(orig_name = INDICATOR,
         name = case_when(str_detect(INDICATOR, "NACE2_A_X") ~ "agr",
                              str_detect(INDICATOR, "NACE2_B_X") ~ "min",
                              str_detect(INDICATOR, "NACE2_C_X") ~ "man",
                              str_detect(INDICATOR, "NACE2_D_X|NACE2_E_X") ~ "utl",
                              str_detect(INDICATOR, "NACE2_F_X") ~ "ctr",
                              str_detect(INDICATOR, "NACE2_G_X") ~ "trd",
                              str_detect(INDICATOR, "NACE2_H_X|NACE2_J_X") ~ "con",
                              str_detect(INDICATOR, "NACE2_I_X") ~ "hos",
                              str_detect(INDICATOR, "NACE2_K_X") ~ "fin",
                              str_detect(INDICATOR, "NACE2_L_X|NACE2_M_X|NACE2_N_X") ~ "bus",
                              str_detect(INDICATOR, "NACE2_O_X|NACE2_P_X|NACE2_Q_X") ~ "adm",
                              str_detect(INDICATOR, "NACE2_R_X|NACE2_S_X|NACE2_T_X|NACE2_U_X") ~ "oth"),
         series = case_when(str_detect(INDICATOR, "NGDPVA_NACE2_") ~ "current",
                            str_detect(INDICATOR, "NGDPVA_R_CH_NACE2_") ~ "constant"),
         base = ifelse(str_detect(INDICATOR, "NGDPVA_R_CH_NACE2_"), BASE_PER, NA),
         year = TIME_PERIOD,
         unit = case_when(UNIT_MULT == 3 ~ "Thousands",
                          UNIT_MULT == 6 ~ "Millions",
                          UNIT_MULT == 9 ~ "Billions"),
         value = OBS_VALUE,
         currency = "Bosnia-Herzegovina Convertible Mark") %>%
  select(orig_name, name, year, value, currency, unit, series,base) %>%
  filter(!is.na(name) & !str_detect(year, "Q")) #exclude quarterly data

write_xlsx(data, "1 - Data/2 - Processed/BIH_national.xlsx")

#KHM############################################################################
data <- read_xlsx("1 - Data/1 - Raw/KHM_national_1993-2022.xlsx") %>%
  filter(str_detect(INDICATOR, "NGDPVA_|NGDPVA_R_")) %>%
  mutate(orig_name = INDICATOR,
         name = case_when(str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_A_X") ~ "agr",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_B_X") ~ "min",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_C_X") ~ "man",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_D351TD352_E36_X") ~ "utl",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_F_X") ~ "ctr",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_G_X") ~ "trd",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_H_J_X") ~ "con",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_I_X") ~ "hos",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_K64_X") ~ "fin",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_L_X") ~ "bus",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_O_X") ~ "adm",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_S_X") ~ "oth"),
         series = case_when(str_detect(INDICATOR, "NGDPVA_ISIC4") ~ "current",
                            str_detect(INDICATOR, "NGDPVA_R_ISIC4") ~ "constant"),
         base = ifelse(str_detect(INDICATOR, "NGDPVA_R_ISIC4"), BASE_PER, NA),
         year = TIME_PERIOD,
         unit = case_when(UNIT_MULT == 3 ~ "Thousands",
                          UNIT_MULT == 6 ~ "Millions",
                          UNIT_MULT == 9 ~ "Billions"),
         value = OBS_VALUE,
         currency = "Cambodia Riel") %>%
  select(orig_name, name, year, value, currency, unit, series,base) %>%
  filter(!is.na(name) & !str_detect(year, "Q")) #exclude quarterly data

write_xlsx(data, "1 - Data/2 - Processed/KHM_national.xlsx")         

#PAK############################################################################
data <- read_xlsx("1 - Data/1 - Raw/PAK_national_2000-2023.xlsx") %>%
  filter(str_detect(INDICATOR, "NGDPVA_|NGDPVA_R_")) %>%
  mutate(orig_name = INDICATOR,
         name = case_when(str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_A_X") ~ "agr",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_B_X") ~ "min",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_C_X") ~ "man",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_D_X") ~ "utl",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_F_X") ~ "ctr",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_G_X") ~ "trd",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_H_X") ~ "con",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_I_X") ~ "hos",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_J_X") ~ "con",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_K_X") ~ "fin",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_L_X") ~ "bus",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_O_X") ~ "adm",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_P_X") ~ "adm",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_Q_X") ~ "adm",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_S_X") ~ "oth"),
         series = case_when(str_detect(INDICATOR, "NGDPVA_ISIC4") ~ "current",
                            str_detect(INDICATOR, "NGDPVA_R_ISIC4") ~ "constant"),
         base = ifelse(str_detect(INDICATOR, "NGDPVA_R_ISIC4"), 2016, NA), #year where NGDPVA == NGDPVA_R
         year = TIME_PERIOD,
         unit = case_when(UNIT_MULT == 3 ~ "Thousands",
                          UNIT_MULT == 6 ~ "Millions",
                          UNIT_MULT == 9 ~ "Billions"),
         value = OBS_VALUE,
         currency = "Pakistani Rupee") %>%
  select(orig_name, name, year, value, currency, unit, series,base) %>%
  filter(!is.na(name) & !str_detect(year, "Q")) #exclude quarterly data

write_xlsx(data, "1 - Data/2 - Processed/PAK_national.xlsx")

#QAT############################################################################
data <- read_xlsx("1 - Data/1 - Raw/QAT_national_2009-2023.xlsx") %>%
  filter(str_detect(INDICATOR, "NGDPVA_|NGDPVA_R_")) %>%
  mutate(orig_name = INDICATOR,
         name = case_when(str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_A_X") ~ "agr",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_B_X") ~ "min",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_C_X") ~ "man",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_DTE_X") ~ "utl",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_F_X") ~ "ctr",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_G_X") ~ "trd",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_H_X") ~ "con",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_I_X") ~ "hos",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_J_X") ~ "con",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_K_X") ~ "fin",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_L_X") ~ "bus",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_MTN_X") ~ "bus",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_O_X") ~ "adm",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_P_X") ~ "adm",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_Q_X") ~ "adm",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_RTS_X") ~ "oth",
                          str_detect(INDICATOR, "NGDPVA_(R_)?ISIC4_T_X") ~ "oth"),
         series = case_when(str_detect(INDICATOR, "NGDPVA_ISIC4") ~ "current",
                            str_detect(INDICATOR, "NGDPVA_R_ISIC4") ~ "constant"),
         base = ifelse(str_detect(INDICATOR, "NGDPVA_R_ISIC4"), BASE_PER, NA), 
         unit = case_when(UNIT_MULT == 3 ~ "Thousands",
                          UNIT_MULT == 6 ~ "Millions",
                          UNIT_MULT == 9 ~ "Billions"),
         value = OBS_VALUE,
         currency = "Qatari Riyal",
         year = as.numeric(str_sub(TIME_PERIOD, start = 1, end = 4))) %>%
  select(orig_name, name, year, value, currency, unit, series,base) %>%
  filter(!is.na(name) & year != 2023 & year > 2010) %>% #2023 values are only up to Q3 and 2009 and 2010 data are only available for 4 sectors AND are both equal to 2010 values so probably errors.
  group_by(orig_name, name, year, currency, unit, series, base) %>%
  summarise(value = sum(as.numeric(value))) %>% #Sum of Qs = yearly values from other sources
  select(orig_name, name, year, value, currency, unit, series,base)

#Checks
complete_df <- expand.grid(
  orig_name = unique(data$orig_name),
  year = unique(data$year)
)

missing_data <- merge(complete_df, data, by = c("orig_name", "year"), all.x = TRUE)
missing_years <- missing_data[is.na(missing_data$value), ]


write_xlsx(data, "1 - Data/2 - Processed/QAT_national.xlsx")
