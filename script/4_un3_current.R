library(tidyverse)
library(haven)
library(readxl)
library(writexl)
library(tools)

# Function to filter the dataset
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

#UN data current################################################################

#original data (just changed column names for ease)
current_orig <- read_csv("1 - Data/1 - Raw/Global/UNdata_3_current_part1.csv") %>%
  select(country = "Country or Area",
         table_code = "SNA93 Table Code",
         subgroup = "Sub Group",
         label = "Item",
         code = "SNA93 Item Code",
         year = "Year",
         series_code = "Series",
         currency = "Currency", 
         SNA = "SNA System",
         year_type = "Fiscal Year Type",
         value = "Value",
         note = "Value Footnotes") %>%
  bind_rows(read_csv("1 - Data/1 - Raw/Global/UNdata_3_current_part2.csv") %>%
              select(country = "Country or Area",
                     table_code = "SNA93 Table Code",
                     subgroup = "Sub Group",
                     label = "Item",
                     code = "SNA93 Item Code",
                     year = "Year",
                     series_code = "Series",
                     currency = "Currency", 
                     SNA = "SNA System",
                     year_type = "Fiscal Year Type",
                     value = "Value",
                     note = "Value Footnotes"),
            read_csv("1 - Data/1 - Raw/Global/UNdata_3_current_part3.csv") %>%
              select(country = "Country or Area",
                     table_code = "SNA93 Table Code",
                     subgroup = "Sub Group",
                     label = "Item",
                     code = "SNA93 Item Code",
                     year = "Year",
                     series_code = "Series",
                     currency = "Currency", 
                     SNA = "SNA System",
                     year_type = "Fiscal Year Type",
                     value = "Value",
                     note = "Value Footnotes"))

sectors <- unique(current_orig[, c("code", "label")])

#Get footnotes definitions
current_footnote <- current_orig %>%
  filter(str_detect(country, "[0-9]") & !str_detect(country, "[A-Za-z]")) %>%
  select(note_code = country,
         note_label = table_code)

#Get data only (without footnotes) and filter sectors (after renaming countries based on unmatch values)
#We're keeping all country-year for now in case we need to infer values based on other years
current <- current_orig %>%
  filter(!str_detect(country, "[0-9]") & str_detect(country, "[A-Za-z]")) %>%
  mutate(country = case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                             country == "Congo" ~ "Congo, Rep.",
                             country == "Democratic Republic of the Congo" ~ "Congo, Dem. Rep.",
                             country == "China, Hong Kong Special Administrative Region" ~ "Hong Kong SAR, China",
                             country == "Egypt" ~ "Egypt, Arab Rep.",
                             country == "Gambia" ~ "Gambia, The",
                             country == "Democratic Yemen [former]" ~ "Yemen, Rep.",
                             country == "Iran (Islamic Republic of)" ~ "Iran, Islamic Rep.",
                             country == "Lao People's Democratic Republic" ~ "Lao PDR",
                             country == "Republic of Korea" ~ "Korea, Dem. People's Rep.",
                             country == "Republic of Moldova" ~ "Moldova",
                             country == "Slovakia" ~ "Slovak Republic",
                             country == "State of Palestine" ~ "West Bank and Gaza",
                             country == "Yemen" ~ "Yemen, Rep.",
                             country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela, RB",
                             country == "Kyrgyzstan" ~ "Kyrgyz Republic",
                             TRUE ~ country)) %>%
  filter_sectors_ISIC3()

current_check <- current %>%
  group_by(country, series_code, year) %>%
  mutate(sector = paste(sort(unique(code)), collapse = "+"),
         n_sector = length(unique(code)),
         full_sector = (sector == "A+B+B.1g+C+D+E+F+G+H+I+J+K+L+M+N+O+P" | sector == "A+B+B.1g+C+D+E+F+G+H+I+J+K+L+M+N+O" | sector == "A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P" | sector == "A+B+C+D+E+F+G+H+I+J+K+L+M+N+O"),
         sum_sector = sum(value[code != "B.1g"]),
         total_value = sum(value[code == "B.1g"]),
         sum_check = (abs((sum_sector-total_value)/ total_value) <= 0.05 | total_value == 0),
         sum_check_10 = (abs((sum_sector-total_value)/ total_value) <= 0.1 | total_value == 0)) %>%
  group_by(country, series_code) %>%
  mutate(min_year = min(year),
         max_year = max(year),
         n_year = length(unique(year)),
         full_year = n_year == max_year - min_year + 1,
         unique_sector_group = length(unique(sector)) == 1, 
         all_check = (full_sector & sum_check & full_year & unique_sector_group),
         all_but_sector = (sum_check & full_year & unique_sector_group)) %>%
  ungroup() %>%
  mutate()

current_growth <- current_check %>%
  filter(all_but_sector == TRUE & code != "B.1g") %>%
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
  group_by(country, series_code, year, code) %>%
  mutate(quality = case_when("M+N+O" %in% unique(code) ~ "2",
                             "G+H" %in% unique(code) ~ "3",
                             "J+K" %in% unique(code) ~ "4",
                             unique(all_check) == TRUE ~ "1")) %>%
  group_by(country, series_code, year) %>%
  mutate(quality = paste(sort(unique(quality[!is.na(quality)])), collapse = ", ")) %>%
  group_by(country, year, series_code, name, currency, SNA, year_type, sector, n_sector, sum_sector, total_value, all_check, quality) %>%
  summarise(value = sum(value)) %>%
  group_by(country, name, series_code) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(value) - value)/value,
         growth_to_prev = (value - lag(value))/lag(value)) %>%
  ungroup()
  
current_series <- current_growth %>%
  group_by(country, year) %>%
  mutate(keep = series_code == max(series_code[str_detect(quality, "1")])) %>%
  group_by(country) %>%
  mutate(main = series_code == max(series_code[str_detect(quality, "1")]))
 
write_xlsx(current_series, "1 - Data/2 - Processed/1 - Global/un3_current_growth.xlsx")
