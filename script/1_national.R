library(tidyverse)
library(haven)
library(readxl)
library(writexl)

#Program########################################################################

#List of files and extract relevant information
files <- list.files(path = "1 - Data/2 - Processed/2 - National/", pattern = "national.xlsx", full.names = F) %>%
  tibble(file_name = .,
         file_path = paste("1 - Data/2 - Processed/2 - National/", ., sep = ""),
         iso3 = substr(., 1,3))

#Create objects to store newly created datasets
overlapped <- tibble(iso3 = character(),
                     year = integer(),
                     series = character())
source <- tibble(iso3 = character())
dataset <- tibble(iso3 = character(),
                  sector = character(),
                  year = integer(),
                  series = character(),
                  currency = character(),
                  value = numeric(),
                  base = character(),
                  source = character()) 


#Compile all national files

for (i in 1:nrow(files)) {
  
  message(files$iso3[i])
  
  tryCatch({
  
    data <- read_xlsx(files$file_path[i]) %>%
      filter(name != "tot") %>%
      mutate(year = as.integer(year),
             value = case_when(unit == "Millions" | unit == "Million" ~ 1000000 * as.numeric(value),
                               unit == "Billions" | unit == "Billion" ~ 1000000000 * as.numeric(value),
                               unit == "Thousands" ~ 1000 * as.numeric(value),
                               unit == "Unit" | is.na(unit) ~ as.numeric(value)),
             base = as.character(base)) %>%
      select(-unit)
    
    #Test overlapping
    duplicate <- data %>%
      group_by(name, year, series) %>%
      summarise(count = n()) %>%
      ungroup() %>%
      filter(count > 1) %>%
      group_by(year, series) %>%
      summarise(duplicate_name = unique(name)) %>%
      ungroup() %>%
      group_by(year, series) %>%
      summarise(count = n()) %>%
      filter(count > 7) # This basically means that at least 8 categories need to
    # have multiple years-series values to highlight potential overlapping years.
    # The sensitivity is such that it will always pick up overlapping years but
    # could pick up non-overlapping years as well. Those will need manual checking.
    
    #Highlight overlapping years unaccounted for
    if(!("source" %in% colnames(data)) & nrow(duplicate) > 0 & files$iso3[i] != "MLI") { #MLI "duplicates" are only due to a high level of disaggregation
      
      duplicate <- duplicate %>%
        mutate(iso3 = files$iso3[i]) %>%
        select(iso3, year, series)
      
      overlapped <- overlapped %>%
        bind_rows(duplicate)
      
      message(paste(files$iso3[i], " has overlapping years.", sep = ""))
      
      next
    }
    
    #Highlight overlapping years accounted for
    if ("source" %in% colnames(data) & nrow(duplicate) > 0) {
      
      message(paste(files$iso3[i], " has overlapping years but 'source' exists.", sep = ""))
      
      source <- bind_rows(source, tibble(iso3 = files$iso3[i]))
      
      data <- data %>%
        group_by(source, series) %>%
        mutate(source = paste(min(year), str_sub(max(year), start = -2), sep = "-"))
      
      compiled <- data %>%
        group_by(year, series, name, base, currency, source) %>%
        summarise(value = sum(value, na.rm = TRUE),
                  iso3 = files$iso3[i]) %>%
        ungroup()
      
      total <- compiled %>%
        group_by(year, series, base, currency, source) %>%
        summarise(name = "tot",
                  value = sum(value, na.rm = TRUE),
                  iso3 = files$iso3[i]) %>%
        ungroup()
      
      final <- bind_rows(compiled, total) %>%
        select(iso3, sector = name, year, series, currency, value, base, source)
      
      dataset <- bind_rows(dataset, final)
      
      message(paste(files$iso3[i], " has been merged.", sep = ""))
      
      next
      
    }
    
    #Generate other
    if (!("source" %in% colnames(data))) {
      
      data <- data %>%
        group_by(series) %>%
        mutate(source = paste(min(year), str_sub(max(year), start = -2), sep = "-"))
      
        compiled <- data %>%
          group_by(year, series, name, base, currency, source) %>%
          summarise(value = sum(value, na.rm = TRUE),
                    iso3 = files$iso3[i]) %>%
          ungroup()
        
        total <- compiled %>%
          group_by(year, series, base, currency, source) %>%
          summarise(name = "tot",
                    value = sum(value, na.rm = TRUE),
                    iso3 = files$iso3[i]) %>%
          ungroup()
        
        final <- bind_rows(compiled, total) %>%
          select(iso3, sector = name, year, series, currency, value, base, source)
        
        dataset <- bind_rows(dataset, final)
        
        message(paste(files$iso3[i], " has been merged.", sep = ""))
        
        next
    }
    
  }, error = function(err) {
    
    message(paste("An error happened for: ", files$iso3[i], "; ", conditionMessage(err), sep = ""))
  })
  
}

#Filter out years outside of scope and compute growth
growth <- dataset %>% 
  filter(year >= 1990) %>%
  mutate(base = ifelse(is.na(base), "NA", base)) %>%
  group_by(iso3, sector, series, currency, source) %>%
  arrange(year) %>%
  mutate(growth_to_next = ifelse(base != "-1", (lead(value) - value)/value, NA),
         growth_to_prev = ifelse(base != "-1", (value - lag(value))/lag(value), NA)) %>%
  ungroup()

check_coverage <- growth %>%
  group_by(iso3, series) %>%
  complete(year = full_seq(year, 1)) %>%
  filter((is.na(growth_to_next) | is.na(growth_to_prev)) & base != "-1") %>%
  group_by(iso3, series, sector) %>%
  mutate(count = n()) %>%
  filter(count > 2)

#For JOR, MAR, MNG, PAN, QAT, SLE, TTO (constant), VEN and ZWE => no issues because at least one overlapping year between sources
#For TTO (current) => no overlapping year but it is in current value so just need to compute growth rate between 2011 and 2012 (even though it's from different sources)
#For MWI => no overlapping year AND it's constant value AND using different bases so need to deal with that case manually between 2009 and 2010.
#Because of current methodology this can only be done once I have normalized current values across all sources (UN and OECD included) - same with constant values where base is the previous year

growth_corrected <- growth %>%
  group_by(iso3, sector, series, currency) %>%
  mutate(growth_to_next = ifelse(iso3 == "TTO" & series == "current" & year == 2011, (lead(value) - value)/value, growth_to_next)) %>%
  ungroup()

#All countries with base 1 have the same coverage across sources

base_1 <- growth_corrected %>%
  group_by(iso3, year) %>%
  mutate(base1 = ifelse("-1" %in% unique(base), 1, 0)) %>%
  filter(base1 == 1) %>%
  ungroup() %>%
  mutate(source_n = case_when(iso3 %in% c("BRA", "KGZ", "MDA", "MLI", "TUN", "UZB", "XKX") ~ 1,
                              iso3 == "MAR" & source == "1998-13" ~ 1,
                              iso3 == "MAR" & source == "2007-20" ~ 2,
                              iso3 == "MAR" & source == "2014-22" ~ 3)) %>%
  select(iso3, sector, year, series, value, source_n, source) %>%
  pivot_wider(id_cols = c(iso3, sector, year,source_n), names_from = "series", values_from = "value") %>%
  group_by(iso3, sector, source_n) %>%
  arrange(year) %>%
  mutate(growth_to_next = (lead(constant) - current) / current,
         growth_to_prev = (constant - lag(current)) / lag(current)) %>%
  ungroup() %>%
  select(iso3, sector, year, source_n, value = constant, growth_to_next, growth_to_prev) %>%
  group_by(iso3, source_n) %>%
  mutate(source = paste(min(year), str_sub(max(year), start = -2), sep = "-"),
         base = "-1") %>%
  ungroup() %>%
  select(-source_n)

growth_final <- growth_corrected %>%
  left_join(base_1, by = c("iso3", "sector", "year", "value", "source", "base")) %>%
  mutate(growth_to_next = case_when(!is.na(growth_to_next.x) & !is.na(growth_to_next.y) ~ 9999,
                            !is.na(growth_to_next.x) & is.na(growth_to_next.y) ~ growth_to_next.x,
                            is.na(growth_to_next.x) & !is.na(growth_to_next.y) ~ growth_to_next.y,
                            is.na(growth_to_next.x) & is.na(growth_to_next.y) ~ NA),
         growth_to_prev = case_when(!is.na(growth_to_prev.x) & !is.na(growth_to_prev.y) ~ 9999,
                                    !is.na(growth_to_prev.x) & is.na(growth_to_prev.y) ~ growth_to_prev.x,
                                    is.na(growth_to_prev.x) & !is.na(growth_to_prev.y) ~ growth_to_prev.y,
                                    is.na(growth_to_prev.x) & is.na(growth_to_prev.y) ~ NA))

check_double_growth <- nrow(growth_final %>% 
                              filter(growth_to_next == 9999 | growth_to_prev == 9999))
check_all_growth_computed <- growth_final %>%
  mutate(end_series = as.numeric(paste("20", str_sub(source, start = -2), sep = "")),
         start_series = as.numeric(str_sub(source, end = -4)),
         start_series = ifelse(start_series < 1990, 1990, start_series)) %>%
  filter((is.na(growth_to_next) & year != end_series) |
           (is.na(growth_to_prev) & year != start_series))

#TLS Due to NaN issues
#MNG min, utl, hos, fin and oth are only recorded from 1995 onward. Remove those 1990-1994


final <- growth_final %>%
  group_by(iso3, series, source) %>%
  mutate(max_year = max(year)) %>%
  ungroup() %>%
  group_by(iso3, year, sector, series) %>%
  mutate(overlapping = ifelse(n() > 1, 1, 0),
         keep = ifelse((overlapping == 1 & max_year == max(max_year)) | overlapping == 0, 1,0)) %>%
  ungroup() %>%
  filter(keep == 1 & !(iso3 == "MNG" & year %in% c(1990:1994) & series == "constant")) %>%
  select(-c(growth_to_prev.x, growth_to_prev.y, growth_to_next.x, growth_to_next.y, max_year, overlapping, keep))

#Save outputs
write_xlsx(final, "1 - Data/2 - Processed/2 - National//national_all.xlsx")

