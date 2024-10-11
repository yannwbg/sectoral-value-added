library(tidyverse)

#Clean data----

#Load country list 
country_list <- read_csv("data/processed/country_list.csv") %>%
  select(iso3, wb_name, un_name)

#Load original data
files_list <- list.files(path = "data/raw/global", pattern = "3_constant", full.names = TRUE)

raw <- map(files_list, function(x) {
  
  read_csv(x,
           col_types = c("cccccnncnncnc")) %>%
    select(country = "Country or Area",
           table_code = "SNA93 Table Code",
           subgroup = "Sub Group",
           label = "Item",
           code = "SNA93 Item Code",
           year = "Fiscal Year",
           series_code = "Series",
           currency = "Currency",
           base = "Base Year",
           SNA = "SNA System",
           year_type = "Fiscal Year Type",
           value = "Value",
           note = "Value Footnotes") %>%
    mutate(part = str_extract(x, "(?<=part)\\d+")) #Since the data was downloaded in 3 parts the footonote numbers will not match between files => need a file id that would help us match the correct footnotes
}) %>%
  do.call(rbind, .)

#Get footnotes definitions
footnote <- raw %>%
  filter(str_detect(country, "[0-9]") & !str_detect(country, "[A-Za-z]")) %>% #All footnotes and only footnotes have a number but no character in the "country" column
  select(note_code = country,
         note_label = table_code,
         part)

#Get data only (without footnotes), filter relevant years and join iso3 codes

data <- raw %>%
  filter(!str_detect(country, "[0-9]") & year >= 1990) %>%
  left_join(country_list, by = c("country" = "un_name"), keep = FALSE) %>%
  filter(!is.na(wb_name)) %>%
  mutate(country = wb_name) %>%
  select(iso3, everything(), - c(wb_name, table_code, subgroup)) #both table_code and subgroup are identical across all rows and do not bring any relevant information

#Save cleaned and ancillary datasets

write_csv(data, "data/temp/un3_constant.csv")
write_csv(footnote, "data/temp/un3_constant_footnote.csv")
