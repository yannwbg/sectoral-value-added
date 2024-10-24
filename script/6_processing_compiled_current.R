library(tidyverse)

#Load data----
data <- read_csv("data/temp/current_compiled.csv") %>%
  filter(!(is.na(growth_to_prev) & is.na(growth_to_next)) & sector != "tot") 

#Process----
complete_coverage <- expand.grid(iso3 = unique(data$iso3), year = unique(data$year), sector = unique(data$sector))

data_expanded <- complete_coverage %>%
  left_join(data, by = c("iso3", "year", "sector"))

test <- data %>%
  group_by(iso3, year, sector) %>%
  mutate(n = n()) %>%
  filter(n != 1)
