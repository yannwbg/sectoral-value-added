library(dplyr)


#NEED TO BE ABLE TO LOOP THROUGH COUNTRY-SERIES_CODE-SUBSERIES1111111

split_composite_series <- function(df) {
  
  df$adjusted_composite <- FALSE

  # Identify composite series and their components
  composite_series <- unique(df$code[grepl("\\+", df$code)])
  
  # Function to find the closest reference year
  find_reference_year <- function(df, components) {
    years_with_components <- df %>%
      filter(code %in% components) %>%
      group_by(year) %>%
      summarise(count = n()) %>%
      filter(count == length(components)) %>%
      pull(year)
    
    if (length(years_with_components) == 0) {
      stop("No reference year found with all components available.")
    }
    
    return(min(years_with_components))
  }
  
  # Split composite series into components for each composite
  for (comp in composite_series) {
    components <- unlist(strsplit(comp, "\\+"))
    
    # Find the closest reference year
    reference_year <- find_reference_year(df, components)
    
    # Calculate proportions for each component series
    reference_df <- subset(df, year == reference_year & code %in% components)
    total_value <- sum(reference_df$value)
    proportions <- sapply(components, function(comp) {
      sum(reference_df$value[reference_df$code == comp]) / total_value
    })
    
    # Apply proportions to split composite series in other years
    for (year in unique(df$year)) {
      if (year != reference_year) {
        composite_value <- df$value[df$year == year & df$code == comp]
        if (length(composite_value) > 0) {
          for (i in seq_along(components)) {
            new_row <- data.frame(
              iso3 = unique(df$iso3[df$year == year & df$code == comp]),
              year = year,
              series_code = unique(df$series_code[df$year == year & df$code == comp]),
              subseries = unique(df$subseries[df$year == year & df$code == comp]),
              code = components[i],
              value = composite_value * proportions[i],
              adjusted_composite = TRUE
            )
            df <- rbind(df, new_row)
          }
          # Remove the original composite series row
          df <- df[!(df$year == year & df$code == comp), ]
        }
      }
    }
  }
  
  return(df)
}

# Example usage:
data_test <- data.frame(
  iso3 = rep("X", 13),
  year = c(2000, 2000, 2000, 2000, 2001, 2001, 2001, 2001, 2002, 2002, 2002, 2002, 2002),
  series_code = rep(100, 13),
  subseries = rep(1, 13),
  code = c("A+B", "C", "D", "E", "A+B", "C", "D", "E", "A", "B", "C", "D", "E"),
  value = c(100, 30, 80, 120, 120, 40, 90, 140, 80, 50, 50, 110, 160)
)

df <- data_composite %>%
  filter(iso3 == "BOL" & series_code == 20) %>%
  select(iso3, year, series_code, subseries, code, value)

result <- split_composite_series(data_test)
print(result)
