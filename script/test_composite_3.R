library(dplyr)

# Ensure df is a data frame
if (!is.data.frame(df)) {
  stop("The input df must be a data frame.")
}

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
              iso3 = ifelse(length(unique(df$iso3[df$year == year & df$code == comp])) > 0, unique(df$iso3[df$year == year & df$code == comp]), NA),
              year = year,
              series_code = ifelse(length(unique(df$series_code[df$year == year & df$code == comp])) > 0, unique(df$series_code[df$year == year & df$code == comp]), NA),
              subseries = ifelse(length(unique(df$subseries[df$year == year & df$code == comp])) > 0, unique(df$subseries[df$year == year & df$code == comp]), NA),
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


# Apply the function to each unique combination of iso3, series_code, and subseries
df <- data_composite %>%
  group_by(iso3, series_code, subseries) %>%
  group_modify(~ split_composite_series(.x)) %>%
  ungroup()

# Print the modified data frame
print(df)
