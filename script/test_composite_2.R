split_composite_series <- function(data, composite_series, component_series, reference_year) {
  # Ensure the data is a data frame
  data <- as.data.frame(data)
  
  # Calculate the sum of component series for the reference year
  reference_data <- subset(data, Year == reference_year & Code %in% component_series)
  total_value <- sum(reference_data$Value)
  
  # Calculate proportions for each component series
  proportions <- sapply(component_series, function(comp) {
    sum(reference_data$Value[reference_data$Code == comp]) / total_value
  })
  
  # Split composite series into components for other years
  for (year in unique(data$Year)) {
    if (year != reference_year) {
      for (comp in composite_series) {
        composite_value <- data$Value[data$Year == year & data$Code == comp]
        if (length(composite_value) > 0) {
          for (i in seq_along(component_series)) {
            new_row <- data.frame(
              Country = unique(data$Country[data$Year == year & data$Code == comp]),
              Year = year,
              Series.Code = unique(data$Series.Code[data$Year == year & data$Code == comp]),
              Subseries = unique(data$Subseries[data$Year == year & data$Code == comp]),
              Code = component_series[i],
              Value = composite_value * proportions[i]
            )
            data <- rbind(data, new_row)
          }
          # Remove the original composite series row
          data <- data[!(data$Year == year & data$Code == comp), ]
        }
      }
    }
  }
  
  return(data)
}

# Example usage:
data <- data.frame(
  Country = rep("X", 15),
  Year = c(2000, 2000, 2000, 2000, 2000, 2001, 2001, 2001, 2001, 2001, 2002, 2002, 2002, 2002, 2002),
  Series.Code = rep(100, 15),
  Subseries = rep(1, 15),
  Code = c("A+B", "C", "D", "E", "A+B", "C", "D", "E", "A", "B", "C", "D", "E"),
  Value = c(100, 30, 80, 120, 120, 40, 90, 140, 80, 50, 50, 110, 160)
)

composite_series <- c("A+B")
component_series <- c("A", "B")
reference_year <- 2002

result <- split_composite_series(data, composite_series, component_series, reference_year)
print(result)
