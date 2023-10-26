data <- read.csv("Wholesale_customers_data.csv")
calculate_deviation <- function(data, outlier_threshold = NULL) {
  # Split the data by region
  data_by_region <- split(data, data$Region)
  
  # Calculate the mean for each region
  region_means <- lapply(data_by_region, function(region_data) {
    col_means <- colMeans(region_data[, 3:8])
    return(col_means)
  })
  
  # Calculate deviations for each customer
  deviations <- lapply(data_by_region, function(region_data) {
    region_mean <- colMeans(region_data[, 3:8])
    customer_deviations <- region_data[, 3:8] - region_mean
    return(customer_deviations)
  })
  
  # Combine deviations for all customers
  all_deviations <- do.call(rbind, deviations)
  
  if (!is.null(outlier_threshold)) {
    z_scores <- scale(all_deviations)
    outliers <- abs(z_scores) > outlier_threshold
    all_deviations[outliers] <- NA
  }
  
  return(all_deviations)
}

# Call the function with an outlier threshold (e.g., 2 for a 2-standard deviation threshold)
outliers_removed_deviations <- calculate_deviation(data, outlier_threshold = 2)
print(outliers_removed_deviations)
