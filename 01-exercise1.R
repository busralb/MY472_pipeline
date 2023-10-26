data <- read.csv("Wholesale_customers_data.csv")

calculate_deviation <- function(data) {
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
  
  return(all_deviations)
}
deviations <- calculate_deviation(data)
print(deviations)