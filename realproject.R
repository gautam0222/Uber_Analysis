# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(moments)
library(caret)
library(car)
library(lmtest)
library(ggthemes)

# Load the dataset
uber_data <- read.csv("uber-raw-data-apr14.csv")

# Convert Date/Time to datetime format
uber_data$Date.Time <- as.POSIXct(uber_data$Date.Time, format="%m-%d-%Y %H:%M")

# Extract time-based features
uber_data$Hour <- hour(uber_data$Date.Time)
uber_data$Day <- day(uber_data$Date.Time)
uber_data$Weekday <- weekdays(uber_data$Date.Time)
uber_data$Month <- month(uber_data$Date.Time)

# Skewness and Kurtosis
lat_skew <- skewness(uber_data$Lat)
lon_skew <- skewness(uber_data$Lon)
lat_kurt <- kurtosis(uber_data$Lat)
lon_kurt <- kurtosis(uber_data$Lon)

# Print skewness and kurtosis
print(paste("Latitude Skewness:", lat_skew))
print(paste("Longitude Skewness:", lon_skew))
print(paste("Latitude Kurtosis:", lat_kurt))
print(paste("Longitude Kurtosis:", lon_kurt))

# Visualize rides per hour
ggplot(uber_data, aes(x=Hour)) +
  geom_histogram(binwidth=1, fill="blue", color="black") +
  labs(title="Rides Per Hour", x="Hour", y="Frequency")

# Save the combined dataset for future months
write.csv(uber_data, "processed_uber_apr.csv", row.names=FALSE)





#Comibned
apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")
data_2014<- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)
# Convert Date.Time to proper datetime format
data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")

# Ensure Time is extracted correctly
data_2014$Time <- format(data_2014$Date.Time, format = "%H:%M:%S")

# Use lubridate functions for extracting date and time components
data_2014$day <- factor(day(data_2014$Date.Time))           # Extract day
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))  # Extract month as factor with labels
data_2014$year <- factor(year(data_2014$Date.Time))         # Extract year
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE)) # Extract weekday as labeled factor

# Extract time-based components
data_2014$hour <- factor(hour(data_2014$Date.Time))         # Extract hour
data_2014$minute <- factor(minute(data_2014$Date.Time))     # Extract minute
data_2014$second <- factor(second(data_2014$Date.Time))     # Extract second







# Continue with your existing code...

# 1. Analyze Trips by Day of Week
ggplot(data_2014, aes(x = dayofweek)) +
  geom_bar(fill = "skyblue") +
  theme_minimal() +
  labs(title = "Trip Distribution by Day of Week",
       x = "Day of Week",
       y = "Number of Trips") +
  theme(axis.text.x = element_text(angle = 45))

# 2. Monthly Trend Analysis
monthly_trips <- data_2014 %>%
  group_by(month) %>%
  summarise(trip_count = n())

ggplot(monthly_trips, aes(x = month, y = trip_count)) +
  geom_bar(stat = "identity", fill = "coral") +
  theme_minimal() +
  labs(title = "Monthly Trip Distribution",
       x = "Month",
       y = "Number of Trips")

# 3. Hourly Demand Heatmap by Day of Week
hourly_dow <- data_2014 %>%
  group_by(dayofweek, hour) %>%
  summarise(trips = n())

ggplot(hourly_dow, aes(x = hour, y = dayofweek, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Hourly Trip Distribution by Day of Week",
       x = "Hour of Day",
       y = "Day of Week")

# 4. Geographic Distribution
ggplot(data_2014, aes(x = Lon, y = Lat)) +
  geom_point(alpha = 0.1, color = "blue") +
  theme_minimal() +
  labs(title = "Geographic Distribution of Uber Trips",
       x = "Longitude",
       y = "Latitude")

# 5. Base Statistics and Summaries
trip_summary <- data_2014 %>%
  group_by(month) %>%
  summarise(
    total_trips = n(),
    avg_trips_per_day = n() / length(unique(day)),
    busiest_hour = names(which.max(table(hour))),
    lat_range = max(Lat) - min(Lat),
    lon_range = max(Lon) - min(Lon)
  )

print(trip_summary)

# 6. Rush Hour Analysis
rush_hours <- data_2014 %>%
  filter(hour %in% c(7,8,9,16,17,18,19)) %>%
  group_by(hour) %>%
  summarise(trip_count = n())

ggplot(rush_hours, aes(x = hour, y = trip_count)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme_minimal() +
  labs(title = "Rush Hour Trip Distribution",
       x = "Hour of Day",
       y = "Number of Trips")

# 7. Density Analysis for Pickup Locations
ggplot(data_2014, aes(x = Lon, y = Lat)) +
  stat_density_2d_filled() +
  theme_minimal() +
  labs(title = "Density Map of Pickup Locations",
       x = "Longitude",
       y = "Latitude")

# 8. Time Series Decomposition
# First, create daily aggregates
daily_trips <- data_2014 %>%
  group_by(Date = as.Date(Date.Time)) %>%
  summarise(trips = n())

# Convert to time series object
trips_ts <- ts(daily_trips$trips, frequency = 7)

# Decompose the time series
decomp <- decompose(trips_ts)

# Plot decomposition
plot(decomp)

# 9. Calculate Additional Statistics
location_stats <- data_2014 %>%
  summarise(
    mean_lat = mean(Lat),
    mean_lon = mean(Lon),
    sd_lat = sd(Lat),
    sd_lon = sd(Lon),
    lat_q1 = quantile(Lat, 0.25),
    lat_q3 = quantile(Lat, 0.75),
    lon_q1 = quantile(Lon, 0.25),
    lon_q3 = quantile(Lon, 0.75)
  )

print(location_stats)

# 10. Save processed results
# Create a summary file with key insights
sink("uber_analysis_summary.txt")
cat("Uber Data Analysis Summary\n\n")
cat("Total Trips:", nrow(data_2014), "\n")
cat("Date Range:", min(data_2014$Date.Time), "to", max(data_2014$Date.Time), "\n")
cat("\nLocation Statistics:\n")
print(location_stats)
cat("\nMonthly Summary:\n")
print(trip_summary)
sink()

# Export visualizations
ggsave("day_of_week_distribution.png", width = 10, height = 6)
ggsave("monthly_distribution.png", width = 10, height = 6)
ggsave("hourly_heatmap.png", width = 12, height = 8)
ggsave("geographic_distribution.png", width = 10, height = 8)



# First, let's clean the data by removing NA values and ensuring proper day conversion
data_2014_clean <- data_2014 %>%
  # Remove rows with NA in dayofweek
  filter(!is.na(dayofweek)) %>%
  # Ensure proper ordering of days
  mutate(dayofweek = factor(dayofweek, 
                            levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")))

# Create improved visualization
ggplot(data_2014_clean, aes(x = dayofweek)) +
  geom_bar(fill = "#2196F3", width = 0.7) +
  theme_minimal() +
  labs(title = "Trip Distribution by Day of Week",
       x = "Day of Week",
       y = "Number of Trips",
       caption = "Data source: Uber Rides 2014") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma) # Format large numbers with commas

# Optional: Print summary to check distribution
day_summary <- data_2014_clean %>%
  group_by(dayofweek) %>%
  summarise(count = n()) %>%
  arrange(dayofweek)

print(day_summary)






# Load required libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# First, let's look at the raw data structure
glimpse_data <- function(data) {
  cat("First few Date.Time values:\n")
  print(head(data$Date.Time))
  cat("\nUnique date formats found:\n")
  print(unique(substr(data$Date.Time, 1, 10)))
}

# Load and combine all data files with error handling
load_uber_data <- function(file_path) {
  tryCatch({
    data <- read.csv(file_path, stringsAsFactors = FALSE)
    # Print sample of raw dates from each file
    cat("Loading:", file_path, "\n")
    print(head(data$Date.Time))
    return(data)
  }, error = function(e) {
    cat("Error loading", file_path, ":", e$message, "\n")
    return(NULL)
  })
}

# Load all data files
apr_data <- load_uber_data("uber-raw-data-apr14.csv")
may_data <- load_uber_data("uber-raw-data-may14.csv")
jun_data <- load_uber_data("uber-raw-data-jun14.csv")
jul_data <- load_uber_data("uber-raw-data-jul14.csv")
aug_data <- load_uber_data("uber-raw-data-aug14.csv")
sep_data <- load_uber_data("uber-raw-data-sep14.csv")

# Function to standardize date format
standardize_dates <- function(data, month) {
  if (is.null(data)) return(NULL)
  
  # Add a column to track the source month for debugging
  data$source_month <- month
  
  # Convert Date.Time to character if it isn't already
  data$Date.Time <- as.character(data$Date.Time)
  
  # Try multiple date formats
  dates <- tryCatch({
    # First try the standard format
    as.POSIXct(data$Date.Time, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")
  }, error = function(e) {
    tryCatch({
      # Try alternative format
      as.POSIXct(data$Date.Time, format = "%m-%d-%Y %H:%M:%S", tz = "UTC")
    }, error = function(e) {
      tryCatch({
        # Try another alternative format
        as.POSIXct(data$Date.Time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      }, error = function(e) {
        NULL
      })
    })
  })
  
  if (is.null(dates)) {
    warning(paste("Could not parse dates for", month))
    return(NULL)
  }
  
  data$Date.Time <- dates
  return(data)
}

# Process each month's data
apr_clean <- standardize_dates(apr_data, "April")
may_clean <- standardize_dates(may_data, "May")
jun_clean <- standardize_dates(jun_data, "June")
jul_clean <- standardize_dates(jul_data, "July")
aug_clean <- standardize_dates(aug_data, "August")
sep_clean <- standardize_dates(sep_data, "September")

# Combine all data
data_2014 <- bind_rows(
  apr_clean, may_clean, jun_clean,
  jul_clean, aug_clean, sep_clean
)

# Clean and process the combined data
data_2014_processed <- data_2014 %>%
  # Remove any rows with NA dates
  filter(!is.na(Date.Time)) %>%
  # Extract all time components
  mutate(
    date = as.Date(Date.Time),
    hour = hour(Date.Time),
    minute = minute(Date.Time),
    second = second(Date.Time),
    dayofweek = wday(Date.Time, label = TRUE, abbr = TRUE),
    month = month(Date.Time, label = TRUE, abbr = TRUE),
    day = day(Date.Time),
    week = week(Date.Time)
  ) %>%
  # Remove any remaining NA values in key fields
  filter(!is.na(dayofweek))

# Print summary statistics to verify the cleaning
summary_stats <- data_2014_processed %>%
  summarise(
    total_rows = n(),
    date_range_start = min(Date.Time),
    date_range_end = max(Date.Time),
    missing_dates = sum(is.na(Date.Time)),
    unique_days = n_distinct(date)
  )

print("Data Summary:")
print(summary_stats)

# Create the improved visualization
ggplot(data_2014_processed, aes(x = dayofweek)) +
  geom_bar(fill = "#2196F3", width = 0.7) +
  theme_minimal() +
  labs(
    title = "Uber Trip Distribution by Day of Week",
    subtitle = paste("Data range:", format(min(data_2014_processed$date), "%B %d, %Y"),
                     "to", format(max(data_2014_processed$date), "%B %d, %Y")),
    x = "Day of Week",
    y = "Number of Trips"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 0, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(labels = scales::comma)

# Print day-of-week distribution
dow_distribution <- data_2014_processed %>%
  group_by(dayofweek) %>%
  summarise(
    count = n(),
    percent = n() / nrow(data_2014_processed) * 100
  ) %>%
  arrange(dayofweek)

print("\nDay of Week Distribution:")
print(dow_distribution)











# Load additional required libraries
library(caret)
library(car)
library(lmtest)

# 1. Prepare data for regression
# Create hour-wise aggregated data for rides
hourly_rides <- data_2014 %>%
  group_by(Date.Time = as.Date(Date.Time), hour) %>%
  summarise(
    ride_count = n(),
    avg_lat = mean(Lat),
    avg_lon = mean(Lon)
  ) %>%
  ungroup()

# Add weather data (assuming temperature affects ride demand)
# This is a simplified example - you might want to join actual weather data
hourly_rides$is_weekend <- ifelse(weekdays(hourly_rides$Date.Time) %in% c("Saturday", "Sunday"), 1, 0)
hourly_rides$hour_num <- as.numeric(as.character(hourly_rides$hour))

# 2. Simple Linear Regression
# Predict ride count based on hour of day
simple_model <- lm(ride_count ~ hour_num, data = hourly_rides)
summary_simple <- summary(simple_model)

# 3. Multiple Linear Regression
# Predict ride count based on multiple features
multiple_model <- lm(ride_count ~ hour_num + is_weekend + avg_lat + avg_lon, data = hourly_rides)
summary_multiple <- summary(multiple_model)

# 4. Perform t-test comparing weekday vs weekend rides
weekend_test <- t.test(
  ride_count ~ is_weekend, 
  data = hourly_rides,
  alternative = "two.sided"
)

# 5. Model Evaluation
# Function to calculate RMSE
calc_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Create training and testing sets
set.seed(123)
train_index <- createDataPartition(hourly_rides$ride_count, p = 0.8, list = FALSE)
train_data <- hourly_rides[train_index, ]
test_data <- hourly_rides[-train_index, ]

# Train models on training data
simple_model_train <- lm(ride_count ~ hour_num, data = train_data)
multiple_model_train <- lm(ride_count ~ hour_num + is_weekend + avg_lat + avg_lon, data = train_data)

# Make predictions on test data
simple_pred <- predict(simple_model_train, test_data)
multiple_pred <- predict(multiple_model_train, test_data)

# Calculate RMSE for both models
simple_rmse <- calc_rmse(test_data$ride_count, simple_pred)
multiple_rmse <- calc_rmse(test_data$ride_count, multiple_pred)

# Calculate R-squared for test data
simple_rsq <- cor(test_data$ride_count, simple_pred)^2
multiple_rsq <- cor(test_data$ride_count, multiple_pred)^2

# 6. Print comprehensive results
cat("\n=== Simple Linear Regression Results ===\n")
print(summary_simple)
cat("\nRMSE:", simple_rmse)
cat("\nTest R-squared:", simple_rsq)

cat("\n\n=== Multiple Linear Regression Results ===\n")
print(summary_multiple)
cat("\nRMSE:", multiple_rmse)
cat("\nTest R-squared:", multiple_rsq)

cat("\n\n=== T-Test Results (Weekday vs Weekend Rides) ===\n")
print(weekend_test)

# 7. Create comparison plot
actual_vs_predicted <- data.frame(
  Actual = test_data$ride_count,
  Simple = simple_pred,
  Multiple = multiple_pred
)

# Plot actual vs predicted values
ggplot(actual_vs_predicted, aes(x = Actual)) +
  geom_point(aes(y = Simple, color = "Simple"), alpha = 0.5) +
  geom_point(aes(y = Multiple, color = "Multiple"), alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(title = "Actual vs Predicted Ride Counts",
       x = "Actual Rides",
       y = "Predicted Rides",
       color = "Model Type") +
  theme_minimal()

# Save results
results_summary <- data.frame(
  Model = c("Simple Regression", "Multiple Regression"),
  RMSE = c(simple_rmse, multiple_rmse),
  R_squared = c(simple_rsq, multiple_rsq)
)
write.csv(results_summary, "regression_results.csv", row.names = FALSE)









cat("\n=== SIMPLE LINEAR REGRESSION SUMMARY ===\n")
cat("\nPredicting ride count based on hour of day:\n")
print(summary(simple_model))

cat("\n\n=== MULTIPLE LINEAR REGRESSION SUMMARY ===\n")
cat("\nPredicting ride count based on hour, weekend status, and location:\n")
print(summary(multiple_model))

# 2. Create a formatted comparison table
comparison_table <- data.frame(
  Metric = c("RMSE", "R-squared", "Adjusted R-squared"),
  Simple_Regression = c(
    round(simple_rmse, 3),
    round(simple_rsq, 3),
    round(summary(simple_model)$adj.r.squared, 3)
  ),
  Multiple_Regression = c(
    round(multiple_rmse, 3),
    round(multiple_rsq, 3),
    round(summary(multiple_model)$adj.r.squared, 3)
  )
)

# Print comparison table
cat("\n=== MODEL COMPARISON ===\n")
print(comparison_table)

# 3. Display T-test results
cat("\n=== WEEKDAY VS WEEKEND T-TEST RESULTS ===\n")
print(weekend_test)

# 4. Create summary statistics by day of week
weekly_summary <- data_2014 %>%
  group_by(dayofweek) %>%
  summarise(
    Total_Rides = n(),
    Avg_Rides_Per_Day = n()/n_distinct(Date.Time),
    Std_Dev = sd(as.numeric(hour))
  ) %>%
  arrange(desc(Total_Rides))

cat("\n=== RIDES BY DAY OF WEEK ===\n")
print(weekly_summary)

# 5. Create hourly pattern summary
hourly_summary <- data_2014 %>%
  group_by(hour) %>%
  summarise(
    Total_Rides = n(),
    Avg_Rides_Per_Hour = n()/n_distinct(Date.Time)
  ) %>%
  arrange(desc(Total_Rides))

cat("\n=== TOP 5 BUSIEST HOURS ===\n")
print(head(hourly_summary, 5))

# 6. Model Performance Metrics
cat("\n=== DETAILED MODEL PERFORMANCE METRICS ===\n")
cat("\nSimple Regression:")
cat("\n- RMSE:", round(simple_rmse, 3))
cat("\n- R-squared:", round(simple_rsq, 3))
cat("\n- F-statistic p-value:", summary(simple_model)$fstatistic[1])

cat("\n\nMultiple Regression:")
cat("\n- RMSE:", round(multiple_rmse, 3))
cat("\n- R-squared:", round(multiple_rsq, 3))
cat("\n- F-statistic p-value:", summary(multiple_model)$fstatistic[1])

# For a more detailed look at the models:
cat("\n\nDetailed Simple Regression Summary:\n")
print(summary(simple_model))

cat("\n\nDetailed Multiple Regression Summary:\n")
print(summary(multiple_model))

# 7. Create visualizations for key findings
# Rides by hour plot
hourly_plot <- ggplot(hourly_summary, aes(x = as.numeric(hour), y = Total_Rides)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Ride Distribution by Hour",
       x = "Hour of Day",
       y = "Total Number of Rides") +
  theme_minimal()

# Weekday vs Weekend comparison plot
weekly_plot <- ggplot(weekly_summary, aes(x = dayofweek, y = Total_Rides)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Rides by Day of Week",
       x = "Day of Week",
       y = "Total Number of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display plots
print(hourly_plot)
print(weekly_plot)

# 8. Save comprehensive results to a file
sink("uber_analysis_results.txt")
cat("=== UBER RIDE ANALYSIS COMPREHENSIVE RESULTS ===\n\n")
cat("1. REGRESSION MODELS COMPARISON\n")
print(comparison_table)
cat("\n2. WEEKLY PATTERNS\n")
print(weekly_summary)
cat("\n3. HOURLY PATTERNS\n")
print(hourly_summary)
cat("\n4. STATISTICAL TEST RESULTS\n")
print(weekend_test)
sink()








# Calculate R-squared manually
simple_rsq <- 1 - sum((test_data$ride_count - simple_pred)^2) / 
  sum((test_data$ride_count - mean(test_data$ride_count))^2)

multiple_rsq <- 1 - sum((test_data$ride_count - multiple_pred)^2) / 
  sum((test_data$ride_count - mean(test_data$ride_count))^2)

# Print results
cat("\nManually calculated R-squared values:")
cat("\nSimple Regression R-squared:", round(simple_rsq, 3))
cat("\nMultiple Regression R-squared:", round(multiple_rsq, 3))