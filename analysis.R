#library needed 
library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(scales)
library(forecast)
library(corrplot)

#to check file path 
getwd()

#import and read data
hack_data <- read.csv("hackingData.csv")

#able to show the data set
list.files(pattern = ".csv")

#confirmation of data type
str(hack_data)
head(hack_data)

# 1. First data quality test
print("Initial Data Assessment:")
str(hack_data)
summary(hack_data)

# 2. Analysis of missing value
missing_analysis <- data.frame(
  Variable = names(hack_data),
  Missing_Count = colSums(is.na(hack_data)),
  Missing_Percentage = colSums(is.na(hack_data))/nrow(hack_data) * 100
)
print("Missing Value Analysis:")
print(missing_analysis)

# 3. Clean and validate the data
clean_data <- hack_data %>%
  # Remove duplicates
  distinct() %>%
  # Standardize date format
  mutate(
    Date = as.Date(Date, format="%d/%m/%Y"),
    # Standardize text fields with validation
    Country = case_when(
      toupper(trimws(Country)) == "UNKNOWN" ~ NA_character_,
      TRUE ~ toupper(trimws(Country))
    ),
    OS = case_when(
      toupper(trimws(OS)) == "UNKNOWN" ~ NA_character_,
      TRUE ~ toupper(trimws(OS))
    ),
    WebServer = case_when(
      toupper(trimws(WebServer)) == "UNKNOWN" ~ NA_character_,
      TRUE ~ toupper(trimws(WebServer))
    )
  ) %>%
  # Validate numeric fields
  mutate(
    Loss = if_else(Loss < 0 | is.na(Loss), NA_real_, Loss),
    DownTime = if_else(DownTime < 0 | is.na(DownTime), NA_real_, DownTime),
    Ransom = if_else(Ransom < 0 | is.na(Ransom), NA_real_, Ransom)
  )

# 4. Verify the data quality
quality_check <- data.frame(
  Metric = c("Original Rows", "Cleaned Rows", "Rows Removed"),
  Value = c(
    nrow(hack_data),
    nrow(clean_data),
    nrow(hack_data) - nrow(clean_data)
  )
)

# 5. Analysis after double checking
post_cleaning_summary <- data.frame(
  Variable = names(clean_data),
  Complete_Records = colSums(!is.na(clean_data)),
  Completion_Rate = colSums(!is.na(clean_data))/nrow(clean_data) * 100
)

print("Data Quality After Cleaning:")
print(post_cleaning_summary)


# Basic data processing to identify main analysis dimensions
processed_data <- clean_data %>%
  mutate(
    # Temporal aspect transformation
    Date = as.Date(Date),
    
    # Geographic aspect standardization
    Country = toupper(trimws(Country)),
    IP = trimws(IP),
    
    # Technical aspect standardization
    OS = toupper(trimws(OS)),
    WebServer = toupper(trimws(WebServer)),
    Encoding = toupper(trimws(Encoding)),
    
    # Economic aspect validation
    Loss = as.numeric(Loss),
    Ransom = as.numeric(Ransom),
    DownTime = as.numeric(DownTime)
  )


# Analyze potential analytical dimensions
dimension_analysis <- data.frame(
  Dimension = c("Temporal", "Geographic", "Technical", "Economic"),
  Key_Variables = c(
    "Date, DownTime", 
    "Country, IP, Lang", 
    "OS, WebServer, Encoding",
    "Loss, Ransom"
  ),
  Data_Completeness = c(
    sum(!is.na(processed_data$Date))/nrow(processed_data),
    sum(!is.na(processed_data$Country))/nrow(processed_data),
    sum(!is.na(processed_data$OS))/nrow(processed_data),
    sum(!is.na(processed_data$Loss))/nrow(processed_data)
  ) * 100
)

print("Analysis Dimension Overview:")
print(dimension_analysis)

# Examine relationships between dimensions
dimension_correlations <- processed_data %>%
  summarise(
    Temporal_Economic = cor(DownTime, Loss, use = "complete.obs"),
    Technical_Impact = mean(Loss[!is.na(OS)], na.rm = TRUE) / 
      mean(Loss[is.na(OS)], na.rm = TRUE),
    Geographic_Distribution = n_distinct(Country, na.rm = TRUE)
  )


# Print correlation analysis results
print("\nDimension Relationships:")
print(dimension_correlations)



# Install and load required packages
if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
library(zoo)

# Data Pre-processing for Temporal Analysis
processed_data <- clean_data %>%
  # Create comprehensive temporal features
  mutate(
    # Basic temporal components
    Year = year(Date),
    Month = month(Date),
    MonthYear = floor_date(Date, "month"),
    
    # Advanced temporal features
    Season = case_when(
      Month %in% c(12,1,2) ~ "Winter",
      Month %in% c(3,4,5) ~ "Spring",
      Month %in% c(6,7,8) ~ "Summer",
      Month %in% c(9,10,11) ~ "Fall"
    ),
    WeekDay = wday(Date, label = TRUE),
    Quarter = quarter(Date),
    IsWeekend = WeekDay %in% c("Sat", "Sun")
  ) %>%
  
  # Calculate time-based metrics
  group_by(Year, Month) %>%
  mutate(
    Monthly_Attack_Count = n(),
    Monthly_Avg_Loss = mean(Loss, na.rm = TRUE),
    Monthly_Avg_Downtime = mean(DownTime, na.rm = TRUE),
    Monthly_Total_Loss = sum(Loss, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Sort by date before calculating rolling averages
  arrange(Date) %>%
  # Calculate rolling averages
  mutate(
    Rolling_Avg_Attacks = rollmean(Monthly_Attack_Count, k = 3, fill = NA, align = "right"),
    Rolling_Avg_Loss = rollmean(Monthly_Avg_Loss, k = 3, fill = NA, align = "right")
  )

# Verify processing results
processing_summary <- data.frame(
  Feature_Type = c("Basic Temporal", "Advanced Temporal", "Derived Metrics", "Rolling Statistics"),
  Features_Created = c(
    "Year, Month, MonthYear",
    "Season, WeekDay, Quarter, IsWeekend",
    "Monthly_Attack_Count, Monthly_Avg_Loss",
    "Rolling_Avg_Attacks, Rolling_Avg_Loss"
  ),
  Purpose = c(
    "Basic time component analysis",
    "Complex temporal pattern analysis",
    "Aggregated temporal insights",
    "Trend and pattern identification"
  )
)

# Print processing summary
print("Data Pre-processing Summary:")
print(processing_summary)

# Verify feature creation
str(processed_data)


# 1. Basic Data list Overview
dataset_overview <- processed_data %>%
  summarise(
    Total_Records = n(),
    Date_Range = paste(min(Date), "to", max(Date)),
    Total_Years = n_distinct(Year),
    Total_Countries = n_distinct(Country),
    Total_Loss = sum(Loss, na.rm = TRUE)
  )

# 2. Temporal trend analysis
temporal_distribution <- processed_data %>%
  group_by(Year) %>%
  summarise(
    Annual_Attacks = n(),
    Annual_Loss = sum(Loss, na.rm = TRUE),
    Avg_Downtime = mean(DownTime, na.rm = TRUE),
    .groups = 'drop'
  )

# Visualization of  annual distribution
ggplot(temporal_distribution, aes(x = Year, y = Annual_Attacks)) +
  geom_line(color = "blue") +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Annual Distribution of Web Security Incidents",
    x = "Year",
    y = "Number of Attacks"
  )

# 3. Seasonal Pattern Exploration
seasonal_patterns <- processed_data %>%
  group_by(Season) %>%
  summarise(
    Attack_Count = n(),
    Avg_Loss = mean(Loss, na.rm = TRUE),
    Avg_Downtime = mean(DownTime, na.rm = TRUE),
    .groups = 'drop'
  )

# 4. Monthly distribution analysis
monthly_distribution <- processed_data %>%
  group_by(Month) %>%
  summarise(
    Attack_Count = n(),
    Success_Rate = sum(Loss > 0, na.rm = TRUE) / n(),
    Avg_Impact = mean(Loss, na.rm = TRUE),
    .groups = 'drop'
  )

# Fabulous summary of findings
print("Key Findings from Data Exploration:")
print(dataset_overview)
print("Seasonal Patterns:")
print(seasonal_patterns)
print("Monthly Patterns:")
print(monthly_distribution)

# Additional pattern visualization
ggplot(monthly_distribution, aes(x = as.factor(Month), y = Attack_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(
    title = "Monthly Distribution of Attacks",
    x = "Month",
    y = "Number of Attacks"
  )


#change of date
temporal_data <- hack_data
temporal_data$Date <- as.Date(temporal_data$Date, format="%d/%m/%Y")

#add on yearly
temporal_data$Year <- year(temporal_data$Date)

#add on monthly 
temporal_data$Month <- month(temporal_data$Date)

#add on combination of yearly & monthly 
temporal_data$MonthYear <- floor_date(temporal_data$Date, "month")

#clean NA
temporal_data <- temporal_data[!duplicated(temporal_data), ]
temporal_data <- temporal_data[!is.na(temporal_data$Date), ]

#check the changes 
head(temporal_data)

#calculation of yearly hacker attack trend 
yearly_attacks <- temporal_data %>%
  group_by(Year) %>%
  summarise(
    attack_count = n(),
    .groups = 'drop'
  )

#visualize the yearly trend pattern
ggplot(yearly_attacks, aes(x = Year, y = attack_count)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "red", size = 3) +
  theme_minimal() +
  labs(
    title = "Annual Trend of Web Hacking Incidents",
    x = "Year",
    y = "Number of Attacks",
    caption = "Data source: Web Hacking Dataset"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

#calculation of monthly trends of attacking 
monthly_attacks <- temporal_data %>%
  group_by(Year, Month) %>%  # based on Year和Month seperately create boxplot
  summarise(
    attack_count = n(),
    avg_loss = mean(Loss, na.rm = TRUE),
    avg_downtime = mean(DownTime, na.rm = TRUE),
    .groups = 'drop'
  )

#visualize the monthly trend pattern
ggplot(monthly_attacks, aes(x = as.factor(Month), y = attack_count)) +
  geom_boxplot(fill = "lightblue", outlier.color = "darkred") +
  theme_minimal() +
  labs(
    title = "Monthly Distribution of Web Hacking Incidents",
    x = "Month",
    y = "Number of Attacks",
    caption = "Data source: Web Hacking Dataset"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

#visualization of heat map for yearly and monthly hacker attack 
ggplot(monthly_attacks, aes(x = as.factor(Month), y = Year, fill = attack_count)) +
  geom_tile() +
  scale_fill_gradient(low = "pink", high = "red") +
  theme_minimal() +
  labs(
    title = "Heatmap of Web Hacking Incidents",
    x = "Month",
    y = "Year",
    fill = "Number of Attacks"
  )

#creation of season analysis 
seasonal_attacks <- temporal_data %>%
  mutate(
    Season = case_when(
      Month %in% c(12,1,2) ~ "Winter",
      Month %in% c(3,4,5) ~ "Spring",
      Month %in% c(6,7,8) ~ "Summer",
      Month %in% c(9,10,11) ~ "Fall"
    )
  ) %>%
  group_by(Year, Season) %>%
  summarise(
    attack_count = n(),
    .groups = 'drop'
  )

#visualization of season pattern 
ggplot(seasonal_attacks, aes(x = Season, y = attack_count)) +
  geom_boxplot(fill = "lightblue", outlier.color = "darkred") +
  theme_minimal() +
  labs(
    title = "Seasonal Distribution of Web Hacking Incidents",
    x = "Season",
    y = "Number of Attacks",
    caption = "Data source: Web Hacking Dataset"
  )

#the temporal overview
temporal_overview <- hack_data %>%
  mutate(
    Date = as.Date(Date, format="%d/%m/%Y"),
    Year = year(Date)
  ) %>%
  group_by(Year) %>%
  summarise(
    incidents = n(),
    total_loss = sum(Loss, na.rm = TRUE)/1000,  # in thousands
    avg_downtime = mean(DownTime, na.rm = TRUE)
  )

#visualize the overview
ggplot(temporal_overview) +
  geom_area(aes(x = Year, y = incidents), fill = "lightblue", alpha = 0.5) +
  geom_line(aes(x = Year, y = incidents), color = "darkblue", size = 1) +
  theme_minimal() +
  labs(
    title = "Overview of Web Security Incidents (1998-2015)",
    subtitle = "Distribution of incidents across the study period",
    x = "Year",
    y = "Number of Incidents",
    caption = "Data source: Web Security Incident Dataset"
  )


# Create Attack Severity Analysis
severity_analysis <- processed_data %>%
  mutate(
    Year = year(Date),
    Month = month(Date)
  ) %>%
  group_by(Year, Month) %>%
  summarise(
    attack_count = n(),
    avg_loss = mean(Loss, na.rm = TRUE),
    avg_downtime = mean(DownTime, na.rm = TRUE),
    severity_index = (avg_loss * avg_downtime)/1000,  # Combined severity metric
    .groups = 'drop'
  )



# 1. Circular Heatmap of Severity
# Reorganize data for circular visualization
circular_severity <- severity_analysis %>%
  mutate(
    # Normalize severity index for better visualization
    normalized_severity = (severity_index - min(severity_index)) / 
      (max(severity_index) - min(severity_index))
  )

# Create circular heatmap
ggplot(circular_severity, aes(x = as.factor(Month), y = as.factor(Year))) +
  geom_tile(aes(fill = normalized_severity)) +
  scale_fill_gradient2(
    low = "white",
    mid = "pink",
    high = "darkred",
    midpoint = 0.5
  ) +
  coord_polar() +
  theme_minimal() +
  labs(
    title = "Circular View of Attack Severity Patterns",
    x = "Month",
    y = "Year",
    fill = "Severity Level"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

# 2. Elegant Impact Flow
ggplot(severity_analysis, 
       aes(x = Year + (Month-1)/12, y = severity_index)) +
  geom_smooth(color = "#FF69B4", fill = "#FFB6C1", alpha = 0.3) +
  geom_point(aes(size = attack_count, color = avg_downtime),
             alpha = 0.6) +
  scale_color_gradient(low = "#E6E6FA", high = "#4B0082") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.grid.major = element_line(color = "#F0F0F0"),
    panel.grid.minor = element_line(color = "#F8F8F8")
  ) +
  labs(
    title = "Evolution of Attack Impact Over Time",
    x = "Time",
    y = "Severity Index",
    color = "Downtime",
    size = "Attack Count"
  )

#Additional feature 1 : decomposition of time 
hackingData <- read.csv("hackingData.csv")

decomposition_analysis <- function(data) {
  # Ensure Date column is in proper date format
  data$Date <- as.Date(data$Date)
  
  # Aggregate data to monthly counts
  monthly_counts <- data %>%
    group_by(year = year(Date), month = month(Date)) %>%
    summarise(attacks = n(), .groups = 'drop') %>%
    arrange(year, month)
  
  # Create time series object
  ts_data <- ts(monthly_counts$attacks, 
                start = c(min(monthly_counts$year), 1),
                frequency = 12)
  
  # Perform decomposition
  decomp <- decompose(ts_data)
  
  # Create plots
  par(mfrow = c(4,1))
  plot(decomp$x, main = "Original Time Series", ylab = "Number of Attacks")
  plot(decomp$trend, main = "Trend Component", ylab = "Trend")
  plot(decomp$seasonal, main = "Seasonal Component", ylab = "Seasonality")
  plot(decomp$random, main = "Random Component", ylab = "Random")
  
  # Return decomposition object for further analysis
  return(decomp)
}

# Apply the analysis
ts_decomp <- decomposition_analysis(hackingData)


#Additional feature 2 : predictive technique
attack_forecasting <- function(data, forecast_periods = 12) {
  # Prepare monthly data
  monthly_data <- data %>%
    group_by(year = year(Date), month = month(Date)) %>%
    summarise(attacks = n(), .groups = 'drop') %>%
    arrange(year, month)
  
  # Create time series object
  ts_data <- ts(monthly_data$attacks, 
                start = c(min(monthly_data$year), 1),
                frequency = 12)
  
  # Fit ARIMA model
  fit <- auto.arima(ts_data, seasonal = TRUE)
  
  # Generate forecast
  forecast_result <- forecast(fit, h = forecast_periods)
  
  # Plot results
  plot(forecast_result, 
       main = "Forecasted Attack Patterns",
       xlab = "Time",
       ylab = "Number of Attacks")
  
  # Return forecast object
  return(forecast_result)
}

# Apply the forecasting
attack_forecast <- attack_forecasting(hackingData)

# Print forecast summary
print(summary(attack_forecast))

#Additional feature 3 : Correlation Analysis 
temporal_correlation_analysis <- function(data) {
  # Prepare data for correlation analysis
  temporal_data <- data %>%
    mutate(
      month_num = month(Date),
      year_num = year(Date),
      day_of_week = wday(Date),
      is_weekend = ifelse(wday(Date) %in% c(1,7), 1, 0),
      day_of_month = day(Date)
    ) %>%
    group_by(year_num, month_num) %>%
    summarise(
      attack_count = n(),
      avg_loss = mean(Loss, na.rm = TRUE),
      avg_downtime = mean(DownTime, na.rm = TRUE),
      weekend_ratio = mean(is_weekend),
      .groups = 'drop'
    )
  
  # Create correlation matrix
  cor_matrix <- cor(temporal_data[,c("attack_count", "avg_loss", 
                                     "avg_downtime", "month_num", 
                                     "weekend_ratio")],
                    use = "complete.obs")
  
  # Create improved correlation plot
  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           title = "Temporal Correlations in Attack Patterns",
           mar = c(2,2,2,2),
           col = colorRampPalette(c("#6D9EC1", "white", "#E46726"))(200))
  
  # Return correlation matrix for further analysis
  return(cor_matrix)
}

# Apply the correlation analysis
temporal_cors <- temporal_correlation_analysis(hackingData)

# Print correlation results with better formatting
print(round(temporal_cors, 3))