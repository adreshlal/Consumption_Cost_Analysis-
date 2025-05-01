# Import required libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)
library(corrplot)

electric_df = read.csv("E:/Education/Post-Grad Program/LIU/Classes/Semester 1/Intro to Data Science & Programming Languages (R & Phython)/Final Project/Data Set/Electric_Consumption_And_Cost__2010_-_Feb_2023__20241215.csv", header = TRUE)

# Drop unnecessary columns
columns_to_drop = c("RC.Code", "AMP..", "TDS..", "EDP", "UMIS.BILL.ID", 
                     "Meter.Number","Meter.AMR", "Meter.Scope", "Bill.Analyzed")

electric_df = electric_df[, !colnames(electric_df) %in% columns_to_drop]

electric_df = na.omit(electric_df)     # Removing nulls

electric_df = electric_df[!duplicated(electric_df),]    #Removing Duplicates

# Rename columns before summarizing
electric_df = electric_df %>%
  rename(
    Current_Charges_Electricity = `Current.Charges`,
    Consumption_KWH = `Consumption..KWH.`, 
    KWH_charges = `KWH.Charges`,
    Consumption_KW = `Consumption..KW.`,
    KW_charges = `KW.Charges`,
    Other_charges = `Other.charges`,
    Development = `Development.Name`,
    Days = `X..days`,
    Vendor_Name = `Vendor.Name`,
    Account_Name = `Account.Name`
  )


# View final column names
colnames(electric_df)

# Columns & Rows
dim(electric_df)

# Unique Boroughs, vendors, Locations, Accounts, Developments
unique(electric_df$Borough)
unique(electric_df$Vendor_Name)
unique(electric_df$Account_Name)
unique(electric_df$Development)
unique(electric_df$Location)
unique(electric_df$Revenue.Month)


# Consumption by Boroughs & Year
borough_consumption_charges = electric_df %>% 
  group_by(Borough, Revenue.Month) %>% 
  summarise(
    total_consumption = sum(`Consumption_KWH`),
    total_kwh_charges = sum(`KWH_charges`)
  )

# Bar Plot for Borough-wise Consumption and Charges
ggplot(borough_consumption_charges, aes(x = Borough)) +
  geom_bar(aes(y = total_consumption, fill = "Total Consumption"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = total_kwh_charges, fill = "Total KWH Charges"), stat = "identity", position = "dodge") +
  labs(title = "Borough-Wise Total Consumption and KWH Charges", 
       x = "Borough", 
       y = "Total Amount (KWH / Charges)") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Total Consumption" = "blue", "Total KWH Charges" = "yellow")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Scatter plot for Current Charges vs Consumption
ggplot(electric_df, aes(x = Consumption_KWH, y = Current_Charges_Electricity)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Current Charges vs Consumption", 
       x = "Consumption (KWH)", 
       y = "Current Charges ($)") +
  theme_minimal()


# Statistics for Key columns
summary(electric_df[c("Current_Charges_Electricity", "KWH_charges", "Consumption_KWH", "Consumption_KW", "KW_charges", "Other_charges")])


# Group by Borough and use summary() on key columns
borough_summary = electric_df %>% 
  group_by(Borough, Location, Revenue.Month) %>%
  summarise(
    Min_consumption = min(Consumption_KWH),
    Max_consumption = max(Consumption_KWH),
    Mean_consumption = mean(Consumption_KWH),
    Median_consumption = median(Consumption_KWH)
  )

borough_summary


# standard deviation of consumption by borough & location
stddev_by_year <- electric_df %>%
  group_by(Borough, Location, Revenue.Month) %>%
  summarize(
    Std_Dev_Price = sd(Consumption_KWH)
  )

stddev_by_year


# Group by Borough and Location to calculate statistics
each_borough_consumption = electric_df %>% 
  group_by(Borough, Location) %>% 
  summarise(
    mean_consumption = mean(`Consumption_KWH`),
    median_consumption = median(`Consumption_KWH`),
    mode_consumption = as.numeric(names(sort(table(`Consumption_KWH`), decreasing = TRUE)[1])),
    sd_consumption = sd(`Consumption_KWH`)
  )


# Ensuring Revenue.Month is in proper Date format (if it's not already a date)
electric_df$Revenue.Month = as.Date(paste0(electric_df$Revenue.Month, "-01"), format = "%Y-%m-%d")

electric_df$Year = format(electric_df$Revenue.Month, "%Y")   # Extracting Year from Revenue.Month

# Group by Year and Borough for total consumption
yearly_consumption = electric_df %>%
  group_by(Year, Borough) %>%
  summarise(Total_Consumption = sum(Consumption_KWH))

# Bar Chart
ggplot(yearly_consumption, aes(x = Year, y = Total_Consumption, fill = Borough)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Yearly Consumption by each Borough", 
       x = "Year", 
       y = "Total Consumption (KWH)") +
  scale_fill_viridis_d() +
  theme_minimal()

# Numeric columns
numeric_data = electric_df[, c("Current_Charges_Electricity", "KWH_charges", "Consumption_KWH", "Consumption_KW", "KW_charges", "Other_charges")]
cor_matrix = cor(numeric_data)
cor_matrix

# Correlation Heatmap
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)


# Total revenue for all vendors in each borough
vendor_revenue = electric_df %>%
  group_by(Borough, Vendor_Name) %>%
  summarise(Total_Revenue = sum(Current_Charges_Electricity))

print(vendor_revenue)

# Grouped Bar Plot
ggplot(vendor_revenue, aes(x = Borough, y = Total_Revenue, fill = Vendor_Name)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(title = "Revenue of All Vendors in Each Borough", 
       x = "Borough", 
       y = "Total Revenue ($)") +
  scale_fill_viridis_d() +
  theme_minimal() +
  geom_text(aes(label = scales::comma(Total_Revenue)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, 
            size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Group by Year and Borough to calculate total consumption
yearly_consumption_boroughs = electric_df %>%
  group_by(Year, Borough) %>%
  summarise(Total_Consumption = sum(Consumption_KWH))


# 3D Plot for Borough, Year, and Total Consumption
plot_ly(yearly_consumption_boroughs, 
        x = ~as.numeric(Year), 
        y = ~Borough, 
        z = ~Total_Consumption, 
        type = 'scatter3d', 
        mode = 'lines+markers',
        color = ~Borough) %>%
  layout(title = "3D Plot: Borough Consumption by Year",
         scene = list(
           xaxis = list(title = 'Year'),
           yaxis = list(title = 'Borough'),
           zaxis = list(title = 'Total Consumption (KWH)')
         ))

# Group by Borough and Location to calculate total consumption for each location
top_locations = electric_df %>%
  group_by(Borough, Location) %>%
  summarise(Total_Consumption = sum(Consumption_KWH)) %>%
  arrange(desc(Total_Consumption)) %>%
  group_by(Borough) %>%
  slice_max(Total_Consumption, n = 3)  # Select top 3 locations for each borough

top_locations

# Line Plot for Top 3 Locations for Each Borough
ggplot(top_locations, aes(x = Borough, y = Total_Consumption, group = Location, color = Location)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(title = "Top 3 Locations with Highest Consumption for Each Borough", 
       x = "Borough", 
       y = "Total Consumption (KWH)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_viridis_d() +
  geom_text(aes(label = scales::comma(Total_Consumption)), vjust = -0.5, size = 3)


# Calculate total consumption by borough
borough_consumption = electric_df %>%
  group_by(Borough) %>%
  summarise(Total_Consumption = sum(Consumption_KWH, na.rm = TRUE)) %>%
  mutate(Percentage = round((Total_Consumption / sum(Total_Consumption)) * 100, 1))


# Pie chart
ggplot(borough_consumption, aes(x = "", y = Total_Consumption, fill = Borough)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Total Consumption by Borough", fill = "Borough") +
  theme_minimal() +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4)


# Ensure Revenue.Month is in proper Date format
electric_df$Revenue.Month <- as.Date(paste0(electric_df$Revenue.Month, "-01"), format = "%Y-%m-%d")

# Extract Year from Revenue.Month
electric_df$Year = format(electric_df$Revenue.Month, "%Y")

# Group by Year and calculate total consumption for each year
yearly_consumption = electric_df %>% 
  group_by(Year) %>% 
  summarise(total_consumption = sum(Consumption_KWH))

# Line Plot for Yearly Consumption
ggplot(yearly_consumption, aes(x = as.numeric(Year), y = total_consumption)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Yearly Electricity Consumption Trend", 
       x = "Year", 
       y = "Total Consumption (KWH)") +
  theme_minimal() +
  scale_x_continuous(breaks = as.numeric(unique(yearly_consumption$Year))) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Group by Borough and calculate total KWH and KW Consumption
borough_consumption = electric_df %>%
  group_by(Borough) %>%
  summarise(
    Total_KWH_Consumption = sum(Consumption_KWH),
    Total_KW_Consumption = sum(Consumption_KW)
  )

# Reshape data from wide to long format for ggplot
long_format = borough_consumption %>%
  pivot_longer(cols = c(Total_KWH_Consumption, Total_KW_Consumption), 
               names_to = "Consumption_Type", 
               values_to = "Total_Consumption")

# Plot the line plot
ggplot(long_format, aes(x = Borough, y = Total_Consumption, color = Consumption_Type, group = Consumption_Type)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "KWH and KW Consumption by Borough", 
       x = "Borough", 
       y = "Total Consumption (KWH & KW)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("Total_KWH_Consumption" = "blue", "Total_KW_Consumption" = "red")) +
  geom_text(aes(label = scales::comma(Total_Consumption)), vjust = -0.5, size = 3)




#### Multiple Linear Regression

# Split the data into training and testing (70%/30%)
set.seed(123) # For reproducibility
trainIndex = createDataPartition(electric_df$Consumption_KWH, p = 0.7, list = FALSE)
trainData = electric_df[trainIndex, ]
testData = electric_df[-trainIndex, ]

# Fit the linear regression model using caret
model = train(Consumption_KWH ~ Current_Charges_Electricity + KWH_charges + KW_charges + Days, 
               data = trainData, 
               method = "lm")

# View model summary
summary(model$finalModel)

# Predict on test data
predictions = predict(model, testData)

# Calculate MAE and RMSE
actual = testData$Consumption_KWH
MAE = mean(abs(predictions - actual))
RMSE = sqrt(mean((predictions - actual)^2))

cat("MAE:", MAE, "\nRMSE:", RMSE, "\n")   # Print MAE and RMSE

results <- data.frame(Actual = actual, Predicted = predictions)   # Create a dataframe of actual and predicted values

# Scatter plot of Actual vs Predicted
ggplot(data = results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Actual vs Predicted Consumption", x = "Actual Consumption (KWH)", y = "Predicted Consumption (KWH)") +
  theme_minimal()

