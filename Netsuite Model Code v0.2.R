#################################################

#### Exploratory Analysis for Net suite data ####

#################################################

# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(stringr)
library(data.table)
library(RColorBrewer)
library(caTools)
library(caret)
library(e1071)

# Read Datasets
rm(list = ls())
setwd('C:/Happiest Minds Work/All_Projects/All_Projects/Charge point/04012019')
Netsuit_invoice_data <- read.csv('NAAnalyticsExtractResults29Mar2019.csv', stringsAsFactors = F)

# Data Cleaning, EDA and preparation
str(Netsuit_invoice_data) # 6710 data points with 12 features

# Drop useless features
Netsuit_invoice_data <- Netsuit_invoice_data %>% select(-Created.From, -Memo,-Payment.ID )

# Remove duplicate rows if any
Netsuit_invoice_data <- Netsuit_invoice_data[!duplicated(Netsuit_invoice_data),]

# Now we go column by column
# 1) Invoice ID- Check if all entries are unique
length(unique(Netsuit_invoice_data$Invoice.ID)) # 6520- Indicates duplicates in Invoice ID

Netsuit_invoice_data <- Netsuit_invoice_data %>% group_by(Invoice.ID) %>% 
  mutate(Num_Occurences_Invoice_Id = n())

duplicated_Invoice_IDs <- Netsuit_invoice_data %>% filter(Num_Occurences_Invoice_Id > 1)

# For now drop and move ahead as it's only 353/6710. Later we will come back to 
# these cases

Netsuit_invoice_data <- Netsuit_invoice_data %>% filter(Num_Occurences_Invoice_Id == 1)

# 2) Invoice Date and payment Date- Convert to proper date time objects
Netsuit_invoice_data$Invoice.Date <- parse_date_time(Netsuit_invoice_data$Invoice.Date,
                                                     orders = c('mdY'))
Netsuit_invoice_data$Payment.Date <- parse_date_time(Netsuit_invoice_data$Payment.Date,
                                                     orders = c('mdY'))

# 3) Invoice Amount- Remove "," and convert to int
Netsuit_invoice_data$Invoice.Amount <- str_replace_all(Netsuit_invoice_data$Invoice.Amount, ",", "")
Netsuit_invoice_data$Invoice.Amount <- as.integer(Netsuit_invoice_data$Invoice.Amount)

# 4) Customer Name- Extract Customer ID from Customer Name
Netsuit_invoice_data$Customer_ID <- word(Netsuit_invoice_data$Customer.Name)
Netsuit_invoice_data$Customer.Name <- str_extract(Netsuit_invoice_data$Customer.Name, " .*")

# 5) Invoice Currency
table(Netsuit_invoice_data$Invoice.Currency)

# 6) Extract Payment due days from Terms
table(Netsuit_invoice_data$Terms)
Netsuit_invoice_data$Due.Date.Receive.By <- parse_date_time(Netsuit_invoice_data$Due.Date.Receive.By,
                                                            orders = c('mdY')) 
Netsuit_invoice_data$Payment_due_period <- difftime(Netsuit_invoice_data$Due.Date.Receive.By, Netsuit_invoice_data$Invoice.Date, units = "days")

# Where Payment_due_period is N/A replace with 30 days
Netsuit_invoice_data$Payment_due_period[is.na(Netsuit_invoice_data$Payment_due_period)] = 30

# Still in some cases Payment_due_period is 0 days. We gotta take care of this next
# Extract Payment date period from terms
Netsuit_invoice_data$Terms <- gsub("1% 10","", Netsuit_invoice_data$Terms)
Netsuit_invoice_data$Terms <- gsub("2% 10","", Netsuit_invoice_data$Terms)
table(Netsuit_invoice_data$Terms)
Netsuit_invoice_data$Terms <- str_extract(Netsuit_invoice_data$Terms, "[0-9]+")

Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
Netsuit_invoice_data$Terms <- as.integer(Netsuit_invoice_data$Terms)

Netsuit_invoice_data$Payment_due_period[which(Netsuit_invoice_data$Payment_due_period == 0)] = Netsuit_invoice_data$Terms[which(Netsuit_invoice_data$Payment_due_period == 0)]

# Drop Terms, Num Occurences and, customer Date created
Netsuit_invoice_data <- Netsuit_invoice_data %>% 
  select(-Terms, -Customer.Date.Created, -Num_Occurences_Invoice_Id)

# Create the Target Feature
Netsuit_invoice_data$Payment_Delay <- difftime(Netsuit_invoice_data$Payment.Date, Netsuit_invoice_data$Invoice.Date, units = 'days')
Netsuit_invoice_data$Payment_Delay <- as.integer(Netsuit_invoice_data$Payment_Delay)
summary(Netsuit_invoice_data$Payment_Delay)

# Drop rows where payment Delay is negative
Netsuit_invoice_data <- Netsuit_invoice_data %>% filter(Payment_Delay >= 0)

Netsuit_invoice_data$Payment_Delay_Categorical = " "

for(i in 1:nrow(Netsuit_invoice_data)){
  if(Netsuit_invoice_data$Payment_Delay[i] <= 0){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "On Time"
  } else if(Netsuit_invoice_data$Payment_Delay[i] > 0 & Netsuit_invoice_data$Payment_Delay[i] <= 30){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "1-30 Days delay"
  } else if(Netsuit_invoice_data$Payment_Delay[i] > 30 & Netsuit_invoice_data$Payment_Delay[i] <= 60){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "30-60 Days delay"
  } else if(Netsuit_invoice_data$Payment_Delay[i] > 60 & Netsuit_invoice_data$Payment_Delay[i] <= 90){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "60-90 Days delay"
  } else Netsuit_invoice_data$Payment_Delay_Categorical[i] = "90+ days delay"
}

# There are some cases where Payment Date is before the invoice date itself.
# Does not seem to be possible

# Re arrange the features 
Netsuit_invoice_data <- Netsuit_invoice_data %>% 
  select(Invoice.ID, Customer_ID, Customer.Name, Invoice.Date,Payment.Date,
         Invoice.Amount, Invoice.Currency, Due.Date.Receive.By, Payment_due_period,
         Payment_Delay, Payment_Delay_Categorical)

# Create the Target Feature
Netsuit_invoice_data$Payment_Delay <- difftime(Netsuit_invoice_data$Payment.Date, Netsuit_invoice_data$Invoice.Date, units = 'days')
Netsuit_invoice_data$Payment_Delay <- as.integer(Netsuit_invoice_data$Payment_Delay)
summary(Netsuit_invoice_data$Payment_Delay)

# Drop rows where payment Delay is negative
Netsuit_invoice_data <- Netsuit_invoice_data %>% filter(Payment_Delay >= 0)

# create the Target feature
Netsuit_invoice_data$Payment_Delay_Categorical = " "

for(i in 1:nrow(Netsuit_invoice_data)){
  if(Netsuit_invoice_data$Payment_Delay[i] <= 0){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "On Time"
  } else if(Netsuit_invoice_data$Payment_Delay[i] > 0 & Netsuit_invoice_data$Payment_Delay[i] <= 30){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "1-30 Days delay"
  } else if(Netsuit_invoice_data$Payment_Delay[i] > 30 & Netsuit_invoice_data$Payment_Delay[i] <= 60){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "30-60 Days delay"
  } else if(Netsuit_invoice_data$Payment_Delay[i] > 60 & Netsuit_invoice_data$Payment_Delay[i] <= 90){
    Netsuit_invoice_data$Payment_Delay_Categorical[i] = "60-90 Days delay"
  } else Netsuit_invoice_data$Payment_Delay_Categorical[i] = "90+ days delay"
}

# There are some cases where Payment Date is before the invoice date itself.
# Does not seem to be possible

# Re arrange the features 
Netsuit_invoice_data <- Netsuit_invoice_data %>% 
  select(Invoice.ID, Customer_ID, Customer.Name, Invoice.Date,Payment.Date,
         Invoice.Amount, Invoice.Currency, Due.Date.Receive.By, Payment_due_period,
         Payment_Delay, Payment_Delay_Categorical)

#### Feature Extraction ####
# 1) Age of customer- Oldest - Newest Invoice Date
Netsuit_invoice_data <- Netsuit_invoice_data %>% group_by(Customer_ID) %>% 
  mutate(Oldest_Date = max(Invoice.Date))

Netsuit_invoice_data <- Netsuit_invoice_data %>% group_by(Customer_ID) %>% 
  mutate(Newest_Date = min(Invoice.Date))

Netsuit_invoice_data$Age_of_customer <- difftime(Netsuit_invoice_data$Oldest_Date, Netsuit_invoice_data$Newest_Date,
                                                 units = 'days')

# 2) Month of Invoice generation date- May influence when invoice will be paid back
Netsuit_invoice_data$Invoice_Month <- lubridate::month(Netsuit_invoice_data$Invoice.Date)
Netsuit_invoice_data$Invoice_Month <- as.integer(Netsuit_invoice_data$Invoice_Month)

# 3) Quarter of Year
Netsuit_invoice_data$Invoice_Quarter <- lubridate::quarter(Netsuit_invoice_data$Invoice.Date)
Netsuit_invoice_data$Invoice_Quarter <- as.integer(Netsuit_invoice_data$Invoice_Quarter)

# 4) Month Of Quarter- e.g December is 3rd Month of Quarter 4
Netsuit_invoice_data$Month_of_Quarter <- Netsuit_invoice_data$Invoice_Month
Netsuit_invoice_data$Month_of_Quarter <- ifelse(Netsuit_invoice_data$Invoice_Quarter == 2, 
                                                Netsuit_invoice_data$Month_of_Quarter - 3,
                                                Netsuit_invoice_data$Month_of_Quarter)
Netsuit_invoice_data$Month_of_Quarter <- ifelse(Netsuit_invoice_data$Invoice_Quarter == 3, 
                                                Netsuit_invoice_data$Invoice_Month - 6,
                                                Netsuit_invoice_data$Month_of_Quarter)
Netsuit_invoice_data$Month_of_Quarter <- ifelse(Netsuit_invoice_data$Invoice_Quarter == 4, 
                                                Netsuit_invoice_data$Invoice_Month - 9,
                                                Netsuit_invoice_data$Month_of_Quarter)

# 5) is_late- whether a payment is late or not depends on the payment term of the
# Invoice too. An invoice which was paid at 99 days is not late if the payment due
# term was 120 days. SO we need to create a flag for each invoice being late or not
# There are some NA's in Payment Due period. Let us correct them.
Netsuit_invoice_data$Payment_due_period[is.na(Netsuit_invoice_data$Payment_due_period)] = 
  difftime(Netsuit_invoice_data$Payment.Date[is.na(Netsuit_invoice_data$Payment_due_period)], 
           Netsuit_invoice_data$Invoice.Date[is.na(Netsuit_invoice_data$Payment_due_period)])

Netsuit_invoice_data$is_late <- Netsuit_invoice_data$Payment_Delay - Netsuit_invoice_data$Payment_due_period
Netsuit_invoice_data$is_late <- ifelse(Netsuit_invoice_data$is_late <= 0, "Not Late", "Late")
ggplot(Netsuit_invoice_data, aes(x = is_late, fill = is_late)) + geom_bar() +
  theme_solarized() + scale_fill_tableau()

# 6) Number of total paid invoices up to prior month- Number of paid invoices prior to the
# last month of a customer. In this data set all occurences 
# are of paid invoices only. For this we will need to order all months chronologically.
min_date = min(Netsuit_invoice_data$Invoice.Date) # 2015-02-27
max_date = max(Netsuit_invoice_data$Invoice.Date) # 2018-10-10
# From 2015-02-27 to 2018-10-10 there are 44 months worth of data here. We need to create a column having
# values from 1-44

Netsuit_invoice_data$Month_ordered <- Netsuit_invoice_data$Invoice_Month
Netsuit_invoice_data$Year <- lubridate::year(Netsuit_invoice_data$Invoice.Date)
Netsuit_invoice_data$Year <- as.numeric(Netsuit_invoice_data$Year)

# Currently hard coding base Year as 2015. Will need to be dynamic later
Netsuit_invoice_data$Month_ordered <- ifelse(Netsuit_invoice_data$Year == 2016, 
                                                Netsuit_invoice_data$Month_ordered + 11,
                                                Netsuit_invoice_data$Month_ordered)
Netsuit_invoice_data$Month_ordered <- ifelse(Netsuit_invoice_data$Year == 2017, 
                                             Netsuit_invoice_data$Month_ordered + 23,
                                             Netsuit_invoice_data$Month_ordered)
Netsuit_invoice_data$Month_ordered <- ifelse(Netsuit_invoice_data$Year == 2018, 
                                             Netsuit_invoice_data$Month_ordered + 35,
                                             Netsuit_invoice_data$Month_ordered)
# Since original Month was Feb we need to subtract 1
Netsuit_invoice_data$Month_ordered <- Netsuit_invoice_data$Month_ordered -1

# After Month ordered is created we can create the feature
Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
Netsuit_invoice_data$Num_paid_Invoices_last_month = 0
list_of_customers <- unique(Netsuit_invoice_data$Customer_ID)

customer_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered, Num_paid_Invoices_last_month)
customer_data <- customer_data[1,]

for(customer_id in list_of_customers){
  temp_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered, Num_paid_Invoices_last_month) %>% 
    filter(Customer_ID == customer_id)
  if(length(unique(temp_data$Month_ordered)) == 1){
    customer_data <- rbind(customer_data, temp_data)
  } else {
    
    temp_data <- temp_data %>% group_by(Month_ordered) %>% 
      mutate(Num_paid_Invoices_each_Month = n())
    
    x <- unique(temp_data[,c(3,5)])
    
    x$Num_paid_Invoices_last_month <- cumsum(x$Num_paid_Invoices_each_Month)
    
    x$Num_paid_Invoices_last_month <- shift(x$Num_paid_Invoices_last_month)
    
    x$Num_paid_Invoices_last_month[is.na(x$Num_paid_Invoices_last_month)] = 0
    
    temp_data <- temp_data %>% select(-Num_paid_Invoices_last_month)
    temp_data <- left_join(temp_data, x[,c(1,3)], by = "Month_ordered") 
    
    temp_data <- temp_data %>% select(-Num_paid_Invoices_each_Month)
    temp_data <- as.data.frame(temp_data)
    customer_data <- rbind(customer_data, temp_data)
  }
}
customer_data <- customer_data[-1,]

Netsuit_invoice_data <- Netsuit_invoice_data %>% select(-Num_paid_Invoices_last_month)
Netsuit_invoice_data <- left_join(Netsuit_invoice_data, customer_data[,c("Invoice.ID","Num_paid_Invoices_last_month")])
rm(x, temp_data, customer_data)

# 7) Number of invoices that were paid late- Number of invoices which were paid late
# prior to the creation month of a new invoice of a customer
Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
Netsuit_invoice_data$Num_paid_Invoices_last_month_late = 0
list_of_customers <- unique(Netsuit_invoice_data$Customer_ID)

customer_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered,is_late, Num_paid_Invoices_last_month_late)
customer_data <- customer_data[1,]

for(customer_id in list_of_customers){
  temp_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered,is_late, Num_paid_Invoices_last_month_late) %>% 
    filter(Customer_ID == customer_id)
  if(length(unique(temp_data$Month_ordered)) == 1){
    customer_data <- rbind(customer_data, temp_data)
  } else {
    
    #temp_data <- temp_data %>% filter(is_late == "Late")
    
    temp_data <- temp_data %>% group_by(Month_ordered) %>% 
      mutate(Num_paid_Invoices_each_Month_late = sum(is_late == "Late"))
    
    x <- unique(temp_data[,c(3,6)])
    
    x$Num_paid_Invoices_last_month_late <- cumsum(x$Num_paid_Invoices_each_Month_late)
    
    x$Num_paid_Invoices_last_month_late <- shift(x$Num_paid_Invoices_last_month_late)
    
    x$Num_paid_Invoices_last_month_late[is.na(x$Num_paid_Invoices_last_month_late)] = 0
    
    temp_data <- temp_data %>% select(-Num_paid_Invoices_last_month_late)
    temp_data <- left_join(temp_data, x[,c(1,3)], by = "Month_ordered") 
    #print(temp_data)
    temp_data <- temp_data %>% select(-Num_paid_Invoices_each_Month_late)
    temp_data <- as.data.frame(temp_data)
    customer_data <- rbind(customer_data, temp_data)
  }
}
customer_data <- customer_data[-1,]

Netsuit_invoice_data <- Netsuit_invoice_data %>% select(-Num_paid_Invoices_last_month_late)
Netsuit_invoice_data <- left_join(Netsuit_invoice_data, customer_data[,c("Invoice.ID","Num_paid_Invoices_last_month_late")])
rm(x, temp_data, customer_data)

# 8) Ratio of paid invoices which were late up to the last month
Netsuit_invoice_data$Ratio_late_payments <- Netsuit_invoice_data$Num_paid_Invoices_last_month_late / Netsuit_invoice_data$Num_paid_Invoices_last_month
Netsuit_invoice_data$Ratio_late_payments[is.nan(Netsuit_invoice_data$Ratio_late_payments)] = 0

# 9) Gap between successive Invoices
Netsuit_invoice_data$Gap_successive_invoices <- 0

list_of_customers <- unique(Netsuit_invoice_data$Customer_ID)
customer_data <- Netsuit_invoice_data %>% select(Invoice.ID, Customer_ID, Invoice.Date,Gap_successive_invoices)
customer_data <- customer_data[1,]


for(customer_id in list_of_customers){
  temp_data <- Netsuit_invoice_data %>% select(Invoice.ID, Customer_ID, Invoice.Date,Gap_successive_invoices) %>% 
    filter(Customer_ID == customer_id)
  # print(nrow(temp_data))
  if(nrow(temp_data) == 1){
    customer_data <- rbind(customer_data, temp_data)
  } else{
    for(i in 2:nrow(temp_data)){
      temp_data$Gap_successive_invoices[i] = difftime(temp_data$Invoice.Date[i], temp_data$Invoice.Date[i-1])
      temp_data$Gap_successive_invoices[i] = as.integer(temp_data$Gap_successive_invoices[i])
    }
    customer_data <- rbind(customer_data, temp_data)
  }
}

customer_data <- customer_data[-1,]
# Join back to the main data frame and remove temporary data
Netsuit_invoice_data <- Netsuit_invoice_data %>% select(-Gap_successive_invoices)
Netsuit_invoice_data <- left_join(Netsuit_invoice_data, customer_data[,c("Invoice.ID", "Gap_successive_invoices")])

# 10) Average Days late per customer
Netsuit_invoice_data <- Netsuit_invoice_data %>% group_by(Customer_ID) %>% mutate(Average_Payment_delay = mean(Payment_Delay))
Netsuit_invoice_data$Average_Payment_delay <- round(Netsuit_invoice_data$Average_Payment_delay, 0)

# 11) No. of delayed Payments per customer
Netsuit_invoice_data <- Netsuit_invoice_data %>% group_by(Customer_ID) %>% 
  mutate(Num_payments_delayed = sum(is_late == "Late"))

# # 12) Sum of the base amount of total paid invoices- The sum of the base amount from all the
# # paid invoices prior to a new invoice for a customer up to the last month. 
# # For this we first need to convert all Invoice amounts to same units
# table(Netsuit_invoice_data$Invoice.Currency) # Only USD and CAD
# Netsuit_invoice_data$Invoice.Amount <- ifelse(Netsuit_invoice_data$Invoice.Currency == "Canadian Dollar",
#                                               Netsuit_invoice_data$Invoice.Amount * 0.75,
#                                               Netsuit_invoice_data$Invoice.Amount)
# 
# Netsuit_invoice_data$Amount_pending_last_month <- 0
# 
# Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
# Netsuit_invoice_data$Num_paid_Invoices_last_month_late = 0
# list_of_customers <- unique(Netsuit_invoice_data$Customer_ID)
# 
# customer_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered,Invoice.Amount,Amount_pending_last_month)
# customer_data <- customer_data[1,]
# 
# for(customer_id in list_of_customers){
#   temp_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered,Invoice.Amount,Amount_pending_last_month) %>% 
#     filter(Customer_ID == customer_id)
#   if(length(unique(temp_data$Month_ordered)) == 1){
#     customer_data <- rbind(customer_data, temp_data)
#   } else {
#     
#     temp_data <- temp_data %>% group_by(Month_ordered) %>% 
#       mutate(Amount_pending_each_month = sum(Invoice.Amount))
#     
#     x <- unique(temp_data[,c(3,6)])
#     
#     x$Amount_pending_last_month <- cumsum(x$Amount_pending_each_month)
#     
#     x$Amount_pending_last_month <- shift(x$Amount_pending_last_month)
#     
#     x$Amount_pending_last_month[is.na(x$Amount_pending_last_month)] = 0
#     
#     temp_data <- temp_data %>% select(-Amount_pending_last_month)
#     temp_data <- left_join(temp_data, x[,c(1,3)], by = "Month_ordered") 
#     #print(temp_data)
#     temp_data <- temp_data %>% select(-Amount_pending_each_month)
#     temp_data <- as.data.frame(temp_data)
#     customer_data <- rbind(customer_data, temp_data)
#   }
# }
# customer_data <- customer_data[-1,]
# 
# Netsuit_invoice_data <- Netsuit_invoice_data %>% select(-Amount_pending_last_month)
# Netsuit_invoice_data <- left_join(Netsuit_invoice_data, customer_data[,c("Invoice.ID","Amount_pending_last_month")])
# rm(x, temp_data, customer_data)
# 
# # 12) Sum of the base amount of total paid invoices that were late- The sum of the base amount from all the
# # paid invoices which were late prior to a new invoice for a customer up to last month
# 
# Netsuit_invoice_data$Amount_pending_last_month_late <- 0
# 
# Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
# Netsuit_invoice_data$Num_paid_Invoices_last_month_late = 0
# list_of_customers <- unique(Netsuit_invoice_data$Customer_ID)
# 
# customer_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered,Invoice.Amount,Amount_pending_last_month_late,is_late)
# customer_data <- customer_data[1,]
# 
# for(customer_id in list_of_customers){
#   temp_data <- Netsuit_invoice_data %>% select(Customer_ID, Invoice.ID, Month_ordered,Invoice.Amount,Amount_pending_last_month_late,is_late) %>% 
#     filter(Customer_ID == customer_id)
#   if(length(unique(temp_data$Month_ordered)) == 1){
#     customer_data <- rbind(customer_data, temp_data)
#   } else {
#     
#     temp_data <- temp_data %>% filter(is_late == "Late")
#     
#     temp_data <- temp_data %>% group_by(Month_ordered) %>% 
#       mutate(Amount_pending_each_month_late = (sum(Invoice.Amount)))
#     
#     x <- unique(temp_data[,c(3,7)])
#     
#     x$Amount_pending_last_month_late <- cumsum(x$Amount_pending_each_month_late)
#     
#     x$Amount_pending_last_month_late <- shift(x$Amount_pending_last_month_late)
#     
#     x$Amount_pending_last_month_late[is.na(x$Amount_pending_last_month_late)] = 0
#     
#     temp_data <- temp_data %>% select(-Amount_pending_last_month_late)
#     temp_data <- left_join(temp_data, x[,c(1,3)], by = "Month_ordered") 
#     #print(temp_data)
#     temp_data <- temp_data %>% select(-Amount_pending_each_month_late)
#     temp_data <- as.data.frame(temp_data)
#     customer_data <- rbind(customer_data, temp_data)
#   }
# }
# customer_data <- unique(customer_data[,c("Customer_ID","Month_ordered","Amount_pending_last_month_late")])
# 
# Netsuit_invoice_data <- Netsuit_invoice_data %>% select(-Amount_pending_last_month_late)
# Netsuit_invoice_data <- left_join(Netsuit_invoice_data, customer_data[,c("Customer_ID","Month_ordered","Amount_pending_last_month_late")])
# rm(x, temp_data, customer_data)
# 
# # 13) Ratio of sum of paid amount that were late
# Netsuit_invoice_data$Ratio_of_late_amounts <- Netsuit_invoice_data$Amount_pending_last_month_late/Netsuit_invoice_data$Amount_pending_last_month
# Netsuit_invoice_data$Ratio_of_late_amounts[is.nan(Netsuit_invoice_data$Ratio_of_late_amounts)] = 0

# Invoice Month and Quarter
# No. of invoices raised per Month. First map Months to Month Names
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
# Add abbreviated month name
Netsuit_invoice_data$Invoice_Month <- mymonths[ Netsuit_invoice_data$Invoice_Month ]

# Quarter
Netsuit_invoice_data$Invoice_Quarter <- paste("Qtr", Netsuit_invoice_data$Invoice_Quarter, "_")

# Invoice month and Quarter to factors
Netsuit_invoice_data$Invoice_Month <- as.factor(Netsuit_invoice_data$Invoice_Month)
Netsuit_invoice_data$Invoice_Quarter <- as.factor(Netsuit_invoice_data$Invoice_Quarter)

# The target var to facctor
Netsuit_invoice_data$Payment_Delay_Categorical <- as.factor(Netsuit_invoice_data$Payment_Delay_Categorical)

str(Netsuit_invoice_data)

# Visualizations and further EDA #
# Create a function which outputs two plots. Count of the target variable categories and 
# percentage of the target variable in each category
Plotter_Categorical <- function(data, source_var, target_var){
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar() +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  
  p2 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) + geom_bar(position = "fill") +
    scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
    labs(x = source_var, y = target_var) + theme(legend.title = element_blank())
  x11() 
  grid.arrange(p1, p2)
  
}

# 2) For numeric variables
Plotter_Numeric <- function(data, source_var, target_var){
  
  p1 <- ggplot(data, aes(x = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
    theme_gdocs() + scale_fill_tableau(name = target_var) + geom_density(alpha = 0.3) +
    labs(x = source_var, y = "density") 
  
  p2 <- ggplot(data, aes(x = data[,c(target_var)], y = data[,c(source_var)], fill = data[,c(target_var)])) +
    geom_boxplot() + theme_gdocs() + scale_fill_tableau(name = target_var) + 
    labs(x = target_var, y = source_var)
  
  x11() 
  grid.arrange(p1, p2)
  
}

rm(customer_data, temp_data)

Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)

# 1) Distribution of Target variable
G1 = ggplot(Netsuit_invoice_data, aes(x = Netsuit_invoice_data[,c("Payment_Delay")], fill = "blue")) +
  geom_histogram(aes(y = ..density..),position = "dodge", col = "black", bins = 30) +
  geom_density(alpha = 0.3) +
  labs(x = "Payment_Delay", y = "density") +
  scale_x_continuous(breaks = seq(0, 1200, 50)) +
  theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5), legend.position="none") +
  labs(x = 'Payment Delay', title = 'Histogram + Density Plot of Payment Delays') 

Netsuit_invoice_data$Payment_Delay_Categorical <- as.character(Netsuit_invoice_data$Payment_Delay_Categorical)
Netsuit_invoice_data$Payment_Delay_Categorical <- ifelse(Netsuit_invoice_data$Payment_Delay_Categorical == "On Time",
                                                         "0 Days delay On time",
                                                         Netsuit_invoice_data$Payment_Delay_Categorical)

G2 = ggplot(Netsuit_invoice_data, aes(x = Payment_Delay_Categorical, fill = Payment_Delay_Categorical)) + 
  geom_bar() +
  scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank())

x11()
grid.arrange(G1, G2)
G2

# We can see that most payments are delayed by 31-30 or 60-90 days. However delay does
# not mean late as payment due dates might be within the range. So let's drill down
# and see actually how many payments are late. For this we created a flag is_late
G3 = ggplot(Netsuit_invoice_data, aes(x = is_late, fill = is_late)) + geom_bar() +
  scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  labs(x = 'Payment Counts', title = 'Count of late vs on Time Payments')


G4 = ggplot(Netsuit_invoice_data, aes(x = Payment_Delay_Categorical, fill = Payment_Delay_Categorical)) + 
  geom_bar() +
  scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.text.x = element_blank()) +
  facet_wrap(~is_late) + labs(x = "Payment Delay Categories", title = 'Payment Delay Category counts')

x11()
grid.arrange(G3, G4)
G4

payment_amounts_grouped <- Netsuit_invoice_data %>%
  group_by(Payment_Delay_Categorical, is_late) %>% summarise(sum(Invoice.Amount))

payment_amounts_grouped = data.frame(payment_amounts_grouped)

colnames(payment_amounts_grouped)[3] = 'Sum_Invoice_Amount'

payment_amounts_grouped$Payment_Delay_Categorical = as.factor(payment_amounts_grouped$Payment_Delay_Categorical)

levels(payment_amounts_grouped$Payment_Delay_Categorical) = c('0 Days','1-30 Days',
                                                              '30-60 Days', '60-90 Days',
                                                              '90+ Days')
payment_amounts_grouped$Sum_Invoice_Amount_in_millions = payment_amounts_grouped$Sum_Invoice_Amount / 1000000
payment_amounts_grouped$Sum_Invoice_Amount_in_millions = round(payment_amounts_grouped$Sum_Invoice_Amount_in_millions, 2)

Gn = ggplot(payment_amounts_grouped, aes(x = Payment_Delay_Categorical,y=Sum_Invoice_Amount_in_millions, fill = Payment_Delay_Categorical)) + 
  geom_col() +
  scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
  geom_text(aes(label = Sum_Invoice_Amount_in_millions), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~is_late) +
  labs(x = "Payment Delay Categories", title = 'Payment Delay Category counts',
       y = 'Sum of INvoice Amount (in mn $)')

Gn
# So the No. of Payments delayed is actually more than double the no of Payments
# within time. This is a matter of concern.

# Let's explore the Top 10 customers 
# with most no. of delayed payments along with their average delays
Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
Top_10_customers_with_late_payments <- Netsuit_invoice_data %>% 
  select(Customer_ID, Customer.Name, Num_payments_delayed, Average_Payment_delay) %>% unique() %>%
  arrange(desc(Num_payments_delayed)) %>% 
  slice(1:10)

Top_customers_amount = Netsuit_invoice_data %>% group_by(Customer.Name) %>% 
  summarize(Total_Amount = sum(Invoice.Amount))

G5 = ggplot(Top_10_customers_with_late_payments, aes(x = Customer.Name, y = Num_payments_delayed, fill = Customer.Name)) + geom_col() +
  scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.x = element_blank()) + 
  geom_text(mapping = aes(label = Num_payments_delayed), vjust = -0.1, position = "nudge") +
  labs(title = 'Number of Payments Delayed Top 10', y = 'Number of Payments Delayed')

G6 = ggplot(Top_10_customers_with_late_payments, aes(x = Customer.Name, y = Average_Payment_delay, fill = Customer.Name)) + geom_col() +
  scale_fill_tableau() + theme_solarized() + theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.x = element_blank()) + 
  geom_text(mapping = aes(label = Average_Payment_delay), vjust = -0.1, position = "nudge") +
  labs(title = 'Average Payment Delay Top 10', y = 'Average Payment Delay')

x11()
grid.arrange(G5, G6)

# Verdek LLC., San Diego gas & Electric & ABM Consolidated have the most number
# of delayed payments. Also San Diego gas & Electric has the most average delay

# Let's explore if the Month and Quarter when an invoice are raised have any 
# influence  on their payment delay

# Lets explore the Top 10 customers who have the most amounts 
# Change company names to lowercase and strip
Netsuit_invoice_data$Customer.Name <- str_trim(Netsuit_invoice_data$Customer.Name)
Netsuit_invoice_data$Customer.Name <- tolower(Netsuit_invoice_data$Customer.Name)

Top_100_customers_Average_delay <- Netsuit_invoice_data %>% 
  group_by(Customer.Name) %>% 
  summarize(Total_Amount_Outstanding = sum(Invoice.Amount), 
            Mean_Delay = mean(Payment_Delay),
            Mean_payment_due_period = mean(Payment_due_period))

Top_100_customers_Average_delay$Annualized_Outstanding <- 
  (Top_100_customers_Average_delay$Total_Amount_Outstanding * Top_100_customers_Average_delay$Mean_Delay)/365

Top_100_customers_Average_delay <- Top_100_customers_Average_delay %>% 
  arrange(desc(Annualized_Outstanding)) %>% slice(1:125)

Top_100_customers_Average_delay$Annualized_Outstanding_in_millions <- Top_100_customers_Average_delay$Annualized_Outstanding / 1000000

Top_100_customers_Average_delay <- Top_100_customers_Average_delay %>% 
  select(Customer.Name, Total_Amount_Outstanding,Mean_payment_due_period, Mean_Delay, Annualized_Outstanding, Annualized_Outstanding_in_millions)

# Write the data
write.csv(Top_100_customers_Average_delay, 'Output Data.csv', row.names = F)

# No. of invoices raised per Month. First map Months to Month Names
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")
# Add abbreviated month name
Netsuit_invoice_data$Invoice_Month <- mymonths[ Netsuit_invoice_data$Invoice_Month ]

colourCount = length(unique(Netsuit_invoice_data$Invoice_Month))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

# Month wise Invoice Counts
G7 = ggplot(Netsuit_invoice_data, aes(x = Invoice_Month, fill = Invoice_Month)) + geom_bar() +
  scale_fill_manual(values = getPalette(colourCount)) + theme_economist() + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank()) 
G7

# Month Wise late or On time Invoices
G8 = ggplot(Netsuit_invoice_data, aes(x = is_late, fill = is_late)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = getPalette(colourCount)) + theme_solarized() + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank()) + facet_wrap(~Invoice_Month)
G8

x11()
grid.arrange(G7, G8)

# October and November months are particulary bad as most Invoices raised during these months
# get paid late
# Let's check Invoices raised each Quarter
# Quarter wise Invoice Counts
Netsuit_invoice_data$Invoice_Quarter <- paste("Qtr", Netsuit_invoice_data$Invoice_Quarter, sep = "_")
colourCount = length(unique(Netsuit_invoice_data$Invoice_Quarter))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

G9 = ggplot(Netsuit_invoice_data, aes(x = Invoice_Quarter, fill = Invoice_Quarter)) + geom_bar() +
  scale_fill_manual(values = getPalette(colourCount)) + theme_economist() + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank()) 
G9

# Month Wise late or On time Invoices
G10 = ggplot(Netsuit_invoice_data, aes(x = is_late, fill = is_late)) + 
  geom_bar(position = "dodge") +
  scale_fill_manual(values = getPalette(colourCount)) + theme_solarized() + 
  theme(axis.text.x = element_text(angle = 90)) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.1, position = "nudge") +
  theme(legend.title = element_blank()) + facet_wrap(~Invoice_Quarter)
G10

x11()
grid.arrange(G9, G10)

# Average Payment delay vs Target
ggplot(Netsuit_invoice_data, aes(x = Payment_Delay_Categorical, y = log(Average_Payment_delay),
                 fill = Payment_Delay_Categorical)) +  geom_boxplot() +   theme_gdocs() + 
  scale_fill_tableau(name = 'Average Payment Delay') + 
  labs(x = 'Payment Delay Categories', y = 'Average Payment Delay')

# Ratio Late Payment delay vs Target
ggplot(Netsuit_invoice_data, aes(x = Payment_Delay_Categorical, y = Ratio_late_payments, fill = Payment_Delay_Categorical)) +  
  geom_boxplot() +   theme_gdocs() + 
  scale_fill_tableau(name = 'Ratio Late Payments') + 
  labs(x = 'Payment Delay Categories', y = 'Ratio Late Payments') +
  theme(axis.text.x = element_blank())

# Age of Customer delay vs Target
Netsuit_invoice_data$Age_of_customer <- as.numeric(Netsuit_invoice_data$Age_of_customer)
ggplot(Netsuit_invoice_data, aes(x = Payment_Delay_Categorical, y = Age_of_customer, fill = Payment_Delay_Categorical)) +  
  geom_boxplot() +   theme_gdocs() + 
  scale_fill_tableau(name = 'Payment Delay Categories') + 
  labs(x = 'Payment Delay Categories', y = 'Age of Customer') +
  theme(axis.text.x = element_blank())

# Clearly Quarter 4 is the worst for business with more than 90% Invoices
# getting delayed

# Model Building
# Drop unnecessary variables
Netsuit_invoice_data <- Netsuit_invoice_data %>%
  select(-Invoice.Date, -Payment.Date, -Invoice.Currency, -Due.Date.Receive.By, -is_late, -Payment_Delay, 
         -Oldest_Date, -Newest_Date, -Month_ordered, -Year)

# Check once if there are any NA values
sapply(Netsuit_invoice_data, function(x) sum(is.na(x)))
# Customer Name contains one NA however it will not be used for modelling in any case

# Convert variables to proper types
Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
str(Netsuit_invoice_data)

# Payment Due period and Age of customer to int
Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
Netsuit_invoice_data$Payment_due_period <- as.integer(Netsuit_invoice_data$Payment_due_period)
Netsuit_invoice_data$Age_of_customer <- as.integer(Netsuit_invoice_data$Age_of_customer)


# Split the train data into train and test
set.seed(123)
indices <- sample.split(Netsuit_invoice_data$Payment_Delay_Categorical, SplitRatio = 0.75)
train <- Netsuit_invoice_data[indices,]
test <- Netsuit_invoice_data[!indices,]

# Create a Local h20 cluster
h2o.shutdown()
h2o.init(nthreads = -1, min_mem_size = "3g")

# Transfer data to the cluster
train.h2o <- as.h2o(train)
test.h2o <- as.h2o(test)

# Run a GBMmodel with default params
colnames(train.h2o)
y.dep = 6
x.indep = c(4:5,7:16)

gbm.model <- h2o.gbm(y = y.dep, x = x.indep, training_frame = train.h2o,
                     nfolds = 3, ntrees = 1000, learn_rate = 0.1, seed = 123)
summary(gbm.model)
h2o.varimp(gbm.model)

# Check metrics on test data
predictions_gbm <- as.data.frame(h2o.predict(gbm.model, test.h2o))
test_data <- data.frame(Actuals = test$Payment_Delay_Categorical, Predicted = predictions_gbm$predict)

# Check Confusion Matrix and other metrices
confusion_matrix = confusionMatrix(data = test_data$Predicted, reference = test_data$Actuals)

# Accuracy
confusion_matrix$overall[1] # ~86%

# Confusion Matrix Table
confusion_matrix$table

# Explore the most important variables against the Target
# 1) Average payment Delay
Netsuit_invoice_data <- as.data.frame(Netsuit_invoice_data)
str(Netsuit_invoice_data)
Netsuit_invoice_data$Payment_Delay_Categorical <- ifelse(Netsuit_invoice_data$Payment_Delay_Categorical)
Plotter_Numeric(Netsuit_invoice_data, "Average_Payment_delay", "Payment_Delay_Categorical")

Netsuit_invoice_data$Age_of_customer <- as.numeric(Netsuit_invoice_data$Age_of_customer)
ggplot(Netsuit_invoice_data, aes(x = Payment_Delay_Categorical, y = Invoice_Quarter, fill = Payment_Delay_Categorical)) +
  geom_boxplot() + theme_gdocs() + scale_fill_tableau() + labs(y = "Gap_successive_invoices") +
  theme(axis.text.x = element_text(angle = 90))


Netsuit_Amounts <- Netsuit_invoice_data %>% group_by(Year) %>% summarise(Income_per_year = sum(Invoice.Amount))
Netsuit_Amounts <- as.data.frame(Netsuit_Amounts)

ggplot(Netsuit_Amounts, aes(x = Year, y = Income_per_year)) + geom_line()
