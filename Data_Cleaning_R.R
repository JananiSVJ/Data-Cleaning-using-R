library(tidyverse)
#set the working directory
path_loc <- "C:/Users/saich/Desktop/Janani/TAMUC/Projects/Data Cleaning using R"
setwd(path_loc)
#reading the data
df <- read.csv("telecom.csv")
df %>%
  filter(Churn=="yes") %>%    #displays data with Churn as yes
  select(customerID,TotalCharges) #displays only 2 columns with churn as yes

# Handle missing values in "Monthly Charger" column
df$MonthlyCharges
is.na(df$MonthlyCharges)   #to know Na values
is.nan(df$MonthlyCharges)  #to know NaN values (undefined)

# To get distinct values
df %>%
  distinct(MonthlyCharges)  # gives only 9 values because Na is repeated twice
df %>% 
  summarise(n=n_distinct(MonthlyCharges))  # count of total unique values
df %>%
  summarise(n=sum(is.na(MonthlyCharges)))

#counting unique, missing and median values at the same time
df %>% summarise(n=n_distinct(MonthlyCharges),
                 na=sum(is.na(MonthlyCharges)),
                 med=median(MonthlyCharges, na.rm=TRUE))

# Mutate the missing values with median value
df <- df %>%
  mutate(MonthlyCharges= replace(MonthlyCharges, is.na(MonthlyCharges), median(MonthlyCharges,na.rm=TRUE)))

#Handle Non-Standard missing values in "Total Charges" Column

df$TotalCharges
is.na(df$TotalCharges)
df %>%
  summarise(n=sum(is.na(TotalCharges)))  #Identifies only 1 with "NA" value because R can identify only "na" value

# Now we replace other unidentified missing values (N/A, na) to NA values

df <- df %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges=="na", NA)) %>%
  mutate(TotalCharges = replace(TotalCharges, TotalCharges=="N/A", NA))
df$TotalCharges <- as.numeric(df$TotalCharges)
df <- df %>%
  mutate(TotalCharges = replace(TotalCharges, is.na(TotalCharges), median(TotalCharges, na.rm=TRUE)))


#Handle missing values in "Payment Method" Column

df$PaymentMethod
is.na(df$PaymentMethod)
df <- df %>%
  mutate(PaymentMethod = replace(PaymentMethod, PaymentMethod=="--", NA)) %>%
  mutate(PaymentMethod = replace(PaymentMethod, PaymentMethod=='', NA)) %>%
df




























