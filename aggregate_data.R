# IXIS Data Science Challenge
# Josh Lovering
# 7/2/2021 - 7/3/2021

# This program creates the .xlsx file containing two sheets:
# 1) month * device aggregation
# 2) month over month comparison

##################################
## import libraries and set cwd ##
##################################

library(dplyr)
library(openxlsx)
library(zoo)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################
## import data ##
#################

sessionCounts <- read.csv(file = 'data/DataAnalyst_Ecom_data_sessionCounts.csv')
addsToCart <- read.csv(file = 'data/DataAnalyst_Ecom_data_addsToCart.csv')
## convert date to datetime
sessionCounts$dim_date <- strptime(sessionCounts$dim_date, format='%m/%d/%y')

################################
## month * device aggregation ##
################################

month_device <- sessionCounts %>%
  mutate(dim_year = format(dim_date, "%Y"), dim_month = as.integer(format(dim_date, "%m"))) %>% # add year and month columns
  subset(select = -c(dim_date, dim_browser)) %>% # drop date and browser columns
  group_by(dim_year, dim_month, dim_deviceCategory) %>% # group by month, year, and device
  summarise(across(everything(), sum)) %>% # sum across non-grouped columns
  ungroup() %>%
  mutate(ECR = transactions/sessions) # add ECR column

#################################
## month over month comparison ##
#################################

month_over_month <- month_device %>%
  subset(select = -c(dim_deviceCategory, ECR)) %>%
  group_by(dim_year, dim_month) %>%
  summarise(across(everything(), sum)) %>%
  ## add ECR column
  mutate(ECR = transactions/sessions) 
## merge addsToCart column
month_over_month <- merge(month_over_month, addsToCart, by=c('dim_year', 'dim_month'))

################
## clean data ##
################

## merge year and month columns from both dfs
month_over_month$dim_month_year <- as.yearmon(paste(month_over_month$dim_month, month_over_month$dim_year), "%m %Y")
month_device$dim_month_year <- as.yearmon(paste(month_device$dim_month, month_device$dim_year), "%m %Y")
## drop old year and month columns from both dfs
month_over_month <- subset(month_over_month, select = -c(dim_year, dim_month))
month_device <- subset(month_device, select = -c(dim_year, dim_month))
## reorder dim_month_year to front from both dfs
month_over_month <- month_over_month[,c(6,1,2,3,4,5)]
month_device <- month_device[,c(6,1,2,3,4,5)]
## sort chronologically
month_over_month <- arrange(month_over_month, dim_month_year)
month_device <- arrange(month_device, dim_month_year)

#######################################
## month over month comparison cont. ##
#######################################

## create two copies of the data one month apart
df_a <- data.frame(month_over_month)[-nrow(month_over_month),]
df_b <- data.frame(month_over_month)[-c(1), ]
row.names(df_a) <- NULL
row.names(df_b) <- NULL
## merge into one df where each row contains adjacent month's data
df <- merge(df_a, df_b, by='row.names', all=TRUE, suffix = c("_prior", "_current"))[,-c(1)]
## sort chronologically
df <- arrange(df, dim_month_year_prior)
## merge the two months into a timeframe column
df$timeframe <- paste(df$dim_month_year_prior, '-', df$dim_month_year_current)
## generate index column for future sorting
df$index <- 1:nrow(df)
## drop prior and current month columns
df <- df[,-c(1,7)]
## reorder index and timeframe to first two columns
df <- df[,c(12,11,1,2,3,4,5,6,7,8,9,10)]
## functionalize calculation of absolute difference
calculate_absolute_diff <- function(prior, current) {
  absolute_diff <- abs(current - prior)
  return(absolute_diff)
}
## functionalize calculation of relative difference
calculate_relative_diff <- function(prior, current) {
  relative_diff <- abs((current - prior)/prior)*100
  return(relative_diff)
}
## calculation of absolute differences
df$sessions_absolute_difference <- calculate_absolute_diff(df$sessions_prior, df$sessions_current)
df$transactions_absolute_difference <- calculate_absolute_diff(df$transactions_prior, df$transactions_current)
df$QTY_absolute_difference <- calculate_absolute_diff(df$QTY_prior, df$QTY_current)
df$ECR_absolute_difference <- calculate_absolute_diff(df$ECR_prior, df$ECR_current)
df$addsToCart_absolute_difference <- calculate_absolute_diff(df$addsToCart_prior, df$addsToCart_current)
## calculation of relative differences
df$sessions_relative_difference <- calculate_relative_diff(df$sessions_prior, df$sessions_current)
df$transactions_relative_difference <- calculate_relative_diff(df$transactions_prior, df$transactions_current)
df$QTY_relative_difference <- calculate_relative_diff(df$QTY_prior, df$QTY_current)
df$ECR_relative_difference <- calculate_relative_diff(df$ECR_prior, df$ECR_current)
df$addsToCart_relative_difference <- calculate_relative_diff(df$addsToCart_prior, df$addsToCart_current)
## rename df
month_over_month <- data.frame(df)

#################
## export data ##
#################

list_of_datasets <- list("month x device" = month_device, "month over month" = month_over_month)
write.xlsx(list_of_datasets, file = "aggregated_output.xlsx", overwrite=TRUE)
