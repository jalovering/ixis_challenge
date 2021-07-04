# IXIS Data Science Challenge
# Josh Lovering
# 7/2/2021 - 7/3/2021

# This program generates plots to represent findings from the
# data generated from aggregate_data.R

##################################
## import libraries and set cwd ##
##################################

library(ggplot2)
theme_set(theme_minimal())
library(scales)
library(readxl) 
library(openxlsx)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#################
## import data ##
#################

list_of_dfs <- lapply(1:2, function(i) read_excel("aggregated_output.xlsx", sheet = i))
month_device <- list_of_dfs[[1]]
month_over_month <- list_of_dfs[[2]]

###########################
## create visualizations ##
###########################

## generates lineplot based on parameters
generate_lineplot <- function(data, x, y, colour, group, title, xlab, ylab, filename) {
  plot <- ggplot(data = data, aes_string(x=x, y=y, colour=colour, group=group))+
    geom_line()+
    geom_point()+
    theme(axis.text.x = element_text(angle = 55, vjust = 0.5,))+
    ggtitle(title)+
    xlab(xlab)+
    ylab(ylab)+
    scale_y_continuous(labels = comma)+
    scale_x_discrete(limits=data$x)
  ggsave(paste("visualizations/", filename, sep=""), width = 8, height = 5)
  return(plot)
}

## generate a lineplot for each metric by month and device
generate_lineplot(month_device,
                  'dim_month_year',
                  'sessions',
                  'dim_deviceCategory',
                  'dim_deviceCategory',
                  "Month vs Number of Sessions by Device",
                  "Month",
                  "Number of Sessions",
                  "month_sessions_device.png")
generate_lineplot(month_device,
                  'dim_month_year',
                  'transactions',
                  'dim_deviceCategory',
                  'dim_deviceCategory',
                  "Month vs Number of Transactions by Device",
                  "Month",
                  "Number of Transactions",
                  "month_transactions_device.png")
generate_lineplot(month_device,
                  'dim_month_year',
                  'QTY',
                  'dim_deviceCategory',
                  'dim_deviceCategory',
                  "Month vs Quantity by Device",
                  "Month",
                  "Quantity",
                  "month_QTY_device.png")
generate_lineplot(month_device,
                  'dim_month_year',
                  'ECR',
                  'dim_deviceCategory',
                  'dim_deviceCategory',
                  "Month vs ECR by Device",
                  "Month",
                  "ECR",
                  "month_ECR_device.png")

## generate lineplot of absolute difference for each metric monthly
generate_lineplot(month_over_month,
                  'timeframe',
                  'sessions_absolute_difference',
                  NULL,
                  1,
                  "Monthly Absolute Difference in Number of Sessions",
                  "Month Range",
                  "Absolute Difference in Sessions",
                  "monthly_sessions_absolute_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'transactions_absolute_difference',
                  NULL,
                  1,
                  "Monthly Absolute Difference in Number of Transactions",
                  "Month Range",
                  "Absolute Difference in Transactions",
                  "monthly_transactions_absolute_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'QTY_absolute_difference',
                  NULL,
                  1,
                  "Monthly Absolute Difference in Quantity",
                  "Month Range",
                  "Absolute Difference in Quantity",
                  "monthly_QTY_absolute_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'ECR_absolute_difference',
                  NULL,
                  1,
                  "Monthly Absolute Difference in ECR",
                  "Month Range",
                  "Absolute Difference in ECR",
                  "monthly_ECR_absolute_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'addsToCart_absolute_difference',
                  NULL,
                  1,
                  "Monthly Absolute Difference in Adds to Cart",
                  "Month Range",
                  "Absolute Difference in Adds to Cart",
                  "monthly_addsToCart_absolute_difference.png")

## generate lineplot of relative difference for each metric monthly
generate_lineplot(month_over_month,
                  'timeframe',
                  'sessions_relative_difference',
                  NULL,
                  1,
                  "Monthly Relative Difference in Number of Sessions",
                  "Month Range",
                  "Relative Difference in Sessions (%)",
                  "monthly_sessions_relative_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'transactions_relative_difference',
                  NULL,
                  1,
                  "Monthly Relative Difference in Number of Transactions",
                  "Month Range",
                  "Relative Difference in Transactions (%)",
                  "monthly_transactions_relative_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'QTY_relative_difference',
                  NULL,
                  1,
                  "Monthly Relative Difference in Quantity",
                  "Month Range",
                  "Relative Difference in Quantity (%)",
                  "monthly_QTY_relative_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'ECR_relative_difference',
                  NULL,
                  1,
                  "Monthly Relative Difference in ECR",
                  "Month Range",
                  "Relative Difference in ECR (%)",
                  "monthly_ECR_relative_difference.png")
generate_lineplot(month_over_month,
                  'timeframe',
                  'addsToCart_relative_difference',
                  NULL,
                  1,
                  "Monthly Relative Difference in Adds to Cart",
                  "Month Range",
                  "Relative Difference in Adds to Cart (%)",
                  "monthly_addsToCart_relative_difference.png")

## Relative change of all metrics on one plot
ggplot(data = month_over_month, aes(x=timeframe, group=1))+
  geom_line(aes(y=sessions_relative_difference, color='Sessions'))+
  geom_line(aes(y=transactions_relative_difference, color='Transactions'))+
  geom_line(aes(y=QTY_relative_difference, color='QTY'))+
  geom_line(aes(y=ECR_relative_difference, color='ECR'))+
  geom_line(aes(y=addsToCart_relative_difference, color='Adds to Cart'))+
  theme(axis.text.x = element_text(angle = 55, vjust = 0.5,))+ 
  ggtitle("Monthly Relative Difference for all Metrics")+
  xlab("Month Range")+
  ylab("Relative Difference (%)")+
  labs(color = "Metric") +
  scale_x_discrete(limits=month_over_month$timeframe)
ggsave("visualizations/monthly_all_relative_difference.png", width = 8, height = 5)

