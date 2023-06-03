library(rvest)
library(dplyr)
URL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQA9Atb2iCAbExo6bGyneP0yYIwaHn3xtaPSOATzziQqELAbBy0hp-bWjC2Y2HWbxAbHsZMr7NmF14s0S-pcEQ/pubhtml#"
rainfall_data <- read_html(URL)
rainfall_data
all_months <- rainfall_data%>%html_table(fill = TRUE)
head(all_months)

#june_data
june_data <- data.frame(all_months[[6]])
june_data

#June data cleaning 

#(removing two rows) 
june_data <- june_data[-1,]
june_data
june_data <- june_data[-1,]
june_data

#removing columns
june_data <- june_data[,-1]
june_data
june_data <- june_data[,-1]
june_data
june_data <- june_data[,-1]
june_data

#june_total
june_total <- june_data$Var.34
june_total

#converting total to integers
june_total_numeric <- as.numeric(june_total)
june_total_numeric


#July data cleaning

july_data <- data.frame(all_months[[7]])
july_data

#removing rows
july_data <- july_data[-1,]
july_data
july_data <- july_data[-1,]
july_data

#removing columns

july_data <- july_data[,-1]
july_data
july_data <- july_data[,-1]
july_data
july_data <- july_data[,-1]
july_data

july_total <- july_data$Var.35
july_total

july_total_numeric <- as.numeric(july_total)
july_total_numeric

#removing extra value from june data

june_total_numeric <- june_total_numeric[-151]
june_total_numeric

#finding change
change <- july_total_numeric-june_total_numeric
change

#converting into matrix
change_matrix <- matrix(unlist(change),ncol=15,nrow = 10)
change_matrix


#max_rainfall

#function to clean data

DataCleaning <- function(month) 
{
  month<-(month[-1,])
  month<-(month[-1,])
  month<-(month[,-1])
  month<-(month[,-1])
  month<-(month[,-1])
}

#function_to_find_total_column
find_total_column_of_31_days <- function(month)
{
  monthly_total <- (month)$var.34
  monthly_total
}

#function to find the max of total 
get_max_of_total <- function(month,month_name)
{
  
  total_values <- c(month)
  df<-total_values
  df[is.na(df)]<-0
  df
  #print(month_name)
  max(df)
}

#function to find the max rainfall

max_rainfall_among_months <- function()
{
  print("MAXIMUM RAINFALL")
  print(max(January_max,Feb_max))
  c('January','February','March','April','May','June','July',
    'August','September','October')[which.max(c(January_max,Feb_max,march_max,
                                                april_max,may_max,june_max,july_max,
                                                aug_max,sep_max,oct_max))]
  
}

#data_cleaning_of_all_ten_months and finding max of each total 

#jan_data

jan_data <- data.frame(all_months[[1]])
jan_data
jan_cleaned_data <- DataCleaning(jan_data)
jan_cleaned_data
jan_total <- jan_cleaned_data$Var.35
jan_total
jan_total_numeric <- as.numeric(jan_total)
jan_total_numeric
January_max <-get_max_of_total(jan_total_numeric,"January")
January_max

#feb_data

feb_data <- data.frame(all_months[[2]])
feb_data
feb_cleaned_data <- DataCleaning(feb_data)
feb_cleaned_data
feb_total <- feb_cleaned_data$Var.33
feb_total
feb_total_numeric <- as.numeric(feb_total)
feb_total_numeric
Feb_max <- get_max_of_total(feb_total_numeric,"February")
Feb_max

#march_data

march_data <- data.frame(all_months[[3]])
march_data
march_cleaned_data <- DataCleaning(march_data)
march_cleaned_data
march_total <- march_cleaned_data$Var.35
march_total
march_total_numeric <- as.numeric(march_total)
march_total_numeric
march_max <-get_max_of_total(march_total_numeric)
march_max

#april_data

april_data <- data.frame(all_months[[4]])
april_data
april_cleaned_data <- DataCleaning(april_data)
april_cleaned_data
april_total <- april_cleaned_data$Var.34
april_total
april_total_numeric <- as.numeric(april_total)
april_total_numeric
april_max <- get_max_of_total(april_total_numeric)
april_max 

#may_data

may_data <- data.frame(all_months[[5]])
may_data
may_cleaned_data <- DataCleaning(may_data)
may_cleaned_data
may_total <- may_cleaned_data$Var.35
may_total
may_total_numeric <- as.numeric(may_total)
may_total_numeric
may_max <- get_max_of_total(may_total_numeric)
may_max

#june_data

june_max <-get_max_of_total(june_total_numeric)
june_max

#july_data

july_max <-get_max_of_total(july_total_numeric)
july_max

#aug_data

aug_data <- data.frame(all_months[[8]])
aug_data
aug_cleaned_data <- DataCleaning(aug_data)
aug_cleaned_data
aug_total <- aug_cleaned_data$Var.34
aug_total
aug_total_numeric <- as.numeric(aug_total)
aug_total_numeric
aug_max <- get_max_of_total(aug_total_numeric)
aug_max

#sep_data

sep_data <- data.frame(all_months[[9]])
sep_data
sep_cleaned_data <- DataCleaning(sep_data)
sep_cleaned_data
sep_total <- sep_cleaned_data$Var.34
sep_total
sep_total_numeric <- as.numeric(sep_total)
sep_total_numeric
sep_max <- get_max_of_total(sep_total_numeric)
sep_max

#oct_data

oct_data <- data.frame(all_months[[10]])
oct_data
oct_cleaned_data <- DataCleaning(oct_data)
oct_cleaned_data
oct_total <- oct_cleaned_data$Var.35
oct_total
oct_total_numeric <- as.numeric(oct_total)
oct_total_numeric
oct_max <- get_max_of_total(oct_total_numeric)
oct_max

max_rainfall_among_months()


#bar plots
library(ggplot2)
barplot(june_total_numeric,xlab = "Rainfall in days",ylab = "Rainfall in months")

barplot(july_total_numeric,xlab = "Rainfall in days",ylab = "Rainfall in months")


#line graph

june <- c(june_total_numeric)
july <- c(july_total_numeric)

plot(july, type = "o", col = "green",
     xlab = "Daily basis difference", ylab = "Monthly basis difference",
     main = "June July Rainfall Difference")

lines(june,type = "o",col="red")


