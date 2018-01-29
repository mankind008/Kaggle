# R Code for Data Wrangling and Univariate time series including ARIMA, Exponential Smoothing and NN Auto regressive

library(dplyr)
library(lubridate)
library(fpp2)
library(forecast)
library(tidyr)
library(imputeTS)
library(tibble)


# Loading Data

air_store_info <- read.csv("C:/Users/Admin/Desktop/Kaggle/All/air_store_info.csv", stringsAsFactors = FALSE, header = TRUE)

air_visit_data <- read.csv("C:/Users/Admin/Desktop/Kaggle/All/air_visit_data.csv", stringsAsFactors = FALSE, header = TRUE)

date_info <- read.csv("C:/Users/Admin/Desktop/Kaggle/All/date_info.csv", stringsAsFactors = FALSE, header = TRUE)

sample_submission <- read.csv("C:/Users/Admin/Desktop/Kaggle/All/sample_submission.csv", stringsAsFactors = FALSE, header = TRUE)


# Data filter for holidat flags
df <- filter(date_info, holiday_flg == 1)

# Checking visits on a holiday
holiday_visit <- df %>%
  inner_join(air_visit_data, by = c("calendar_date" = "visit_date" ))

# Group visits by date
air_visit_data_grouped_by_date <- summarise(group_by(air_visit_data, visit_date), no_visits = sum(visitors))

# Aggregated visits for all stores
ts_agg_visit <- ts(air_visit_data_grouped_by_date[,2], start = c(1,1), frequency = 7)
autoplot(ts_agg_visit)

# check STL on aggregated data

fit <- stlf(ts_agg_visit)
fc <- forecast(fit)
plot(fc)

# Checking ARIMA with fourier terms

bestfit <- list(aicc=Inf)
for(K in seq(25))
{
  fit <- auto.arima(ts_agg_visit, xreg=fourier(ts_agg_visit, K=K), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
  {
    bestfit <- fit
    bestK <- K
  }
}
fc <- forecast(bestfit, xreg=fourier(ts_agg_visit, K=bestK, h=104))
autoplot(fc)


#testing model on training data itself

autoplot(ts_agg_visit, series = "Data") + 
        autolayer(fitted(fit), series = "fitted")


# checking sum of visitors for reservation and actual visit
sum(air_reserve$reserve_visitors)
sum(hpg_reserve$reserve_visitors)
sum(air_visit_data$visitors)

# No of stores by genre
table(air_store_info$air_genre_name)
table(hpg_store_info$hpg_genre_name)

# no of visits group by genre 
group_by_genre <- summarise(group_by(air_visit_data$genre))
distinct(air_visit_data, air_store_id)

# visit data grouped by store id
store_wise_visits <- summarise(group_by(air_visit_data,air_store_id), no_visits = sum(visitors))
hist(as.vector(store_wise_visits$no_visits))


# Spliting store_id and date from sample submissions file

sample_submission_split1 <- separate(sample_submission, id , c("air_store_id1", "date1"),remove =FALSE, extra = "merge")
sample_submission_split2 <- separate(sample_submission_split1, date1 , c("air_store_id2", "date2"),remove =FALSE, extra = "merge")
sample_submission_split <- unite(sample_submission_split2, "air_store_id" , c("air_store_id1", "air_store_id2") )
sample_submission_mod <- select(sample_submission_split, id, air_store_id, "visit_date" =  date2, visitors )

# no of distinct stores in submission file
no_of_stores_sub <- distinct(sample_submission_mod, air_store_id)

# checking time series for single store
store_data <- filter(air_visit_data, air_store_id == "air_fea5dc9594450608")
store_visits <- summarise(group_by(store_data, visit_date), no_visits = sum(visitors))

# time series for store
#all_829_ts_data <- data.frame(matrix(1, nrow = 478, ncol = 829))
#all_829_ts_data <- mutate(all_829_ts_data, row_names = air_visit_data_grouped_by_date$visit_date )

# no of stores open on a particular day
stores_open <- summarise(group_by(air_visit_data, visit_date), no_stores_open = n())
plot( 1:nrow(stores_open), stores_open$no_stores_open, "l")
plot( 100:150, stores_open$no_stores_open[100:150], "l")
plot( 100:150, air_visit_data_grouped_by_date$no_visits[100:150], "l")

ts_agg_no_stores <- ts(stores_open[,2], start = c(1,1), frequency = 7)

# plotting to see any visit changes in Golden week (17,5) to (18,4)
autoplot(window(ts_agg_no_stores/max(ts_agg_no_stores),start = c(14,1),end = c(20,7)), series = "No of Stores Open") + 
  autolayer(window(ts_agg_visit/max(ts_agg_visit),start = c(14,1),end = c(20,7)), series = "total No of Visitors")

# distinct store ids
all_stores_ts_data <- air_visit_data_grouped_by_date
store_ids <- distinct(air_visit_data, air_store_id)

for(i in 1:829) {
  
  store_data <- filter(air_visit_data, air_store_id == store_ids[i,1])
  store_visits <- summarise(group_by(store_data, visit_date), no_visits = sum(visitors)) 
  colnames(store_visits)[2] <- store_ids[i,1]
  all_stores_ts_data <- all_stores_ts_data %>% 
                            left_join(store_visits, by = "visit_date")
                }
                
# creating daaframe for forecasts

  forecast_all_stores <- data.frame(matrix(NA, nrow = 39, ncol = 830))
  colnames(forecast_all_stores) <- colnames(all_stores_ts_data)[c(1,3:831)]
  forecast_all_stores[,1] <- c("2017-04-23", "2017-04-24","2017-04-25","2017-04-26","2017-04-27","2017-04-28","2017-04-29","2017-04-30","2017-05-01", "2017-05-02","2017-05-03","2017-05-04","2017-05-05","2017-05-06","2017-05-07","2017-05-08","2017-05-09","2017-05-10","2017-05-11","2017-05-12","2017-05-13","2017-05-14","2017-05-15","2017-05-16","2017-05-17","2017-05-18","2017-05-19","2017-05-20","2017-05-21","2017-05-22","2017-05-23","2017-05-24","2017-05-25","2017-05-26","2017-05-27","2017-05-28","2017-05-29","2017-05-30","2017-05-31")

# time series for individual stores
ts_store <- ts(store_visits[,2], start = c(1,1), frequency = 7)
autoplot(ts_store)

# check STL on aggregated data
fit <- stlf(ts_store)
fc <- forecast(fit)
autoplot(fc)


# setting directory to store time series for all air stores
setwd("C:/Users/Admin/Desktop/Kaggle/Submission")
write.csv(all_stores_ts_data, "all_stores_ts_data.csv")

# imputed data and store in a data frame
df_imp  <- data.frame(matrix(0, nrow = 478, ncol = 830))
colnames(df_imp) <- colnames(all_stores_ts_data)[2:831]
df_imp[,1] <- all_stores_ts_data$visit_date

for (i in 1:829){
    ts_df <- ts(all_stores_ts_data[,2+i], start = c(1,1), frequency = 7)
  ts_df_imp <- na.seadec(ts_df, "kalman")
  
  df_imp[,i+1] <- ts_df_imp[,1]
                }
                
# imputing and running base model forecast for next 39 days by tbats

for (i in 1:829){
ts_df <- ts(all_stores_ts_data[,2+i], start = c(1,1), frequency = 7)
ts_df_imp <- na.seadec(ts_df, "kalman") 

store_forecast <- ts_df_imp %>%
                    tbats(use.trend = TRUE, seasonal.periods = c(7,366)) %>%
                    forecast(h = 39)

forecast_all_stores[,1+i] <-  store_forecast$mean
                    }

# imputing and running base model forecast for next 39 days by ets/ nnetar

for (i in 1:829){
    ts_df <- ts(all_stores_ts_data[,2+i], start = c(1,1), frequency = 7)
  #ts_df_imp <- na.seadec(ts_df, "kalman") 
  
  store_forecast <- ts_df %>%
                    nnetar() %>%
                    forecast(h = 39)
  
  forecast_all_stores[,1+i] <-  store_forecast$mean
                }

# column to row names
forecast_all <- column_to_rownames(forecast_all_stores, "visit_date")

# Creating visitor enteries in the submission data frame

for (i in 1:32019) {
  sample_submission_mod$visitors[i] <- forecast_all[sample_submission_mod[i,3],sample_submission_mod[i,2]]
                    }

# Select the two columns required for submission

submission <- select(sample_submission_mod, id, visitors)
submission[(submission < 0)] <- 0
