library(tidyverse)
library(tictoc)
library(RcppRoll)
library(lubridate)
library(data.table)
library(pryr)
library(plotly)
library(lightgbm)
library(MLmetrics)
theme_set(theme_light())


MAX_LAGS <- 1912 # number of lags for test set
LAST_D <- 1913 # last training day
HORIZON <- 28 # forecast horizon
valid_date <- ymd("2016-04-25")
test_date <- ymd("2016-03-28")
# Set the flag to 1 if run on Kaggle's server to change the directories
KAGGLE <- 0
forecast_palette <- c("Train" = "#7e7e7e", "Real Values" = "#4E8F2C", "Forecast" = "#4E59DB")

state <- "CA"

lgb_param <- list(objective = "tweedie",
                  boosting_type = "gbdt",
                  metric ="rmse",
                  tweedie_variance_power = 1.2,
                  subsample = 0.7,
                  subsample_freq = 1,
                  learning_rate = 0.075,
                  num_leaves = 2047,
                  min_data_in_leaf = 4095,
                  feature_fraction = 0.5,
                  max_bin = 100,
                  n_estimators = 800,
                  boost_from_average = FALSE)

if(KAGGLE){
  calendar <- read_csv("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/calendar.csv")
  prices <- read_csv("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sell_prices.csv")
}else{
  calendar <- read_csv(here::here("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/calendar.csv"))
  prices <- read_csv(here::here("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sell_prices.csv"))  
}

stores_list <- function(){
  if(state == "CA"){
    stores <- c("CA_1", "CA_2", "CA_3", "CA_4")
  }
  if(state == "TX"){
    stores <- c("TX_1", "TX_2", "TX_3")
  }
  if(state == "WI"){
    stores <- c("WI_1", "WI_2", "WI_3")
  }
  stores
}


save_df <- function(){
  if(KAGGLE){
    saveRDS(sales, "/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sales.RDS")
    saveRDS(sales_index, "/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/index.RDS")
  }else{
    saveRDS(sales, "/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sales.RDS")
    saveRDS(sales_index, "/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/index.RDS")
  }    
}

load_df <- function(){
  if(KAGGLE){
    sales <<- readRDS("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sales.RDS")
    sales_index <<- readRDS("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/index.RDS")
  }else{
    sales <<- readRDS("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sales.RDS")
    sales_index <<- readRDS("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/index.RDS")
  }    
}


reduce_size <- function(){
  
  calendar <<- calendar %>%
    mutate_if(is.numeric, as.integer) %>%
    mutate(date = ymd(date)) %>%
    mutate(event_type_1 = ifelse(is.na(event_type_1), "Other", event_type_1),
           event_name_1 = ifelse(is.na(event_name_1), "Other", event_name_1),
           event_type_1 = as.integer(as.factor(event_type_1)),
           event_name_1 = as.integer(as.factor(event_name_1)),
           event_type_2 = ifelse(is.na(event_type_2), "Other", event_type_2),
           event_name_2 = ifelse(is.na(event_name_2), "Other", event_name_2),
           event_type_2 = as.integer(as.factor(event_type_2)),
           event_name_2 = as.integer(as.factor(event_name_2))
    ) %>%
    select(-weekday)
}

obj_size <- function(x){
  format(object.size(x), units = "Mb")
}

free_mem <- function(){
  gc()
  Sys.sleep(1)
}

# Use integer for the 6 categorical field, it save about 1.5 Gb
# Save all the indexes in files to be able to reconstruct the original one
# NOT USED ANYMORE in final model.
create_idx <- function(index){
  
  idx_enc <- index %>%
    mutate_if(is.character, ~as.integer(as.factor(.)))
  
  idx_enc <- rename_all(idx_enc, ~paste0(.,"x")) %>%
    bind_cols(index)
  
  x_id <- idx_enc %>% distinct(id, idx)
  x_item <- idx_enc %>% distinct(item_id, item_idx)
  x_store <- idx_enc %>% distinct(store_id, store_idx)
  x_state <- idx_enc %>% distinct(state_id, state_idx)
  x_cat <- idx_enc %>% distinct(cat_id, cat_idx)
  x_dept <- idx_enc %>% distinct(dept_id, dept_idx)
  
  saveRDS(x_id, here::here("wrk", "tmp", "x_id.RDS"))
  saveRDS(x_state, here::here("wrk", "tmp", "x_state.RDS"))
  saveRDS(x_cat, here::here("wrk", "tmp", "x_cat.RDS"))
  saveRDS(x_dept, here::here("wrk", "tmp", "x_dept.RDS"))
  saveRDS(x_store, here::here("wrk", "tmp", "x_store.RDS"))
  saveRDS(x_item, here::here("wrk", "tmp", "x_item.RDS"))
  
  saveRDS(idx_enc, here::here("wrk", "tmp", "idx_enc.RDS"))
  
  return(idx_enc[1:6])
}


# Create the initial dataframe
# is_train was to create either for train or for test purpose.
# and is not used anymore in this version
create_long <- function(is_train = TRUE, store_id){
  
  if(KAGGLE){
    sales <- fread("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sales_train_validation.csv", drop = paste0("d_", 1:(LAST_D - MAX_LAGS)))
  }else{
    sales <- fread(here::here("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sales_train_validation.csv"), drop = paste0("d_", 1:(LAST_D - MAX_LAGS)))
  }
  
  sales <- sales %>% filter(store_id == !!store_id)
  
  valid_columns <- as_tibble(t(rep(NA, 28)))
  colnames(valid_columns) <- paste0("d_", seq(LAST_D + 1, LAST_D + HORIZON, 1))
  valid_new <- as_tibble(sapply(valid_columns, function(x) { x <- rep(NA, NROW(sales))} ))
  
  sales_grid <- sales %>% 
    bind_cols(valid_new) %>%
    mutate_at(vars(starts_with("d_")), as.integer) %>%
    group_by(id) %>%
    pivot_longer(cols = -c(1:6), names_to = "d") %>%
    ungroup()
  
  
  sales_grid <- sales_grid %>% 
    left_join(calendar) %>%
    left_join(prices, on = c("store_id", "item_id", "wm_yr_wk"))
  
  sales_valid <- sales_grid %>%
    filter(date >= valid_date)
  
  sales_grid <- sales_grid %>%
    filter(date < valid_date) %>%
    na.omit()
  
  sales_grid <- sales_grid %>%
    bind_rows(sales_valid) %>%
    mutate(revenu = sell_price * value) %>%
    select(-sell_price)
  
}


date_features <- function(){
  
  sales_base <- sales_base %>%
    mutate(snap = case_when(
      state_id == "CA" ~ snap_CA,
      state_id == "TX" ~ snap_TX,
      state_id == "WI" ~ snap_WI
    )) %>% 
    mutate(mweek = ceiling(day(date) / 7),
           day = day(date),
           week = lubridate::week(date),
           is_weekend = ifelse(wday <= 2, 1, 0)
    ) %>%
    select(-snap_CA, - snap_TX, -snap_WI)
  
}


lag_features<- function(){
  # Get lag 28 & 364
  sales_base <- sales_base %>%
    mutate(is_zero = ifelse(value == 0, 1, 0)) %>%
    group_by(id) %>%
    arrange(id, date) %>%
    mutate(
      value_lag1 = lag(value, 1),
      value_lag7 = lag(value, 7),
      value_lag14 = lag(value, 14),
      value_lag21 = lag(value, 21),
      value_lag28 = lag(value, 28),
      value_lag29 = lag(value, 29),
      value_lag30 = lag(value, 30),
      value_lag31 = lag(value, 31),
      value_lag32 = lag(value, 32),
      value_lag33 = lag(value, 33),
      value_lag34 = lag(value, 34),
      value_lag35 = lag(value, 35),
      value_lag36 = lag(value, 36),
      value_lag37 = lag(value, 37),
      value_lag38 = lag(value, 38),
      value_lag39 = lag(value, 39),
      value_lag40 = lag(value, 40),
      value_lag41 = lag(value, 41),
      value_lag42 = lag(value, 42),
      value_lag365 = lag(value, 365),
      revenu_lag28 = lag(revenu, 28),
      is_zero_lag28 = lag(is_zero, 28),
      snap_eve = lead(snap, 1),
      snap_nxt = lag(snap, 1),
      event_eve = lead(event_type_1, 1),
      short_trend = (value_lag28 + 1) / (ifelse(!is.na(value_lag35), value_lag35, 0) + 1),
      medium_trend = (value_lag28 + 1) / (ifelse(!is.na(value_lag42), value_lag42, 0) + 1),
      long_trend = (value_lag28 + 1) / (ifelse(!is.na(value_lag365), value_lag365, 0) + 1),
    ) %>%
    ungroup()
  
  sales_base <- sales_base %>%
    mutate(snap_eve = ifelse(is.na(snap_eve), 0, snap_eve),
           snap_nxt = ifelse(is.na(snap_nxt), 0, snap_nxt),
           event_eve = ifelse(is.na(event_eve), 0, event_eve))
  
  snap_density <- expand.grid(snap_eve = c(0,1), snap = c(0,1), snap_nxt = c(0,1)) %>%
    mutate(snap_density = row_number())
  
  sales_base <- sales_base %>%
    left_join(snap_density, by = c("snap_eve", "snap", "snap_nxt")) %>%
    select(-snap_eve, -snap_nxt)
  
  sales_base <- sales_base %>% 
    group_by(id) %>%
    arrange(id, date) %>%
    mutate(
      item_ma7_lag1 = roll_meanr(value_lag1, 7),
      item_ma14_lag1 = roll_meanr(value_lag1, 14),
      item_ma28_lag1 = roll_meanr(value_lag1, 28),
      item_ma60_lag1 = roll_meanr(value_lag1, 60),
      
      item_ma7_lag7 = roll_meanr(value_lag7, 7),
      item_ma14_lag7 = roll_meanr(value_lag7, 14),
      item_ma28_lag7 = roll_meanr(value_lag7, 28),
      item_ma60_lag7 = roll_meanr(value_lag7, 60),
      
      item_ma7_lag14 = roll_meanr(value_lag14, 7),
      item_ma14_lag14 = roll_meanr(value_lag14, 14),
      item_ma28_lag14 = roll_meanr(value_lag14, 28),
      item_ma60_lag14 = roll_meanr(value_lag14, 60),
      
      item_ma7_lag21 = roll_meanr(value_lag21, 7),
      item_ma14_lag21 = roll_meanr(value_lag21, 14),
      item_ma28_lag21 = roll_meanr(value_lag21, 28),
      item_ma60_lag21 = roll_meanr(value_lag21, 60),
      
      item_ma7_lag28 = roll_meanr(value_lag28, 7),
      item_ma14_lag28 = roll_meanr(value_lag28, 14),
      item_ma28_lag28 = roll_meanr(value_lag28, 28),
      item_ma60_lag28 = roll_meanr(value_lag28, 60),
      item_ma180_lag28 = roll_meanr(value_lag28, 180),
      
      revenu_ma7_lag28 = roll_meanr(revenu_lag28, 7),
      revenu_sd7_lag28 = roll_sdr(revenu_lag28, 7),
      is_zero_ma7_lag28 = roll_meanr(is_zero_lag28, 7),
      is_zero_sd7_lag28 = roll_sdr(is_zero_lag28, 7),
      
      item_sd7_lag28 = roll_sdr(value_lag28, 7),
      item_sd14_lag28 = roll_sdr(value_lag28, 14),
      item_sd30_lag28 = roll_sdr(value_lag28, 30),
      item_sd60_lag28 = roll_sdr(value_lag28, 60),
      item_sd180_lag28 = roll_sdr(value_lag28, 180),
      item_max7_lag28 = roll_maxr(value_lag28, 7),     
      item_max28_lag28 = roll_maxr(value_lag28, 28),
      item_min7_lag28 = roll_minr(value_lag28, 7),   
      item_min28_lag28 = roll_minr(value_lag28, 28),  
      
      # item_ma7_lag35 = roll_meanr(value_lag35, 7),
      # item_ma7_lag42 = roll_meanr(value_lag42, 7),
      # item_ma7_lag365 = roll_meanr(value_lag365, 7),
      # short_trend = ifelse(!is.na(item_ma7_lag35) & item_ma7_lag35 != 0, (item_ma7_lag28 - item_ma7_lag35) / item_ma7_lag35, 0),
      # medium_trend = ifelse(!is.na(item_ma7_lag42) & item_ma7_lag42 != 0, (item_ma7_lag28 - item_ma7_lag42) / item_ma7_lag42, 0),
      # long_trend = ifelse(!is.na(item_ma7_lag365) & item_ma7_lag365 != 0, (item_ma7_lag28 - item_ma7_lag365) / item_ma7_lag365, 0),
      
    ) %>%
    ungroup() %>%
    select(-value_lag1, -value_lag7, -value_lag14, -value_lag21, -revenu, -is_zero, -is_zero_lag28, -snap)
  
}

stat_features <- function(){
  
  sales_base <- sales_base %>%
    mutate(d_num = as.integer(str_replace(d, "d_", ""))) %>%
    group_by(id) %>%
    mutate(release = min(d_num)) %>%
    ungroup() %>%
    select(-d_num)
  
  sales_base <- sales_base %>%
    mutate(is_zero = ifelse(value == 0, 1, 0)) %>%
    group_by(id, wday) %>%
    mutate(wday_avg = mean(value, na.rm = T),
           wday_sd = sd(value, na.rm = T),
           wday_max = max(value, na.rm = T),    # NEW
           wday_zero_avg = mean(is_zero, na.rm = T)) %>%
    ungroup()
  
  sales_base <- sales_base %>%
    group_by(id, mweek) %>%
    mutate(mweek_avg = mean(value, na.rm = T),
           mweek_sd = sd(value, na.rm = T),
           mweek_max = max(value, na.rm = T),    # NEW
           mweek_zero_avg = mean(is_zero, na.rm = T)) %>%
    ungroup() %>%
    select(-is_zero)
  
}

price_features <- function(){
  
  prices_feat <- prices %>%
    group_by(store_id, item_id) %>%
    mutate(price_avg = mean(sell_price),
           price_std = sd(sell_price),
           price_min = min(sell_price),
           price_max = max(sell_price),
           price_norm_item = sell_price / max(sell_price)) %>%
    ungroup()
  
  prices_feat <- prices_feat %>%
    mutate(price_norm = sell_price / max(sell_price)) %>%
    select(-sell_price)
  
  
  prices_trend <- prices %>%
    group_by(store_id, item_id) %>%
    arrange(store_id, item_id, wm_yr_wk) %>%
    mutate(price_diff = (sell_price - lag(sell_price)) / lag(sell_price),
           price_rap = sell_price / lag(sell_price)) %>%
    ungroup() %>%
    select(-sell_price)
  
  setDT(sales_base)
  setDT(prices_feat)
  setDT(prices_trend)
  
  sales_base[prices_feat, `:=`(price_avg = price_avg, price_std = price_std,
                               price_min= price_min, price_max = price_max, 
                               price_norm = price_norm, price_norm_item = price_norm_item), on = .(store_id, item_id, wm_yr_wk)]
  sales_base[prices_trend, `:=`(price_diff = price_diff, price_rap = price_rap), on = .(store_id, item_id, wm_yr_wk)]
  #sales_base[,("wm_yr_wk") := NULL]
  
}


encoding_features <- function(){
  
  sales_base[, `:=`(enc_cat_mean = mean(value, na.rm = T), enc_cat_sd = sd(value, na.rm = T)), by = .(cat_id)]
  sales_base[, `:=`(enc_dept_mean = mean(value, na.rm = T), enc_dept_sd = sd(value, na.rm = T)), by = .(dept_id)]
  sales_base[, `:=`(enc_item_mean = mean(value, na.rm = T), enc_item_sd = sd(value, na.rm = T)), by = .(item_id)]
  sales_base[, `:=`(enc_cat_ohe = as.numeric(as.factor(cat_id)), enc_dept_ohe = as.numeric(as.factor(dept_id)), enc_item_ohe = as.numeric(as.factor(item_id)))]
  
}

# extra_data <- function(){
#   
#   if(KAGGLE){
#     load("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/extra_data_valid.RData")
#   }else{
#     load(file = ("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/extra_data_valid.RData")  )
#   }


#   setDT(sales_base)
#   
#   sales_base[full_dept_id, `:=`(full_dept_avg = dept_avg, full_dept_sd = dept_sd), on = .(dept_id)]
#   sales_base[full_item_id, `:=`(full_item_avg = item_avg, full_item_sd = item_sd), on = .(item_id)]
#   sales_base[full_cat_id, `:=`(full_cat_avg = cat_avg, full_cat_sd = cat_sd), on = .(cat_id)]
#   sales_base[full_state_cat, `:=`(full_state_cat_avg = state_cat_avg, full_state_cat_sd = state_cat_sd), on = .(state_id, cat_id)]
#   sales_base[full_state_dept, `:=`(full_state_dept_avg = state_dept_avg, full_state_dept_sd = state_dept_sd), on = .(state_id, dept_id)]
#   sales_base[full_state_item, `:=`(full_state_item_avg = state_item_avg, full_state_item_sd = state_item_sd), on = .(state_id, item_id)]
#   sales_base[price_mom_m, `:=`(price_mom_m = price_mom_m), on = .(item_id, store_id, d)]
#   sales_base[price_mom_y, `:=`(price_mom_y = price_mom_y), on = .(item_id, store_id, d)]
#   sales_base[prices_change, `:=`(nb_stores = nb_stores, nb_states = nb_states, rapp_change_avg = rapp_change_avg), on = .(item_id, store_id, wm_yr_wk)]
#   sales_base[is.na(nb_stores), `:=`(nb_stores = 0)]
#   sales_base[is.na(nb_states), `:=`(nb_states = 0)]
#   sales_base[is.na(rapp_change_avg), `:=`(rapp_change_avg = 0)]
#   sales_base[,("wm_yr_wk") := NULL]
# }




make_predictions_days <- function(train_sales, dates, beg_date, lgb) {
  
  for(i in seq_along(dates)){
    
    print(paste0("Prediction for day ", i, ", date ", dates[i]))
    #create chunk
    chunk <- train_sales %>%
      filter(date == dates[i]) %>%
      select(-d)
    #remove variables
    chunk$date <- NULL
    chunk$value <- NULL
    chunk$id <- NULL
    #create index
    row_index <- train_sales %>%
      filter(date == dates[i]) %>%
      select(date, d, id)
    
    # predict
    valid_mx <- data.matrix(chunk)
    preds <- predict(lgb, valid_mx)
    rm(valid_mx); rm(chunk)
    # add index
    new_values <- cbind(row_index, preds)
    
    # Add the predictions to value
    new_values <- train_sales %>%
      left_join(new_values, by = c("d", "date", "id")) %>%
      mutate(value = ifelse(!is.na(preds), preds, value))
    
    new_values <- new_values %>%
      group_by(id) %>%
      arrange(id, date) %>%
      mutate(
        value_lag1 = lag(value, 1),
        value_lag7 = lag(value, 7),
        value_lag14 = lag(value, 14),
        value_lag28 = lag(value, 28),
        
      ) %>%
      ungroup()
    
    new_values <- new_values %>% 
      group_by(id) %>%
      arrange(id, date) %>%
      mutate(
        item_ma7_lag1 = roll_meanr(value_lag1, 7),
        item_ma14_lag1 = roll_meanr(value_lag1, 14),
        item_ma28_lag1 = roll_meanr(value_lag1, 28),
        item_ma60_lag1 = roll_meanr(value_lag1, 60),
        
        item_ma7_lag7 = roll_meanr(value_lag7, 7),
        item_ma14_lag7 = roll_meanr(value_lag7, 14),
        item_ma28_lag7 = roll_meanr(value_lag7, 28),
        item_ma60_lag7 = roll_meanr(value_lag7, 60),
        
        item_ma7_lag14 = roll_meanr(value_lag14, 7),
        item_ma14_lag14 = roll_meanr(value_lag14, 14),
        item_ma28_lag14 = roll_meanr(value_lag14, 28),
        item_ma60_lag14 = roll_meanr(value_lag14, 60),
        
        item_ma7_lag28 = roll_meanr(value_lag28, 7),
        item_ma14_lag28 = roll_meanr(value_lag28, 14),
        item_ma28_lag28 = roll_meanr(value_lag28, 28),
        item_ma60_lag28 = roll_meanr(value_lag28, 60),
        
        item_sd7_lag28 = roll_sdr(value_lag28, 7),
        item_sd14_lag28 = roll_sdr(value_lag28, 14),
        item_sd30_lag28 = roll_sdr(value_lag28, 30),
        item_sd60_lag28 = roll_sdr(value_lag28, 60),
      ) %>%
      ungroup()
    
    train_sales$value <- new_values$value
    train_sales$value_lag28 <- new_values$value_lag28
    train_sales$item_ma7_lag1 <- new_values$item_ma7_lag1
    train_sales$item_ma14_lag1 <- new_values$item_ma14_lag1
    train_sales$item_ma28_lag1 <- new_values$item_ma28_lag1
    train_sales$item_ma60_lag1 <- new_values$item_ma60_lag1
    
    train_sales$item_ma7_lag7 <- new_values$item_ma7_lag7
    train_sales$item_ma14_lag7 <- new_values$item_ma14_lag7
    train_sales$item_ma28_lag7 <- new_values$item_ma28_lag7
    train_sales$item_ma60_lag7 <- new_values$item_ma60_lag7
    
    train_sales$item_ma7_lag14 <- new_values$item_ma7_lag14
    train_sales$item_ma14_lag14 <- new_values$item_ma14_lag14
    train_sales$item_ma28_lag14 <- new_values$item_ma28_lag14
    train_sales$item_ma60_lag14 <- new_values$item_ma60_lag14
    
    train_sales$item_ma7_lag28 <- new_values$item_ma7_lag28
    train_sales$item_ma14_lag28 <- new_values$item_ma14_lag28
    train_sales$item_ma28_lag28 <- new_values$item_ma28_lag28
    train_sales$item_ma60_lag28 <- new_values$item_ma60_lag28
    
    train_sales$item_sd7_lag28 <- new_values$item_sd7_lag28
    train_sales$item_sd14_lag28 <- new_values$item_sd14_lag28
    train_sales$item_sd30_lag28 <- new_values$item_sd30_lag28
    train_sales$item_sd60_lag28 <- new_values$item_sd60_lag28
    
    rm(new_values)
    
  }
  return(train_sales)
}


train_test_model <- function(){
  
  wallmart_train <- sales %>%
    filter(date < test_date) %>%
    select(-c(id, d, date))
  
  #wallmart_train <- na.omit(wallmart_train)
  
  wallmart_test <- sales %>%
    filter(date >= test_date) %>%
    filter(date < valid_date) %>%
    select(-c(id, d, date))
  
  rm(sales, pos = ".GlobalEnv");free_mem()
  
  wallmart_train$date <- NULL
  wallmart_test$date <- NULL
  
  response <- wallmart_train$value
  test_response <- wallmart_test$value
  
  wallmart_train$value <- NULL
  wallmart_test$value <- NULL
  
  # tic()
  print("Test - Convert train to matrix")
  wallmart_train_mx <- data.matrix(wallmart_train)
  rm(wallmart_train); free_mem()
  d0 <- lgb.Dataset(wallmart_train_mx, label = response, free_raw_data=F)
  rm(wallmart_train_mx); free_mem()
  print("Test - Convert test to matrix")
  wallmart_test_mx <-data.matrix(wallmart_test)
  dval <- lgb.Dataset(wallmart_test_mx, label = test_response, free_raw_data=F) 
  rm(wallmart_test_mx); free_mem()
  
  #  train 
  valids <- list(train = d0, valid = dval)
  print("Test - Train model")
  lgb <- lgb.train(params = lgb_param, data = d0, valids = valids, eval_freq = 200, early_stopping_rounds = 400, reset_data = TRUE, verbose = 1, seed = 123)
  
  # Predict
  oof_pred <- predict(lgb, data.matrix(wallmart_test))
  
  cat("best iter :" , lgb$best_iter, "best score :", RMSE(oof_pred, test_response[which(!is.na(test_response))]) ,"\n" )
  iter <- lgb$best_iter
  
  
  weight_loss_valid <- as.numeric(lgb$record_evals$valid$rmse$eval)
  weight_loss_train <- as.numeric(lgb$record_evals$train$rmse$eval)
  
  gg <- tibble(type = "valid", loss = weight_loss_valid) %>%
    bind_rows(tibble(type = "train", loss = weight_loss_train)) %>% 
    group_by(type) %>%
    mutate(iteration = row_number()) %>%
    ungroup() %>%
    ggplot(aes(iteration, loss, color = type)) +
    geom_line() +
    scale_color_manual(values = c("train" = "#7e7e7e", "valid" = "#4E59DB")) +
    labs(title = paste("Learning curve ", store_id, sep =" - "))
  
  plot1 <- ggplotly(gg)
  
  return(list(lgb, iter, plot1))
}


predict_test <- function(lgb, iter, sales_index, store_id){
  
  print("test - Make predictions")
  
  sales_pred <- sales %>%
    mutate(rows = row_number()) %>%
    filter(date > (ymd(test_date) - 365))
  
  rm(sales, pos = ".GlobalEnv");free_mem()
  
  index_keep <- sales_pred$rows
  sales_index <- sales_index[index_keep,]
  sales_pred$rows <- NULL
  
  real <- sales_pred %>%
    bind_cols(sales_index[, c(3,4)]) %>%  # add that after mean encoding the dept and cat
    select(cat_id, dept_id, date, value) 
  
  dates <- seq(ymd(test_date), ymd(test_date)+27, by = "days")
  
  beg_date <- ymd(test_date)
  sales_pred <- make_predictions_days(sales_pred, dates, beg_date, lgb)
  
  if(!KAGGLE){
    real_summary <- real %>%
      group_by(dept_id, date) %>%
      summarise(total = sum(value)) %>%
      ungroup()
    
    real_summary <- real_summary %>%
      filter(date >= ymd("2016-01-01")) %>%
      mutate(type = ifelse(date > test_date, "Real Values", "Train"))
    
    gg <- sales_pred %>%
      bind_cols(sales_index[, c(3,4)]) %>%   # add that after mean encoding the dept and cat
      filter(date > ymd(test_date)) %>%
      group_by(dept_id, date) %>%
      summarise(total = sum(value)) %>%
      ungroup() %>%
      mutate(type = "Forecast", "Train") %>%
      bind_rows(real_summary) %>%
      ggplot(aes(date, total, color = type)) + 
      geom_line(aes(group=type)) +
      scale_color_manual(values = forecast_palette) +      
      facet_wrap(~dept_id)  +
      labs(title = paste("Prediction on test  for store", store_id, sep =" "),
           color = "Legend",
           y = "Daily sales") +
      theme(axis.title.x = element_blank())
    
    plot1 <- ggplotly(gg)
    
  }
  
  if(!KAGGLE){
    
    imp <- lgb.importance(lgb)
    
    gg <- imp %>%
      mutate(Feature = fct_reorder(Feature, Gain)) %>%
      arrange(desc(Gain)) %>%
      head(50) %>%
      ggplot(aes(Feature, Gain, fill = Feature)) +
      geom_col() +
      coord_flip() +
      labs(title = paste("Features importance for store", store_id, sep =" "))
    
    plot2 <- ggplotly(gg)
    
    features <- imp %>%
      mutate(Feature = fct_reorder(Feature, Gain)) %>%
      arrange(desc(Gain)) 
    
    write_csv(features, here::here("output", paste0("features_",store_id,".csv")))
  }
  
  pred  <- sales_pred %>% 
    filter(date >= test_date, date < valid_date) %>%
    select(value)
  
  test <- real %>% filter(date >= test_date) %>%
    filter(date < valid_date)
  
  cat("best iter :" , lgb$best_iter, "best score :", RMSE(pred$value, test$value) ,"\n" )
  
  sales_pred$id <- sales_index$id
  sales_pred$d <- sales_index$d
  
  predictions <- sales_pred %>% 
    filter(date >= test_date, date < valid_date) %>%
    select(id, d, value) %>%
    pivot_wider(id_cols = id, names_from = d, values_from = value)
  
  print("Write test predictions to file")
  
  if(KAGGLE){
    write_csv(predictions, paste0("M5_store_test_",store_id,".csv"))
  }else{
    write_csv(predictions, here::here("output", paste0("M5_store_test_",store_id,".csv")))  
    return(list(plot1, plot2))
  }
  
  
}


train_model <- function(iter = 1200){
  
  wallmart_train <- sales %>%
    filter(date < valid_date) %>%
    select(-c(id, d, date))
  
  rm(sales, pos = ".GlobalEnv");free_mem()    
  
  #wallmart_train <- na.omit(wallmart_train)
  
  wallmart_train$date <- NULL
  response <- wallmart_train$value
  wallmart_train$value <- NULL
  
  print("Validation - Convert train to matrix")
  wallmart_train_mx <- data.matrix(wallmart_train)
  rm(wallmart_train); free_mem()
  
  print("Validation - Train model")
  d0 <- lgb.Dataset(wallmart_train_mx, label = response)
  lgb <- lgb.train(params = lgb_param, data = d0, nrounds = iter * 1.05, verbose = -1, seed = 123)
  
  return(list(lgb))
}


predict_final <- function(lgb, sales_index, store_id){
  
  sales <- sales %>%
    mutate(rows = row_number()) %>%
    filter(date > valid_date - 365)
  
  index_keep <- sales$rows
  sales_index <- sales_index[index_keep,]
  sales$rows <- NULL
  
  dates <- seq(valid_date, valid_date+27, by = "days")
  beg_date <- valid_date
  
  sales <- make_predictions_days(sales, dates, beg_date, lgb)
  
  if(!KAGGLE){
    gg <- sales %>%
      bind_cols(sales_index[, c(3,4)]) %>%   # add that after mean encoding the dept and cat
      filter(date > ymd("2016-01-01")) %>%
      group_by(dept_id, date) %>%
      summarise(total = sum(value)) %>%
      ungroup() %>%      
      mutate(type = ifelse(date > test_date, "Forecast", "Train")) %>%
      ggplot(aes(date, total, color = type, label = total)) + 
      scale_color_manual(values = forecast_palette) +
      geom_line(aes(group=type)) +
      facet_wrap(~dept_id) +
      labs(title = paste("Final validation for store", store_id, sep =" "),
           color = "Legend",
           y = "Daily sales") +
      theme(axis.title.x = element_blank())
    
    plot <- ggplotly(gg)
    
    eval <- fread(here::here("/Users/adityasj/Documents/Assignments/SDM_Assignment/Project/M5_Forcasting/sales_train_validation.csv"), drop = paste0("d_", 1:LAST_D))
    
    eval <- eval %>%
      filter(store_id == !!store_id) %>%
      mutate(id = str_replace(id, "evaluation", "validation")) %>%
      group_by(id) %>%
      pivot_longer(cols = -c(1:6), names_to = "d") %>%
      ungroup() %>%
      select(id, d, real = value)
    
    pred <- sales %>%
      filter(date >= valid_date) %>%
      select(id, d, pred = value) %>%
      left_join(eval, by = c("id", "d"))
    
    cat("best score evaluation :", RMSE(pred$real, pred$pred) ,"\n" )
  }
  
  
  print("Validation - Create submission file")
  
  sales$id <- sales_index$id
  sales$d <- sales_index$d
  
  predictions <- sales %>% 
    filter(date >= valid_date) %>%
    select(id, d, value) %>%
    pivot_wider(id_cols = id, names_from = d, values_from = value)
  
  print("Write predictions to file")
  
  if(KAGGLE){
    write_csv(predictions, paste0("M5_store_valid_",store_id,".csv"))
  }else{
    write_csv(predictions, here::here("output", paste0("M5_store_valid_",store_id,".csv")))  
    return(plot)
  }
  
}

store_id <- "CA_1"
sales_base <- create_long(T, store_id)
sales_base <- date_features()
sales_base <- stat_features()
sales_base <- lag_features()
sales_base <- price_features()
sales_base <- encoding_features()
sales <- sales_base %>%
  select(-state_id, -store_id)
sales_index <- sales[,1:5]
sales[, c("item_id", "dept_id", "cat_id"):=NULL]
save_df()  
rm(sales_base); free_mem()
model_fit <- train_test_model()
iter <- model_fit[[2]]
lgb <- model_fit[[1]]
print(model_fit[[3]])