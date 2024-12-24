
#### Importing Part ####

library(tidyverse)
library(dynlm)

data <- readRDS("data/preppedData.rds")
data <- data %>% mutate(Change_CPI = log(CPI_Motor) - log(lag(CPI_Motor)),
                        Change_OilPrice = log(OilMonthlyAverage) - log(lag(OilMonthlyAverage)),
                        Change_GasPrice = log(GasolineFutureMonthly) - log(lag(GasolineFutureMonthly)),
                        Change_OilVolDaily = log(Oil_Volatility_Daily) - log(lag(Oil_Volatility_Daily)),
                        Change_GasVolDaily = log(Gasoline_Volatility_Daily) - log(lag(Gasoline_Volatility_Daily)))

data_train <- data %>% filter(Year < 2023)
data_test <- data %>% filter(Year > 2022)
n_test <- nrow(data_test)

add_lag <- function(eqtn, var, p){
  if(str_ends(eqtn, "~")){
    eqtn <- paste0(eqtn, " lag(",var,",",p,")")
  } else{
    eqtn <- paste0(eqtn, " + lag(",var,",",p,")")
  }
  return(eqtn)
}

RMSE_out_of_sample <- function(mdl, data_test, data, printout = T){
  data$prediction <- predict(mdl, data)
  data_test <- left_join(data_test, data %>% select(Year, Month, prediction)) %>%
    select(Year, Month, Change_CPI, prediction) %>% filter(complete.cases(.))
  npredictions <- sum(!is.na(data_test$prediction))
  lastmonth <- paste(data_test$Year[nrow(data_test)],data_test$Month[nrow(data_test)], sep = "-")
  rmse <- sqrt(sum((data_test$Change_CPI - data_test$prediction)**2,na.rm = T))
  rmse_0 <- sqrt(sum((data_test$Change_CPI)**2,na.rm = T))
  data_train <- data %>% filter(!Year %in% data_test$Year)  
  rmse_mean <- sqrt(sum((data_test$Change_CPI- mean(data_train$Change_CPI, na.rm = T)  )**2,na.rm = T))
  if(printout){
    print(paste("Number of Predicted Terms:", npredictions))
    print(paste("Last predicted month:", lastmonth))
    print(paste("RMSE:", round(rmse, 5)))
    print(paste("RMSE of Predicting Zero Only (Benchmark):", round(rmse_0, 5)))
    print(paste("RMSE of Predicting Historical Average (Benchmark):", round(rmse_mean, 5)))
    print(paste("Variance of Real Values:", round(var(data_test$Change_CPI),5)))
    print(paste("Std Dev of Real Values:", round(sd(data_test$Change_CPI),5)))
    print(paste("Average Real Value:", round(mean(data_test$Change_CPI),5)))
    print(paste("Average Prediction:", round(mean(data_test$prediction),5)))
  }
  return(rmse)
}

#### Prediction 1 ####
p <- 2 # Number of lags
exp_vars <- c("Change_OilPrice", "Change_GasPrice") 
frml <- "Change_CPI ~"
for(v in exp_vars){
  for(i in 1:p){
    frml <- add_lag(frml, v, i)
  }
}

frml <- add_lag(frml, "Change_CPI", 1)
frml <- add_lag(frml, "Change_CPI", 12)

mdl <- lm(as.formula(frml), data_train)
summary(mdl)
RMSE_out_of_sample(mdl, data_test, data)

#### Plot ####

# Check the graphs

data %>% mutate(Date = as.yearmon(paste(Year, Month, sep = "-"))) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x = Date)) + geom_line(aes(y = Change_CPI, colour = "CPIMotor")) +
  geom_line(aes(y = Change_GasPrice, colour = "GasolineFutures")) +
  geom_line(aes(y = Change_OilPrice, colour = "OilFutures")) + 
  scale_colour_manual("", 
                      breaks = c("CPIMotor", "GasolineFutures", "OilFutures"),
                      values = c("red", "green", "blue"))

#### Prediction 2 ####

# Train only between 2015 and 2020, 
# since the price movements are different during Covid then any other time period

p <- 2 # Number of lags
exp_vars <- c("Change_OilPrice", "Change_GasPrice") 
frml <- "Change_CPI ~"
for(v in exp_vars){
  for(i in 1:p){
    frml <- add_lag(frml, v, i)
  }
}

frml <- add_lag(frml, "Change_CPI", 1)
frml <- add_lag(frml, "Change_CPI", 12)

mdl <- lm(as.formula(frml), data_train %>% filter(Year < 2020))
summary(mdl)
RMSE_out_of_sample(mdl, data_test, data)

# Removing Covid Period increases the accuracy of out-of-sample predictions

#### A Simple XGBoost Model ####

library(xgboost)
dataxg <- data %>% 
  mutate(Change_CPI_1 = lag(Change_CPI),
         Change_CPI_12 = lag(Change_CPI, 12),
         Change_OilPrice_1 = lag(Change_OilPrice, 1),
         Change_OilPrice_2 = lag(Change_OilPrice, 2),
         Change_GasPrice_1 = lag(Change_GasPrice, 1),
         Change_GasPrice_2 = lag(Change_GasPrice, 2)) %>%
  select(Year, Month, Change_CPI, Change_CPI_1, Change_CPI_12, Change_OilPrice_1, Change_OilPrice_2,
         Change_GasPrice_1, Change_GasPrice_2) %>%
  filter(complete.cases(.)) 

dataxg_train <- dataxg %>% filter(Year < 2023)
dataxg_test <- dataxg %>% filter(Year > 2022)

xgb_model <- xgboost(data = as.matrix(dataxg_train %>% select(-Year, -Month, -Change_CPI)), 
                     label = (dataxg_train$Change_CPI), 
                     nrounds = 100, 
                     objective = "reg:squarederror")

test_x <- as.matrix(dataxg_test %>% select(-Year, -Month, -Change_CPI))
test_y <- dataxg_test$Change_CPI
preds <- predict(xgb_model, test_x)
rmse <- sqrt(mean((preds - test_y)^2))
print(paste("RMSE:", rmse))

# Improves a lot

### Lets add Month for seasonality as well

xgb_model_ssnlty <- xgboost(data = as.matrix(dataxg_train %>% select(-Year, -Change_CPI)), 
                     label = (dataxg_train$Change_CPI), 
                     nrounds = 100, 
                     objective = "reg:squarederror")

test_x <- as.matrix(dataxg_test %>% select(-Year, -Change_CPI))
test_y <- dataxg_test$Change_CPI
preds <- predict(xgb_model_ssnlty, test_x)
rmse <- sqrt(mean((preds - test_y)^2))
print(paste("RMSE:", rmse))

# Imrpoves a little more

#### XGBoost without Covid period ####

dataxg_train <- dataxg %>% filter(Year < 2020)
dataxg_test <- dataxg %>% filter(Year > 2022)

xgb_model <- xgboost(data = as.matrix(dataxg_train %>% select(-Year, -Month, -Change_CPI)), 
                     label = (dataxg_train$Change_CPI), 
                     nrounds = 100, 
                     objective = "reg:squarederror")

test_x <- as.matrix(dataxg_test %>% select(-Year, -Month, -Change_CPI))
test_y <- dataxg_test$Change_CPI
preds <- predict(xgb_model, test_x)
rmse <- sqrt(mean((preds - test_y)^2))
print(paste("RMSE:", rmse))

# Even better

## Lets add Seasonality as well

xgb_model_ssnlty <- xgboost(data = as.matrix(dataxg_train %>% select(-Year, -Change_CPI)), 
                            label = (dataxg_train$Change_CPI), 
                            nrounds = 100, 
                            objective = "reg:squarederror")

test_x <- as.matrix(dataxg_test %>% select(-Year, -Change_CPI))
test_y <- dataxg_test$Change_CPI
preds <- predict(xgb_model_ssnlty, test_x)
rmse <- sqrt(mean((preds - test_y)^2))
print(paste("RMSE:", rmse))

