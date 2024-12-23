
library(tidyverse)
data <- readRDS("data/preppedData.rds")
data <- data %>% mutate(Change_CPI = log(CPI_Motor) - log(lag(CPI_Motor)),
                        Change_OilPrice = log(OilFutureMonthly) - log(lag(OilFutureMonthly)),
                        Change_GasPrice = log(GasolineFutureMonthly) - log(lag(GasolineFutureMonthly)),
                        Change_OilVolDaily = log(Oil_Volatility_Daily) - log(lag(Oil_Volatility_Daily)),
                        Change_GasVolDaily = log(Gasoline_Volatility_Daily) - log(lag(Gasoline_Volatility_Daily)))

data_train <- data %>% filter(Year < 2023) %>% filter(Year > 2008)
data_test <- data %>% filter(Year > 2022)

add_lag <- function(eqtn, var, p){
  if(str_ends(eqtn, "~")){
    eqtn <- paste0(eqtn, " lag(",var,",",p,")")
  } else{
    eqtn <- paste0(eqtn, " + lag(",var,",",p,")")
  }
  return(eqtn)
}

library(dynlm)
p <- 4 # Number of lags
form <- "Change_CPI ~ Change_OilPrice + lag(Change_OilPrice,1)"
mdl <- dynlm(as.formula(form), data_train)
summary(mdl)
