library(tidyverse)
library(readxl)

# Cpi Motor
cpimotor <- read_excel("data/cpimotor_all.xlsx", sheet = "BLS Data Series", skip = 11)
colnames(cpimotor)[2:13] <- 1:12
cpimotor <- cpimotor %>% select(-HALF1, -HALF2) %>% 
  pivot_longer(`1`:`12`, names_to = "Month",values_to = "CPI_Motor") %>% 
  mutate(Month = as.numeric(Month))

# Gasoline Futures
gasoline_daily <- read_xls("data/gasolinefutures_daily.xls", sheet = 2, skip = 2) 
colnames(gasoline_daily)[2] <- "GasolineFutureDaily"
gasoline_weekly <- read_xls("data/gasolinefutures_weekly.xls", sheet = 2, skip = 2)
colnames(gasoline_weekly)[2] <- "GasolineFutureWeekly"
gasoline_monthly <- read_xls("data/gasolinefutures_monthly.xls", sheet = 2, skip = 2)
colnames(gasoline_monthly)[2] <- "GasolineFutureMonthly"

gasoline_daily <- gasoline_daily %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% select(-Date)

gasoline_weekly <- gasoline_weekly %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% select(-Date)

gasoline_monthly <- gasoline_monthly %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% select(-Date)

Gasoline <- left_join(gasoline_monthly %>% select(Year, Month, GasolineFutureMonthly), 
                      gasoline_weekly %>% group_by(Year, Month) %>%
            summarise(Gasoline_Volatility_Weekly = sd(GasolineFutureWeekly))) %>%
  left_join(gasoline_daily %>% group_by(Year, Month) %>%
              summarise(Gasoline_Volatility_Daily = sd(GasolineFutureDaily)))

# Oil Futures

oil_daily <- read_xlsx("data/oilfutures_daily.xlsx") %>% select(Date, Close) %>%
  rename(OilFutureDaily = Close) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% select(-Date)
oil_weekly <- read_xlsx("data/oilfutures_weekly.xlsx") %>% select(Date, Close) %>%
  rename(OilFutureWeekly = Close) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% select(-Date)
oil_monthly <- read_xlsx("data/oilfutures_monthly.xlsx") %>% select(Date, Close) %>%
  rename(OilFutureMonthly = Close) %>% 
  mutate(Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>% select(-Date)



Oil <- left_join(oil_daily %>% group_by(Year, Month) %>%
                   summarise(Oil_Volatility_Daily = sd(OilFutureDaily),
                             OilMonthlyAverage = mean(OilFutureDaily)), 
                 oil_weekly %>% group_by(Year, Month) %>%
                        summarise(Oil_Volatility_Weekly = sd(OilFutureWeekly)))

dfcombined <- left_join(cpimotor, Gasoline) %>% left_join(Oil)

saveRDS(dfcombined, file = "data/preppedData.rds")
