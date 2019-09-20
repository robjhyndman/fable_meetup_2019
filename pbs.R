library(tidyverse)
library(tsibble)
library(tsibbledata)
library(feasts)
library(fable)

PBS

PBS %>% filter(ATC2=="H02") %>% autoplot(Scripts)

# Combine concession and type

PBS <- PBS %>%
  group_by(ATC1,ATC2) %>%
  summarise(Scripts = sum(Scripts)) %>%
  ungroup()

PBS %>% filter(ATC1=="B") %>% autoplot(Scripts)

PBS %>% filter(ATC1=="B") %>% autoplot(log(Scripts))

# Fit some models

fit_pbs <- PBS %>%
  model(
    ets = ETS(Scripts),
    arima = ARIMA(log(Scripts))
  )

# Produce some forecasts

fc_pbs <- forecast(fit_pbs, h="2 years")

fc_pbs
fc_pbs %>% filter(ATC2=="H02", .model=="ets")
fc_pbs %>% filter(ATC2=="H02", .model=="arima")

fc_pbs %>% filter(ATC2=="H02") %>% autoplot(PBS, level=NULL)
fc_pbs %>% filter(ATC2=="H02") %>% autoplot(PBS, level=90)
