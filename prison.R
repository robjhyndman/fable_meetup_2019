library(tidyverse)
library(lubridate)
library(tsibble)
library(feasts)
library(fable)

prison <- readr::read_csv("prison_population.csv")

prison <- prison %>%
  mutate(
    quarter = yearquarter(date),
    state = recode(state,
      `1` = "NSW",
      `2` = "VIC",
      `3` = "QLD",
      `4` = "SA",
      `5` = "WA",
      `6` = "TAS",
      `7` = "NT",
      `8` = "ACT"
    ),
    sex = recode(sex, `1` = "Male", `2` = "Female"),
    legal = recode(legal,
      `2` = "Remanded",
      `3` = "Sentenced",
      `4` = "Sentenced"
    ),
    indigenous = recode(indigenous,
      `1` = "ATSI",
      `2` = "Non-ATSI",
      `3` = "Non-ATSI"
    ),
  ) %>%
  group_by(state, sex, legal, indigenous, quarter) %>%
  summarise(count = sum(count, na.rm = TRUE)) %>%
  ungroup() %>%
  as_tsibble(key = c(state, sex, legal, indigenous), index = quarter)

# Combine into state and gender categories only
prison <- prison %>%
  group_by(sex, state) %>%
  summarise(count = sum(count)) %>%
  ungroup()

prison %>% autoplot(count)

# Fit some models

fit_prison <- prison %>%
  model(
    ets = ETS(count),
    arima = ARIMA(log(count))
  )

# Produce some forecasts
fc_prison <- forecast(fit_prison, h="2 years")

fc_prison
fc_prison %>% filter(state=="NSW", sex=="Male", .model=='arima')

fc_prison %>% filter(state=="NSW") %>% autoplot(prison, level=90)

