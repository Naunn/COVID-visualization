source("dane.R")

covid_testing_all_observations %>%
  filter(ISO.code == "POL") %>%
  select(c("Date", "X7.day.smoothed.daily.change")) %>%
  plot(type = 'l')







