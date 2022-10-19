source("dane.R")

# Temat: "Porównanie wydolności opieki zdrowia państw w obliczu pandemii"
# Pomysł: na początku wybierać z mapy świata państwa do porównania
# (zeby pojawialo sie ile jest danych dla tych krajow)
# Potem one pojawiają się na wykresie testów, zachorowan, smierci, itp
# Finalnie i tak tabela z rankigiem top i worst 5 wydolnosci sluzby zdrowia

library(dplyr)
library(plotly)

## Analiza testowania
covid_testing_all_observations %>%
  filter(ISO.code == "POL") %>%
  select(c("Date", "X7.day.smoothed.daily.change")) %>%
  plot(type = 'l')

covid_testing_all_observations %>% filter(ISO.code == "POL") %>% View()

# Agregacja przypadków do tygodni (jeżeli NA, to srednia z daty przed i daty po)
covid_testing_all_observations$week <- lubridate::ceiling_date(covid_testing_all_observations$Date,
                                                               "week",
                                                               week_start = getOption("lubridate.week.start", 1),
                                                               change_on_boundary = TRUE)

# Podglad ilosci danych/tygodni (niezerowych)
covid_testing_all_observations %>%
  select(week, ISO.code, Cumulative.total) %>% 
  group_by(ISO.code, week) %>%
  summarize(sum = sum(Cumulative.total, na.rm = TRUE)) %>%
  filter(sum > 0) %>% 
  .$ISO.code %>%
  table() %>%
  as.data.frame() %>% View()

# Uspójnienie osi (mozna teraz łatwo rysować wartosci)
covid_testing_agg <- covid_testing_all_observations %>%
  select(week, ISO.code, Cumulative.total) %>% 
  group_by(ISO.code, week) %>%
  summarize(sum = sum(Cumulative.total, na.rm = TRUE))

fig <- plot_ly(covid_testing_agg,
              x = ~week,
              y = ~sum,
              name = ~ISO.code,
              type = 'scatter',
              mode = 'lines+markers')
fig


# zrobic cumulative na tysiac (wtedy wyeliminujemy problem rozmiaru panstwa)
# dodac positive rate