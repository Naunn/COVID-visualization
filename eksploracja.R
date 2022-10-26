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
covid_testing_count <- covid_testing_all_observations %>%
  select(week, ISO.code, Cumulative.total) %>%
  group_by(ISO.code, week) %>%
  summarize(sum = sum(Cumulative.total, na.rm = TRUE)) %>%
  filter(sum > 0) %>%
  .$ISO.code %>%
  table() %>%
  as.data.frame()

covid_testing_count %>% filter(Freq > 90) # bieżemy pod uwagę kraje, w których pojawiają się 3 miesiące danych

# Uspójnienie osi (mozna teraz łatwo rysować wartosci)
covid_testing_agg <- covid_testing_all_observations %>%
  select(week, ISO.code, Cumulative.total, Short.term.positive.rate) %>%
  group_by(ISO.code, week) %>%
  summarize(cumulative_per_week = sum(Cumulative.total, na.rm = TRUE),
            Short.term.positive.rate = mean(Short.term.positive.rate, na.rm = TRUE) %>%
              format(9999, scientific = FALSE)) %>% # Usuniecie zapisu naukowego, tj. z "e"
  # jeszcze trzeba usunac "NaN'-y ze sredniej
  filter(cumulative_per_week > 0) %>% 
  inner_join(covid_testing_count %>% filter(Freq > 90), by = c("ISO.code" = ".")) %>% 
  select(!Freq)

fig <- plot_ly(covid_testing_agg,
              x = ~week,
              y = ~sum,
              name = ~ISO.code,
              type = 'scatter',
              mode = 'lines+markers')
fig


# zrobic cumulative na tysiac (wtedy wyeliminujemy problem rozmiaru panstwa) - troche sie to nie klei
# jakby podzielic ilosc testow przez populacje danego panstwa? i porownac ilosc testow do ludnosci wraz ze (wstepnie) pozytywnym wynikiem 
# dodac positive rate

















