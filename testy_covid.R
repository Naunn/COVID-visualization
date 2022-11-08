source("dane.R")

# Temat: "Porównanie wydolności opieki zdrowia państw w obliczu pandemii"
# Pomysł: na początku wybierać z mapy świata państwa do porównania
# (zeby pojawialo sie ile jest danych dla tych krajow)
# Potem one pojawiają się na wykresie testów, zachorowan, smierci, itp
# Finalnie i tak tabela z rankigiem top i worst 5 wydolnosci sluzby zdrowia

library(dplyr)
library(plotly)
library(tidyr)

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
  select(week,
         ISO.code,
         X7.day.smoothed.daily.change,
         X7.day.smoothed.daily.change.per.thousand,
         Cumulative.total,
         Short.term.positive.rate) %>%
  group_by(ISO.code, week) %>%
  summarize(suma_testow_tyg = sum(X7.day.smoothed.daily.change, na.rm = TRUE),
            avg_test_tyg_1000 = round(mean(X7.day.smoothed.daily.change.per.thousand, na.rm = TRUE),4),
            cumulative_per_week = sum(Cumulative.total, na.rm = TRUE),
            Short.term.positive.rate = round(mean(Short.term.positive.rate, na.rm = TRUE),4) %>%
              format(9999, scientific = FALSE)) %>% # Usuniecie zapisu naukowego, tj. z "e"
  # jeszcze trzeba usunac "NaN'-y ze sredniej
  filter(cumulative_per_week > 0) %>% 
  inner_join(covid_testing_count %>% filter(Freq > 90), by = c("ISO.code" = ".")) %>% 
  select(!Freq) %>% 
  mutate(Short.term.positive.rate = as.numeric(Short.term.positive.rate)) %>% 
  replace(is.na(.), 0)

# short.term.positive.rate - 100 x Liczba nowych potwierdzonych przypadków/liczba testów wykonanych w tygodniu

kraj = "POL"
baza <- covid_testing_agg %>% filter(ISO.code == kraj)
kor <- cor.test(baza$suma_testow_tyg, baza$Short.term.positive.rate, method=c("pearson", "kendall", "spearman"))
ifelse(kor$p.value < 0.05,
       paste("Korelacja pomiędzy zmiennymi jest statystycznie istotna i wynosi:",round(kor$estimate,2)),
       paste("Korelacja pomiędzy zmiennymi jest statystycznie nieistotna i wynosi:",round(kor$estimate,2)))

fig1 <- plot_ly(baza,
              x = ~week,
              y = ~suma_testow_tyg,
              name = ~ISO.code,
              type = 'scatter',
              mode = 'lines+markers')
fig2 <- plot_ly(baza,
                x = ~week,
                y = ~Short.term.positive.rate,
                name = ~ISO.code,
                type = 'scatter',
                mode = 'lines+markers')
fig <- subplot(fig1, fig2, nrows = 2)
fig
# pomysl: wyswietlanie porownania lacznej ilosc testow w tygodniu + % pozytywnych wynikow
# + wniosek z testu korelacji tych zmiennych (wszystko dla wskazanego panstwa)















