library(tidyverse)
library(lubridate)
library(patchwork) # mozliwosc intyicujnego ustawiania wykresow (https://patchwork.data-imaginist.com/)

## Wczytać bazę danych z liczbą zachorowań na covid-19 w Polsce
## (I kolumna - data,
## II kolumna - liczba nowych zdiagnozowanych przypadków danego dnia,
## III kolumna - liczba wszystkich przypadków zdiagnozowanych, licząc od I przypadku).

lokalizacja <- getwd()

covid <- read.csv(
  paste0(lokalizacja,"/zad2/covid.csv"),
  header = TRUE,
  sep = ";")

## Uwaga, w naszych danych mamy roku, tylko dzień i miesiąc (z kontekstu wiemy, że dane dotyczą roku 2020).
## Ponadto miesiąc jest oznaczony polskim skrótem. Korzystając z biblioteki lubridate uporządkować oś czasu.

covid %>% glimpse()
# Rows: 230
# Columns: 3
# $ data           <chr> "03.mar", "04.mar", "05.mar", "06.mar", "07.mar", "08.mar", "09.mar", "10.mar", "1~
# $ nowe_przypadki <int> 0, 1, 0, 4, 1, 5, 6, 5, 9, 20, 17, 36, 21, 52, 61, 49, 68, 70, 111, 98, 115, 152, ~
# $ wszystkich     <int> 0, 1, 1, 5, 6, 11, 17, 22, 31, 51, 68, 104, 125, 177, 238, 287, 355, 425, 536, 634~

covid$data <- seq.Date(as.Date("2020-03-03"), as.Date("2020-10-18"), by = "day")
# Rows: 230
# Columns: 3
# $ data           <date> 2020-03-03, 2020-03-04, 2020-03-05, 2020-03-06, 2020-03-07, 2020-03-08, 2020-03-0~
# $ nowe_przypadki <int> 0, 1, 0, 4, 1, 5, 6, 5, 9, 20, 17, 36, 21, 52, 61, 49, 68, 70, 111, 98, 115, 152, ~
# $ wszystkich     <int> 0, 1, 1, 5, 6, 11, 17, 22, 31, 51, 68, 104, 125, 177, 238, 287, 355, 425, 536, 634~

# komenatrz: Uwaga! Działa tylko w przypadku danych ciągłych

## Narysować wykres przyrostu zachorowań w skali liniowej i logarytmiczej.
# Bez zmian
ggplot(data = covid) +
  geom_line(aes(x = data, y = nowe_przypadki)) +
  ggtitle("Przyrost zachorowań COVID (2020)")

# Skala logarytmiczna (o podstawie e)
ggplot(data = covid) +
  geom_line(aes(x = data, y = log(nowe_przypadki))) +
  labs(title = "Przyrost zachorowań COVID (2020)", caption = "Skala logarytmiczna (o podstawie e)")

# Skala logarytmiczna (o podstawie 10)
ggplot(data = covid) +
  geom_line(aes(x = data, y = log10(nowe_przypadki))) +
  labs(title = "Przyrost zachorowań COVID (2020)", caption = "Skala logarytmiczna (o podstawie 10)")

# Porównanie na jednej osi
ggplot(data = covid, aes(x = data)) +
  geom_line(aes(y = log(nowe_przypadki), colour = "log_e")) +
  geom_line(aes(y = log10(nowe_przypadki), colour = "log_10")) +
  scale_colour_manual("", values = c("log_e"="red", "log_10"="blue")) +
  labs(title = "Przyrost zachorowań COVID (2020)", subtitle = "(porównanie skal)") +
  ylab("Logarytm z liczby zachorowań")

# komentarz: mało czytelne dla laika

# Skala logarytmiczna (o podstawie 10) z poprawioną skalą
ggplot(data = covid) +
  geom_line(aes(x = data, y = nowe_przypadki)) +
  labs(title = "Procentowy przyrost zachorowań COVID (2020)", caption = "Skala logarytmiczna (o podstawie 10)") +
  ylab("%  przyrost  zachorowań") + 
  scale_y_continuous(trans='log10')

## Porównać graficznie ilość pozytywnych wyników w poniedziałek i środę.
covid <- covid %>% mutate(dzien = wday(data, week_start = getOption("lubridate.week.start", 1)))

covid_agg <- covid %>%
  # grupowanie po dniach tygodnia (srednia)
  group_by(dzien) %>%
  summarise(nowe_przypadki_srednia = mean(nowe_przypadki)) %>%
  # grupowanie po dniach tygodnia (suma) + zlaczenie tabel
  merge(covid %>%
          group_by(dzien) %>%
          summarise(nowe_przypadki_suma = sum(nowe_przypadki)),
        by = "dzien"
  ) %>%
  # przerobienie kolumny "dzien" w taki sposob, aby zmienic liczby na nazwy dni tygodnia
  # taka kolejnosc rozwiazuje problem segregowania w kolejnosci dni tygodnia
  mutate(dzien_tygodnia = ifelse(dzien + 1 <= 7,
                                 dzien + 1,
                                 1),
         dzien_tygodnia = wday(dzien_tygodnia, label = TRUE)) %>% 
  select(!dzien)

covid_agg$dzien_tygodnia <- gsub(x = covid_agg$dzien_tygodnia, pattern = "[\\.]",  "")

suma <- ggplot(data = covid_agg, aes(x = factor(dzien_tygodnia, level = c('pon', 'wt', 'śr', 'czw', 'pt', 'sob', 'niedz')))) +
  geom_point(aes(y = nowe_przypadki_suma, colour = nowe_przypadki_suma), size = 15, shape="\u2623") +
  scale_colour_gradient(low = "grey", high = "red") +
  theme(legend.position = "none") +
  ylim(c(17000, 32000)) +
  xlab("") +
  ylab("liczba zachorowań (suma)") +
  labs(title = "Porównanie zakażeń po dniach tygodnia")
srednia <- ggplot(data = covid_agg, aes(x = factor(dzien_tygodnia, level = c('pon', 'wt', 'śr', 'czw', 'pt', 'sob', 'niedz')))) +
  geom_point(aes(y = nowe_przypadki_srednia, colour = nowe_przypadki_srednia), size = 15, shape="\u2623") +
  scale_colour_gradient(low = "grey", high = "red") +
  theme(legend.position = "none") +
  ylim(c(525, 975)) +
  xlab("dzień tygodnia") +
  ylab("liczba zachorowań (średnia)") +
  labs("Porównanie dni tygodnia")

suma / srednia













