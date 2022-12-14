library(dplyr)
library(knitr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(gganimate)
library(RColorBrewer)
library(ggflags)
library(countrycode) # for ISO2 country codes

# Zadanie - Przy pomocy animacji zwizualizować dane dotyczące ceny kanapki w wybranych krajach oraz średniej ceny kanapki.

## Dane ================================================================================================================
# https://www.kaggle.com/datasets/mrmorj/big-mac-index-data
bigmac <- read.csv2("zad7/BigmacPrice_t.csv", na.strings = "")
bigmac %>% head(10) %>% kable()

bigmac %>% glimpse()
# Rows: 1,946
# Columns: 6
# $ date          <chr> "01.04.2000", "01.04.2000", "01.04.2000", "01.04.2000", "01.04.2000~
# $ currency_code <chr> "ARS", "AUD", "BRL", "GBP", "CAD", "CLP", "CNY", NA, "DKK", "EUR", ~
# $ name          <chr> "Argentina", "Australia", "Brazil", "Britain", "Canada", "Chile", "~
# $ local_price   <dbl> 2.50, 2.59, 2.95, 1.90, 2.85, 1260.00, 9.90, 54.37, 24.75, 2.56, 10~
# $ dollar_ex     <int> 1, 1, 1, 1, 1, 514, 8, 39, 8, 1, 7, 279, 7945, 4, 106, 3, 9, 2, 4, ~
# $ dollar_price  <chr> "$2,5", "$2,59", "$2,95", "$1,9", "$2,85", "$2,45", "$1,24", "$1,39~

# local_price - cena Big Maca w lokalnej walucie
# dollar_ex - jednostki waluty lokalnej na dolara (kurs dolara)
# dollar_price - cena Big Maca w dolarach

## Przygotowanie Danych ================================================================================================

# Uzupełnienie currency_code
bigmac$currency_code %>% table() %>% kable()
is.na(bigmac$currency_code) %>% table() %>% kable()

bigmac$name %>% table() %>% kable()
is.na(bigmac$name) %>% table() %>% kable()

# Wenezuela ma dwie waluty - problem ze złączeniem wiele do wielu
bigmac_clean <- bigmac %>%
  left_join((
    bigmac %>% select(currency_code, name) %>% drop_na() %>% filter(name != "Venezuela") %>% distinct()
  ),
  by = "name") %>%
  mutate(currency_code = coalesce(currency_code.x, currency_code.y),
         .keep = "unused")

# bigmac_clean$currency_code %>% table() %>% as.data.frame() %>%
#   left_join(bigmac$currency_code %>% table() %>% as.data.frame(), by = ".") %>%
#   mutate(tst = Freq.x == Freq.y) %>%
#   view()

# Zmiana zapisu naukowego w local_price
# to rozwiazanie zamienia wartosci dbl w chr - problem
# bigmac_clean <- bigmac_clean %>%
#   mutate(local_price = format(round(local_price, 2), scientific = FALSE, big.mark = " "))
options(scipen = 999)
bigmac_clean <- bigmac_clean %>%
  mutate(local_price = round(local_price, 2))

# Formatowanie dat
bigmac_clean <- bigmac_clean %>%
  mutate(date = as.Date(date, tryFormats = c("%d.%m.%Y")))

# Uzupełnienie dat
bigmac_clean <- bigmac_clean %>% fill(date, .direction = "updown")
bigmac_clean$date %>% is.na() %>% table() %>% kable()

# Konfiguracja dollar_price
bigmac_clean <- bigmac_clean %>%
  mutate(dollar_price = str_remove(str_replace(dollar_price, ",", "."), "\\$") %>% as.numeric())

# "Uciąglenie" dollar_price
bigmac_clean$dollar_price %>% is.na() %>% table()

bigmac_clean <- bigmac_clean %>%
  group_by(name) %>%
  # wewnątrz grupy, jeżeli NA, to wez srednia z poprzedniej i kolejnej wartosci
  mutate(dollar_price = coalesce(dollar_price,
                                 (lag(dollar_price) + lead(dollar_price)) /
                                   2)) %>%
  ungroup() %>%
  # reszte wypelnil pierwsza (kolejna) napotkana wartoscia
  fill(dollar_price, .direction = "up")

## Wizualizacja ========================================================================================================
# Britain, Brazil, United States, South Korea, Russia, South Africa, China, Poland
# Wszystkie, gdzie min. 30 pozycji
df <- bigmac_clean %>%
  # filter(name %in% c("Britain",
  #                    "Brazil",
  #                    "United States",
  #                    "South Korea",
  #                    "Russia",
  #                    "South Africa",
  #                    "China",
  #                    "Poland")) %>%
  group_by(name) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count >= 35 & name != "Euro area") %>%
  mutate(iso2c = tolower(countrycode(name, "country.name", "iso2c")))

df %>% select(name) %>% table() %>% kable()

animation <- ggplot(df,  aes(
  x = dollar_price,
  y = name,
  country = iso2c,
  label = sprintf("%0.2f", round(local_price, digits = 2))
)) +
  geom_bar(stat = "identity") +
  geom_text(hjust = 1.5,
            color = "white",
            size = 3.5) +
  geom_flag(aes(country = iso2c), size = 7) +
  theme(legend.position = "none", plot.title = element_text(size = 25)) +
  transition_time(date) +
  labs(title = "{frame_time}", x = "Big Mac (local price) dollar price", y = "")

animate(animation, duration = 30, fps = 10)

