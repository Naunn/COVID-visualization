library(dplyr)
library(ggplot2) # rysowanie
library(ggpmisc) # wielomiany
library(purrr) # map()
library(tidyr)
library(lubridate)
library(plotly)

## Wyczytać plik rossmann.xlsx. ----
lokalalizacja <- getwd()
rossman <- readxl::read_excel(paste0(lokalalizacja,"/zad4/rossmann.xlsx"))
rossman <- rossman %>% mutate(across(c(6:9), ~ ifelse(.x == "Tak", 1, 0)),
                              dzien_tyg = wday(dzien_tyg+1, label = TRUE))
rossman$dzien_tyg <- gsub(x = rossman$dzien_tyg, pattern = "[\\.]",  "")
rossman$sklep_id <- rossman$sklep_id %>% as.character()
rossman[c(6:11)] <- sapply(rossman[c(6:11)], as.factor)
rossman$data <- rossman$data %>% as.Date()
# nadajemy hierachie dniami tygodnia
rossman <- rossman %>% mutate(across(dzien_tyg, factor, levels = c("pon", "wt", "śr", "czw", "pt", "sob", "niedz"))) %>% filter(dzien_tyg != "niedz")

rossman %>% glimpse()
# Rows: 373,855
# Columns: 12
# $ sklep_id           <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "14", "15…
# $ dzien_tyg          <fct> śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, …
# $ data               <date> 2014-12-31, 2014-12-31, 2014-12-31, 2014-12-31, 2014-12-31, 2014-12-31,…
# $ sprzedaz           <dbl> 2605, 2269, 3804, 10152, 1830, 2604, 5219, 3025, 5063, 4216, 3838, 4871,…
# $ liczba_klientow    <dbl> 327, 252, 408, 1311, 217, 308, 557, 394, 462, 450, 543, 609, 453, 476, 4…
# $ czy_otwarty        <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1…
# $ czy_promocja       <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0…
# $ czy_swieto         <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0…
# $ czy_swieto_szkolne <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1…
# $ sklep_typ          <chr> "c", "a", "a", "c", "a", "a", "a", "a", "a", "a", "a", "a", "a", "d", "a…
# $ sklep_asort        <chr> "Podstawowy", "Podstawowy", "Podstawowy", "Rozszerzony", "Podstawowy", "…
# $ sklep_konkurencja  <dbl> 1270, 570, 14130, 620, 29910, 310, 24000, 7520, 2030, 3160, 960, 1070, 1…


# select(rossman, starts_with("czy")) <- sapply(select(rossman, starts_with("czy")), as.factor) - nie zadziala
# rossman[c(6,7,8,9)] <- sapply(rossman[c(2,10,11)], function(x) {ifelse(.x == "Tak", 1, 0)})


## Dla wybranego sklepu narysować zależność między liczbą klientów, a sprzedażą. ----
# zabawa z zagniezdzaniem

# rossman_nest <- rossman %>% nest_by(sklep_id) 
# rossman_nest[4,][[2]]
# rossman_nest <- rossman_nest %>% mutate(model = list(lm(sprzedaz ~ liczba_klientow, data = data)))
# rossman_nest %>% filter(sklep_id == "1") %>% .$data
# rossman_nest %>% filter(sklep_id == "1") %>% .$model

rossman_nest <- rossman %>% 
  group_by(sklep_id) %>% 
  nest()

rossman_nest <- rossman_nest %>% 
  mutate(plot_lm1 = map2(data, sklep_id,
                     ~ ggplot(data = .x, aes(x=liczba_klientow, y=sprzedaz)) +
                       stat_poly_line(formula = y ~ poly(x, raw = TRUE)) +
                       stat_poly_eq(formula = y ~ poly(x, raw = TRUE),
                                    aes(label = after_stat(eq.label))) +
                       geom_point()))

rossman_nest$plot_lm1[1]
rossman_nest %>% filter(sklep_id == "1") %>% .$plot_lm1

## Zbadać tą zależność uwzględniając promocje. ----
rossman_nest <- rossman_nest %>% 
  mutate(plot_lm2 = map2(data, sklep_id,
                         ~ ggplot(data = .x, aes(x=liczba_klientow, y=sprzedaz, color = czy_promocja)) +
                           stat_poly_line(formula = y ~ poly(x, raw = TRUE)) +
                           stat_poly_eq(formula = y ~ poly(x, raw = TRUE),
                                        aes(label = after_stat(eq.label))) +
                           guides(col = guide_legend(reverse = TRUE)) +
                           geom_point()))

rossman_nest$plot_lm2[1]
rossman_nest %>% filter(sklep_id == "1") %>% .$plot_lm2

## Zbadać tą zależność liczba klientów - sprzedaż, uwzględniając dzień tygodnia. ----
# rossman_nest <- rossman_nest %>% 
#   mutate(plot_lm3 = map2(data, sklep_id,
#                          ~ ggplot(data = .x, aes(x=liczba_klientow, y=sprzedaz, color = dzien_tyg)) +
#                            stat_poly_line(formula = y ~ poly(x, raw = TRUE)) +
#                            stat_poly_eq(formula = y ~ poly(x, raw = TRUE),
#                                         aes(label = after_stat(eq.label))) +
#                            geom_point()))
# 
# rossman_nest$plot_lm3[1]
# rossman_nest %>% filter(sklep_id == "1") %>% .$plot_lm3
# wszystko sie zlewa

rossman_nest <- rossman_nest %>% 
  mutate(plot_lm3 = map2(data, sklep_id,
                         ~ ggplot(data = .x %>% filter(dzien_tyg != "niedz"), aes(x=liczba_klientow, y=sprzedaz, color = dzien_tyg)) +
                           stat_poly_line(formula = y ~ poly(x, raw = TRUE)) +
                           stat_poly_eq(formula = y ~ poly(x, raw = TRUE),
                                        aes(label = after_stat(eq.label))) +
                           geom_point() +
                           theme(legend.position="none") +
                           facet_wrap(~dzien_tyg)))

rossman_nest$plot_lm3[1]
rossman_nest %>% filter(sklep_id == "1") %>% .$plot_lm3

## Sprawdzić czy zależność wielkości sprzedaży od dnia tygodnia jest lepiej widoczna jeżeli na osi x zamiast liczby klientów umieścimy czas (oś x - Data). ----
rossman_nest <- rossman_nest %>% 
  mutate(plot_lm4 = map2(data, sklep_id,
                         ~ plot_ly(data = .x, #%>% filter(dzien_tyg != "niedz"),
                                   x= ~data,
                                   y= ~sprzedaz,
                                   color = ~dzien_tyg,
                                   type = "bar") %>% layout(
                                     title = "Sprzedaz na przestrzeni 2014r.") %>% 
                           rangeslider()))

rossman_nest$plot_lm4[1]
rossman_nest %>% filter(sklep_id == "1") %>% .$plot_lm4

## (*) Wybrać pięć sklepów i porównać sprzedaż w tych sklepach w kolejnych dniach (oś x - czas). ----
rossman %>%
  filter(dzien_tyg != "niedz" &
           sklep_id %in% sample(rossman$sklep_id %>% unique(), size = 5, replace = FALSE)) %>% 
plot_ly(x= ~data,
        y= ~sprzedaz,
        color = ~sklep_id,
        type = "bar") %>%
  layout(title = "Porwnanie losowych 5-ciu sklepów") %>% 
  rangeslider()
  


























