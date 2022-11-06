library(dplyr)
library(ggplot2) # rysowanie
library(ggpmisc) # wielomiany
library(purrr) # map()

## Wyczytać plik rossmann.xlsx.
rossman <- readxl::read_excel("zad4/rossmann.xlsx")

rossman <- rossman %>% mutate(across(c(6:9), ~ ifelse(.x == "Tak", 1, 0)))

rossman %>% glimpse()
# Rows: 373,855
# Columns: 12
# $ sklep_id           <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, ~
# $ dzien_tyg          <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, ~
# $ data               <chr> "2014-12-31", "2014-12-31", "2014-12-31", "2014-12-31", "2014-1~
# $ sprzedaz           <dbl> 2605, 2269, 3804, 10152, 1830, 2604, 5219, 3025, 5063, 4216, 38~
# $ liczba_klientow    <dbl> 327, 252, 408, 1311, 217, 308, 557, 394, 462, 450, 543, 609, 45~
# $ czy_otwarty        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
# $ czy_promocja       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
# $ czy_swieto         <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ~
# $ czy_swieto_szkolne <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ~
# $ sklep_typ          <chr> "c", "a", "a", "c", "a", "a", "a", "a", "a", "a", "a", "a", "a"~
# $ sklep_asort        <chr> "Podstawowy", "Podstawowy", "Podstawowy", "Rozszerzony", "Podst~
# $ sklep_konkurencja  <dbl> 1270, 570, 14130, 620, 29910, 310, 24000, 7520, 2030, 3160, 960~

rossman$sklep_id <- rossman$sklep_id %>% as.character()
rossman[c(2,6:11)] <- sapply(rossman[c(2,6:11)], as.factor)
rossman$data <- rossman$data %>% as.Date()

# select(rossman, starts_with("czy")) <- sapply(select(rossman, starts_with("czy")), as.factor) - nie zadziala
# rossman[c(6,7,8,9)] <- sapply(rossman[c(2,10,11)], function(x) {ifelse(.x == "Tak", 1, 0)})


## Dla wybranego sklepu narysować zależność między liczbą klientów, a sprzedażą.
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

## Zbadać tą zależność uwzględniając promocje.
rossman_nest <- rossman_nest %>% 
  mutate(plot_lm2 = map2(data, sklep_id,
                         ~ ggplot(data = .x, aes(x=liczba_klientow, y=sprzedaz)) +
                           stat_poly_line(formula = y + czy_promocja ~ poly(x, raw = TRUE)) +
                           stat_poly_eq(formula = y + czy_promocja~ poly(x, raw = TRUE),
                                        aes(label = after_stat(eq.label))) +
                           geom_point()))

rossman_nest$plot_lm2[1]
rossman_nest %>% filter(sklep_id == "1") %>% .$plot_lm2
































