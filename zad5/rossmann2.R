library(dplyr)
library(lubridate)
library(ggplot2) # rysowanie

lokalalizacja <- getwd()
rossman <- readxl::read_excel(paste0(lokalalizacja,"/zad5/rossmann.xlsx"))
rossman <- rossman %>% mutate(across(c(6:9), ~ ifelse(.x == "Tak", 1, 0)),
                              dzien_tyg = wday(dzien_tyg+1, label = TRUE))
rossman$dzien_tyg <- gsub(x = rossman$dzien_tyg, pattern = "[\\.]",  "")
rossman$sklep_id <- rossman$sklep_id %>% as.character()
rossman[c(6:11)] <- sapply(rossman[c(6:11)], as.factor)
rossman$data <- rossman$data %>% as.Date()
# nadajemy hierachie dniami tygodnia
rossman <- rossman %>% mutate(across(dzien_tyg, factor, levels = c("pon", "wt", "śr", "czw", "pt", "sob", "niedz"))) %>% filter(dzien_tyg != "niedz")

rossman %>% glimpse()
# Rows: 320,555
# Columns: 12
# $ sklep_id           <chr> "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "14", "15", "16", "17…
# $ dzien_tyg          <fct> śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, śr, …
# $ data               <date> 2014-12-31, 2014-12-31, 2014-12-31, 2014-12-31, 2014-12-31, 2014-12-31, 2014-12-31,…
# $ sprzedaz           <dbl> 2605, 2269, 3804, 10152, 1830, 2604, 5219, 3025, 5063, 4216, 3838, 4871, 3939, 4291,…
# $ liczba_klientow    <dbl> 327, 252, 408, 1311, 217, 308, 557, 394, 462, 450, 543, 609, 453, 476, 473, 367, 492…
# $ czy_otwarty        <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",…
# $ czy_promocja       <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",…
# $ czy_swieto         <chr> "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",…
# $ czy_swieto_szkolne <chr> "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",…
# $ sklep_typ          <chr> "c", "a", "a", "c", "a", "a", "a", "a", "a", "a", "a", "a", "a", "d", "a", "a", "d",…
# $ sklep_asort        <chr> "Podstawowy", "Podstawowy", "Podstawowy", "Rozszerzony", "Podstawowy", "Podstawowy",…
# $ sklep_konkurencja  <dbl> 1270, 570, 14130, 620, 29910, 310, 24000, 7520, 2030, 3160, 960, 1070, 1300, 4110, 3…

# Narysować wykresy pokazujące liczbę sklepów każdego z typów: a,b,c,... i rodzaj asortymentu: podstawowy, rozszerzony i ekstra.
# Polecenie wykonać zarówno za pomocą polecenia *geom_bar* jak i *geom_col*.
sklepy <- rossman %>%
  select(sklep_typ, sklep_asort) %>% 
  group_by(sklep_typ, sklep_asort) %>% 
  summarize(n = n()) %>% 
  ungroup()

ggplot(sklepy, aes(x= sklep_typ, y= n, fill= sklep_asort)) +
  geom_bar(stat = "identity", position= "dodge") +
  labs(title= "Liczba sklepów w podziale na typ i asortyment",
       y= "",
       x= "Typ sklepu",
       fill='Asortyment') +
  geom_text(aes(label= n),
            size= 3,
            position = position_dodge(0.9),
            vjust = -1)

ggplot(sklepy, aes(x= sklep_typ, y= n, fill= sklep_asort)) +
  geom_col(position= "dodge") +
  labs(title= "Liczba sklepów w podziale na typ i asortyment",
       y= "",
       x= "Typ sklepu",
       fill='Asortyment') +
  geom_text(aes(label= n),
            size= 3,
            position = position_dodge(0.9),
            vjust = -1)

