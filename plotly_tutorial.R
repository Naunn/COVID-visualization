# przetwarzanie
library(dplyr)
# wizualizacja
library(plotly)
library(ggplot2)

# Wstęp ======================================================================================================
## Linki =====================================================================================================
# https://plotly.com/r/
# https://plotly-r.com/index.html - świetna książka (FREE, jak zresztą wszystko o R-rze godne uwagi)
# https://chart-studio.plotly.com/feed/ - ciekawostka

# Istnieją dwa główne sposoby tworzenia obiektów plotly: albo przez przekształcenie obiektu ggplot2
# (poprzez ggplotly()) w obiekt plotly, albo przez bezpośrednią inicjalizację obiektu plotly za pomocą
# plot_ly()/plot_geo()/plot_mapbox(). Oba podejścia mają nieco uzupełniające się mocne i słabe strony,
# więc może opłacić się nauczyć obu podejść. Co więcej, oba podejścia są implementacją Gramatyki Grafiki i
# oba są zasilane przez bibliotekę graficzną JavaScript plotly.js, więc wiele z tych samych koncepcji
# i narzędzi, których nauczysz się dla jednego interfejsu, może być ponownie wykorzystanych w drugim.

## Dane ======================================================================================================
data(midwest) # Midwest demographics - https://ggplot2.tidyverse.org/reference/midwest.html
midwest %>% colnames()
# [1] "PID"                  "county"               "state"               
# [4] "area"                 "poptotal"             "popdensity"          
# [7] "popwhite"             "popblack"             "popamerindian"       
# [10] "popasian"             "popother"             "percwhite"           
# [13] "percblack"            "percamerindan"        "percasian"           
# [16] "percother"            "popadults"            "perchsd"             
# [19] "percollege"           "percprof"             "poppovertyknown"     
# [22] "percpovertyknown"     "percbelowpoverty"     "percchildbelowpovert"
# [25] "percadultpoverty"     "percelderlypoverty"   "inmetro"             
# [28] "category"  
midwest %>% glimpse()
# Rows: 437
# Columns: 28
# $ PID                  <int> 561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571, 572, 573, 574, 575, 576…
# $ county               <chr> "ADAMS", "ALEXANDER", "BOND", "BOONE", "BROWN", "BUREAU", "CALHOUN", "CARROLL"…
# $ state                <chr> "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", "IL", …
# $ area                 <dbl> 0.052, 0.014, 0.022, 0.017, 0.018, 0.050, 0.017, 0.027, 0.024, 0.058, 0.042, 0…
# $ poptotal             <int> 66090, 10626, 14991, 30806, 5836, 35688, 5322, 16805, 13437, 173025, 34418, 15…
# $ popdensity           <dbl> 1270.9615, 759.0000, 681.4091, 1812.1176, 324.2222, 713.7600, 313.0588, 622.40…
# $ popwhite             <int> 63917, 7054, 14477, 29344, 5264, 35157, 5298, 16519, 13384, 146506, 34176, 158…
# $ popblack             <int> 1702, 3496, 429, 127, 547, 50, 1, 111, 16, 16559, 82, 10, 4, 1021, 925, 131714…
# $ popamerindian        <int> 98, 19, 35, 46, 14, 65, 8, 30, 8, 331, 51, 26, 17, 48, 92, 10289, 34, 6, 123, …
# $ popasian             <int> 249, 48, 16, 150, 5, 195, 15, 61, 23, 8033, 89, 36, 29, 104, 341, 188565, 48, …
# $ popother             <int> 124, 9, 34, 1139, 6, 221, 0, 84, 6, 1596, 20, 7, 7, 83, 109, 384119, 19, 6, 10…
# $ percwhite            <dbl> 96.71206, 66.38434, 96.57128, 95.25417, 90.19877, 98.51210, 99.54904, 98.29813…
# $ percblack            <dbl> 2.57527614, 32.90043290, 2.86171703, 0.41225735, 9.37285812, 0.14010312, 0.018…
# $ percamerindan        <dbl> 0.14828264, 0.17880670, 0.23347342, 0.14932156, 0.23989034, 0.18213405, 0.1503…
# $ percasian            <dbl> 0.37675897, 0.45172219, 0.10673071, 0.48691813, 0.08567512, 0.54640215, 0.2818…
# $ percother            <dbl> 0.18762294, 0.08469791, 0.22680275, 3.69733169, 0.10281014, 0.61925577, 0.0000…
# $ popadults            <int> 43298, 6724, 9669, 19272, 3979, 23444, 3583, 11323, 8825, 95971, 22945, 10734,…
# $ perchsd              <dbl> 75.10740, 59.72635, 69.33499, 75.47219, 68.86152, 76.62941, 62.82445, 75.95160…
# $ percollege           <dbl> 19.63139, 11.24331, 17.03382, 17.27895, 14.47600, 18.90462, 11.91739, 16.19712…
# $ percprof             <dbl> 4.355859, 2.870315, 4.488572, 4.197800, 3.367680, 3.275891, 3.209601, 3.055727…
# $ poppovertyknown      <int> 63628, 10529, 14235, 30337, 4815, 35107, 5241, 16455, 13081, 154934, 33788, 15…
# $ percpovertyknown     <dbl> 96.27478, 99.08714, 94.95697, 98.47757, 82.50514, 98.37200, 98.47802, 97.91729…
# $ percbelowpoverty     <dbl> 13.151443, 32.244278, 12.068844, 7.209019, 13.520249, 10.399635, 15.149781, 11…
# $ percchildbelowpovert <dbl> 18.011717, 45.826514, 14.036061, 11.179536, 13.022889, 14.158819, 13.787761, 1…
# $ percadultpoverty     <dbl> 11.009776, 27.385647, 10.852090, 5.536013, 11.143211, 8.179287, 12.932331, 10.…
# $ percelderlypoverty   <dbl> 12.443812, 25.228976, 12.697410, 6.217047, 19.200000, 11.008586, 21.085271, 9.…
# $ inmetro              <int> 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, …
# $ category             <chr> "AAR", "LHR", "AAR", "ALU", "AAR", "AAR", "LAR", "AAR", "AAR", "HAU", "AAR", "…

# Trzeba tak, bo nazwy stanów są unikatowe, a hrabst, niekoniecznie
midwest_agg <- midwest %>% 
  select(c(state,
           percpovertyknown,
           percbelowpoverty,
           percchildbelowpovert,
           percadultpoverty,
           percelderlypoverty)) %>% 
  group_by(state) %>% 
  mutate(percpovertyknown = mean(percpovertyknown),
         percbelowpoverty = mean(percbelowpoverty),
         percchildbelowpovert = mean(percchildbelowpovert),
         percadultpoverty = mean(percadultpoverty),
         percelderlypoverty = mean(percelderlypoverty)) %>% 
  ungroup() %>% 
  distinct()

# percpovertyknown - Percent of population with known poverty status.
# percbelowpoverty - Percent of people below poverty line.
# percchildbelowpovert - Percent of children below poverty line.
# percadultpoverty - Percent of adults below poverty line.
# percelderlypoverty - Percent of elderly below poverty line.

# Zaczynamy! =================================================================================================
# Odsetek osób z wyższym wykształceniem według lokalizacji (percollege - Percent college educated).
fig <- plot_ly(midwest,
               x = ~percollege,
               color = ~state,
               type = "violin") 
fig

## Początkowy wykres =========================================================================================
# Podpowiedzi od plot_ly 
fig <- plot_ly(midwest,
               x = ~percollege,
               color = ~state,
               type = "viollin")
fig

fig <- plot_ly(midwest_agg,
               x = ~state,
               y = ~percpovertyknown,
               type = "scatter",
               name = "% poverty known") %>% 
  add_trace(y = ~percbelowpoverty,
            name = "% below poverty line")
# fig <- fig %>% add_trace(y = ~percbelowpoverty) # 2 sposób
fig
# Mało czytelne, może rozdzielimy?
fig1 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percpovertyknown,
                type = "scatter",
                name = "% poverty known",
                mode = "markers") # "lines", "markers", "lines+markers"
fig2 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percbelowpoverty,
                type = "scatter",
                name = "% below poverty line",
                mode = "markers") # "lines", "markers", "lines+markers"
fig <- subplot(fig1, fig2, nrows = 2)
fig
# Lepiej, ale bez szału - czas na layout()

## Layout() ==================================================================================================
# Uwaga! Layout() określa konfiguracje wyświetlania figury. Jeżeli Łączymy figury, i chcemy osobny wygląd
# należy każdej z figur zastosować wybraną konfigurację. W innym przypadku, layout() jest automatyczny.
# Przykład:

# Modyfikacja osi, dodajmy % na osiach
fig <- fig %>% layout(xaxis = list(showticklabels = FALSE), # z automatu działa tak jakbyśmy chcieli
                      yaxis = list(tickformat = ".0%")) # ...
fig
# Jakie są dwa problemy? Naprawmy to!
fig1 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percpovertyknown/100,
                type = "scatter",
                name = "% poverty known",
                mode = "markers") %>% 
  layout(xaxis = list(showticklabels = FALSE), # z automatu działa tak jakbyśmy chcieli
         yaxis = list(tickformat = ".0%"))
fig2 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percbelowpoverty/100,
                type = "scatter",
                name = "% below poverty line",
                mode = "markers") %>% 
  layout(yaxis = list(tickformat = ".0%"))
fig <- 
fig
# Lepiej ...
# Tytuł
fig <- fig %>% layout(title = "% of poverty by categories")
fig
# Dodajmy procentów
fig2 <- fig2 %>%
  add_trace(y = ~percchildbelowpovert/100,
            name = "% below poverty line (chlidren)") %>% 
  add_trace(y = ~percadultpoverty/100,
            name = "% below poverty line (adults)") %>% 
  add_trace(y = ~percelderlypoverty/100,
            name = "% below poverty line (eldery)")
fig
# Dlaczego się nie zmieniło?
fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = "% of poverty by categories")
fig
# Czas na kolory!

## Kolory ====================================================================================================
# Dyksretne - https://plotly.com/r/discrete-color/
# Ciagłe - https://plotly.com/r/colorscales/
# Wbudowane palety koloróW - https://plotly.com/r/builtin-colorscales/

library(RColorBrewer)
brewer.pal.info
#          maxcolors category colorblind
# BrBG            11      div       TRUE
# PiYG            11      div       TRUE
# PRGn            11      div       TRUE
# PuOr            11      div       TRUE
# RdBu            11      div       TRUE
# RdGy            11      div      FALSE
# RdYlBu          11      div       TRUE
# RdYlGn          11      div      FALSE
# Spectral        11      div      FALSE
# Accent           8     qual      FALSE
# Dark2            8     qual       TRUE
# Paired          12     qual       TRUE
# Pastel1          9     qual      FALSE
# Pastel2          8     qual      FALSE
# Set1             9     qual      FALSE
# Set2             8     qual       TRUE
# Set3            12     qual      FALSE
# Blues            9      seq       TRUE
# BuGn             9      seq       TRUE
# BuPu             9      seq       TRUE
# GnBu             9      seq       TRUE
# Greens           9      seq       TRUE
# Greys            9      seq       TRUE
# Oranges          9      seq       TRUE
# OrRd             9      seq       TRUE
# PuBu             9      seq       TRUE
# PuBuGn           9      seq       TRUE
# PuRd             9      seq       TRUE
# Purples          9      seq       TRUE
# RdPu             9      seq       TRUE
# Reds             9      seq       TRUE
# YlGn             9      seq       TRUE
# YlGnBu           9      seq       TRUE
# YlOrBr           9      seq       TRUE
# YlOrRd           9      seq       TRUE

# Zacznijmy od pociemnienia tła (w tym przypadku wyraziste osie będą zalecane)
fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = "% of poverty by categories",
         plot_bgcolor='#e5ecf6') # zapis Hex rgb
fig

# Ciekawostka ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# https://www.w3.org/TR/css-color-3/#svg-color
# Dodatkowy trik od R-a dla kolorów (różne zapisy koloru niebieskiego):
image(1, 1, as.matrix(1), col="blue")

image(1, 1, as.matrix(1), col=rgb(0,0,255, maxColorValue = 255))

rgb(0,0,255, maxColorValue = 255)
# [1] "#0000FF"

image(1, 1, as.matrix(1), col="#0000FF")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Uspójnijmy dotychczasowy wykres
fig1 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percpovertyknown/100,
                type = "scatter",
                name = "% poverty known",
                mode = "markers") %>% 
  layout(xaxis = list(showticklabels = FALSE),
         yaxis = list(tickformat = ".0%"))

fig2 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percbelowpoverty/100,
                type = "scatter",
                name = "% below poverty line",
                mode = "markers") %>%
  # Uwaga! Layout nalezy umieszczac po glownym skladniu, a nastepnie dodawac kolejne punkty.
  layout(yaxis = list(tickformat = ".0%")) %>%
  add_trace(y = ~percchildbelowpovert/100,
            name = "% below poverty line (chlidren)") %>% 
  add_trace(y = ~percadultpoverty/100,
            name = "% below poverty line (adults)") %>% 
  add_trace(y = ~percelderlypoverty/100,
            name = "% below poverty line (eldery)")

fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = "% of poverty by categories",
         plot_bgcolor='#e5ecf6')
fig

# Co jakbyśmy chcieli nadać kolory po "state"?
fig1 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percpovertyknown/100,
                type = "scatter",
                name = "% poverty known",
                mode = "markers") %>% 
  layout(xaxis = list(showticklabels = FALSE),
         yaxis = list(tickformat = ".0%"))

fig2 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percbelowpoverty/100,
                type = "scatter",
                name = "% below poverty line",
                mode = "markers",
                color = ~state,
                colors = "BrBG") %>%
  layout(yaxis = list(tickformat = ".0%")) %>%
  add_trace(y = ~percchildbelowpovert/100,
            name = "% below poverty line (chlidren)") %>% 
  add_trace(y = ~percadultpoverty/100,
            name = "% below poverty line (adults)") %>% 
  add_trace(y = ~percelderlypoverty/100,
            name = "% below poverty line (eldery)")

fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = "% of poverty by categories",
         plot_bgcolor='#e5ecf6')
fig
# Pomidor ...
# Wróćmy do poprtzedniego wykresu, ale zamieńmy na barplota + dodajmy legende na dole
fig1 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percpovertyknown/100,
                type = "bar",
                name = "% poverty known") %>% 
  layout(xaxis = list(showticklabels = FALSE),
         yaxis = list(tickformat = ".2%", range = c(0.96,0.975)), # ustawmy limity osi
         bargap = 0.05) # pozbądźmy się szczerby

fig2 <- plot_ly(midwest_agg,
                x = ~state,
                y = ~percbelowpoverty/100,
                type = "bar",
                name = "% below poverty line") %>%
  # Uwaga! Layout nalezy umieszczac po glownym skladniu, a nastepnie dodawac kolejne punkty.
  layout(yaxis = list(tickformat = ".2%", range = c(0.08,0.20)),
         xaxis = list(tickfont = list(size = 20))) %>%
  add_trace(y = ~percchildbelowpovert/100,
            name = "% below poverty line (chlidren)") %>% 
  add_trace(y = ~percadultpoverty/100,
            name = "% below poverty line (adults)") %>% 
  add_trace(y = ~percelderlypoverty/100,
            name = "% below poverty line (eldery)")

fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = "% of poverty by categories",
         plot_bgcolor='#e5ecf6',
         legend = list(orientation = 'h'))
fig

# Dodatek ====================================================================================================

# Funkcja ggplotly() z pakietu plotly ma możliwość tłumaczenia ggplot2 na plotly. Ta funkcjonalność
# może być naprawdę pomocna w szybkim dodaniu interaktywności do istniejącego przepływu pracy z ggplot2.

# Przykłady z poprzednich zajęć:

mtcars <- mtcars %>% mutate(kml = round(mpg*0.425144,1),
                            kg = round(wt*1000*0.45359237))
mtcars$cyl <- as.character(mtcars$cyl)

fig <- ggplot(mtcars, aes(x=kml, y=kg, color=cyl)) +
  geom_point(size=5)+scale_color_brewer(name = "Liczba cylindrów", palette="Accent") +
  ylab("Waga (kg)") + xlab("Spalanie (km/l)")
fig
ggplotly(fig)

data(ToothGrowth)
# Porównanie przy skupieniu się na wpływie metod dostarczania
fig <- ggplot(ToothGrowth %>% mutate(dose = as.character(dose)), aes(x=supp, y=len)) + 
  geom_violin(aes(fill=dose)) +
  xlab("Wielkość dawki (mg/dzień)") +
  ylab("Długość") +
  labs(title = "Długość odontoblastów",
       subtitle = "(komórek odpowiedzialnych za wzrost zębów)")
fig
ggplotly(fig)

# Porównanie przy skupieniu się na wpływie wielkości dawek
fig <- ggplot(ToothGrowth %>% mutate(dose = as.character(dose)), aes(x=dose, y=len)) + 
  geom_violin(aes(fill=supp)) +
  xlab("Wielkość dawki (mg/dzień)") +
  ylab("Długość") +
  labs(title = "Długość odontoblastów",
       subtitle = "(komórek odpowiedzialnych za wzrost zębów)")
fig
ggplotly(fig)

































