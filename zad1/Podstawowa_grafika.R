library(dplyr)
library(plotly)
library(tidyr)
### CZĘŚĆ 1 ----
data(rivers) # Lengths of Major North American Rivers
## Zaproponować jak można zobrazować te dane
## (zaproponuj różne ilustracje zmieniając typy wykresów, skale, parametry wykresów).
# Rozkład (histogram) długośći rzek
hist(rivers,
     breaks = 50,
     freq = TRUE,
     main = "CZĘSTOTLIWOŚĆ WYSTĘPOWANIA RZEK O OKREŚLONYCH DŁUGOŚCIACH (KM)",
     xlab = "ILOŚĆ RZEK",
     ylab = "DŁUGOŚĆ RZEKI",
     xlim = c(0, 4000),
     ylim = c(0, 35))
axis(side=1, at=seq(0,4000,250), labels=seq(0,4000,250))

# Wartości statystyczne i boxplot
summary(rivers)
# Min.    1st Qu.  Median  Mean    3rd Qu.  Max. 
# 135.0   310.0    425.0   591.2   680.0    3710.0 
boxplot(rivers,
        main = "STATYSTYKA OPISOWA DŁUGOŚCI RZEK",
        xlab = "DŁUGOŚĆ RZEKI",
        horizontal=TRUE,
        col='steelblue')

# Piechart dla danych wrzuconych w określone przedziały (do dokończenia)
table(cut(rivers,seq(0,4000,500)))

## Narysować błędny wykres i uzasadnić dlaczego jest błędny.
# przykład błędnego wykresu
plot(rivers,
     type = "l",
     main = "Długości głównych rzek Ameryki Północnej (km)",
     ylab = "DŁUGOŚĆ RZEKI",
     xlab = "")
# Problem z tym wykresem jest taki, że przypomina on przebieg jakiejś wartości
# w czasie, podczas, gdy pomiary są oddzielnymi bytami

### CZĘŚĆ 2 ----
# Bierzemy pod uwagę tylko dane dotyczące: wszystkich chrześcijan (christianity_all), całego islamu (islam_all),
# hindulizmu (hinduism_all), wyznawców wszystkich religii (religion_all), wyznawców innych religii poza tymi trzema wiodącymi.
lokalizacja <- getwd()

Religions <- read.csv(paste0(lokalizacja,"/zad1/Religions.csv"), sep = ";")
Religions %>% colnames()
# [1]  "year"                         "christianity_protestant"     
# [3]  "christianity_romancatholic"   "christianity_easternorthodox"
# [5]  "christianity_anglican"        "christianity_other"          
# [7]  "christianity_all"             "judaism_orthodox"            
# [9]  "judaism_conservative"         "judaism_reform"              
# [11] "judaism_other"                "judaism_all"                 
# [13] "islam_sunni"                  "islam_szyici"                
# [15] "islam_all"                    "buddhism_all"                
# [17] "zoroastrianism_all"           "hinduism_all"                
# [19] "sikhism_all"                  "shinto_all"                  
# [21] "taoism_all"                   "jainism_all"                 
# [23] "confucianism_all"             "syncretism_all"              
# [25] "animism_all"                  "noreligion_all"              
# [27] "otherreligion_all"            "religion_all"                
# [29] "population"                   "world_population"   
Religions <- cbind(Religions["year"],
                   Religions[, grepl("_all" ,names(Religions))],
                   Religions["population"],
                   Religions["world_population"])
Religions %>% colnames()
# [1] "year"               "christianity_all"   "judaism_all"       
# [4] "islam_all"          "buddhism_all"       "zoroastrianism_all"
# [7] "hinduism_all"       "sikhism_all"        "shinto_all"        
# [10] "taoism_all"         "jainism_all"        "confucianism_all"  
# [13] "syncretism_all"     "animism_all"        "noreligion_all"    
# [16] "otherreligion_all"  "religion_all"       "population"        
# [19] "world_population"  

## Zobrazować rozwój na przełomie lat głównych religii świata.
fig <- plot_ly(Religions, x = ~year, y = ~world_population, name = 'world_population', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~christianity_all, name = 'christianity_all', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~islam_all, name = 'islam_all', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~hinduism_all, name = 'hinduism_all', type = 'scatter', mode = 'lines')
fig <- fig %>% add_trace(y = ~religion_all, name = 'religion_all', type = 'scatter', mode = 'lines')
fig <- fig %>% layout(title = "Rozwój głównych religii na przełomie lat",
                      xaxis = list(title = "Lata"),
                      yaxis = list (title = "Liczba ludności"))
fig

## Policzyć jak na przestrzeni lat zmienia się różnica między liczbą chrześcijan, a muzułmanów. Zobrazować to odpowiednim wykresem.
fig1 <- plot_ly(Religions, x = ~year, y = ~christianity_all, name = 'christianity_all', type = 'scatter', mode = 'lines')
fig1 <- fig1 %>% add_trace(y = ~islam_all, name = 'islam_all', type = 'scatter', mode = 'lines')
fig1 <- fig1 %>% layout(title = "Różnica między ludnością chrześcijańską, a islamu, na przełomie lat",
                        xaxis = list(range = c(1944,2011),showticklabels=FALSE),
                        yaxis = list (title = "Przyrost wyznawców"))

fig2 <- plot_ly(mutate(Religions, "christ_minus_islam" = christianity_all - islam_all),
               x = ~year,
               y = ~christ_minus_islam,
               name = 'christ_minus_islam',
               type = 'scatter',
               mode = 'markers')
fig2 <- fig2 %>% layout(title = "Rozwój chrześcijaństwa i islamu na przełomie lat",
                        xaxis = list(title = "Lata", range = c(1944,2011)),
                        yaxis = list (title = "Różnica w liczbie wyznawców"))

fig <- subplot(fig1, fig2, nrows = 2, titleY = TRUE, titleX = FALSE)
fig

## Na wybranym wykresie zobrazować jak wyglądała liczebność głównych religii świata w 2010 roku.
Religions2010 <- Religions %>% filter(year == 2010) %>% mutate(other = religion_all -
                                                                 christianity_all -
                                                                 islam_all -
                                                                 hinduism_all -
                                                                 judaism_all -
                                                                 buddhism_all -
                                                                 noreligion_all) %>% 
  select(c("year",
           "christianity_all",
           "islam_all",
           "hinduism_all",
           "judaism_all",
           "buddhism_all",
           "noreligion_all",
           "other"))
Religions2010 <- Religions2010 %>% pivot_longer(!year, names_to = "religion", values_to = "count")

fig <- plot_ly(Religions2010, labels = ~religion, values = ~count, type = 'pie')
fig <- fig %>% layout(title = 'Liczebność głównych religii świata w 2010 roku',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig









