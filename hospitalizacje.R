source('dane.R')

library(dplyr)
library(plotly)

covid_hospitalizations$indicator %>% table()
# Daily hospital occupancy       Daily hospital occupancy per million 
# 30886                                      30886 
# Daily ICU occupancy            Daily ICU occupancy per million 
# 28134                                      28134 
# Weekly new hospital admissions Weekly new hospital admissions per million 
# 14250                                      14250 
# Weekly new ICU admissions      Weekly new ICU admissions per million 
# 7160       
# ICU - oddzial intensywnej opieki
full_data_Johns_Hopkins_University

# wykres bar jak u Kamila (nowe przypadki/hospitalizacje/smierci)
# porownanie, czy laczna ilosc zachorowan/hospitalizacji, zwiekszala smiertelnosc (procentowo)
# taki wskaznik bedzie naszym wskaznikiem wydolnosci panstwa, po ktorym na koncu bedzie ranking panstw
# moze by wypadalo dodac ogolna smiertelnosc do tego rankingu skoro juz jest pkb per capita
# mozna tez porownanc z europa i swiatem (sa takie dane w Hopkins)

# [1] "Afghanistan"                      "Africa"                           "Albania"                          "Algeria"                         
# [5] "Andorra"                          "Angola"                           "Anguilla"                         "Antigua and Barbuda"             
# [9] "Argentina"                        "Armenia"                          "Aruba"                            "Asia"                            
# [13] "Australia"                        "Austria"                          "Azerbaijan"                       "Bahamas"                         
# [17] "Bahrain"                          "Bangladesh"                       "Barbados"                         "Belarus"                         
# [21] "Belgium"                          "Belize"                           "Benin"                            "Bermuda"                         
# [25] "Bhutan"                           "Bolivia"                          "Bonaire Sint Eustatius and Saba"  "Bosnia and Herzegovina"          
# [29] "Botswana"                         "Brazil"                           "British Virgin Islands"           "Brunei"                          
# [33] "Bulgaria"                         "Burkina Faso"                     "Burundi"                          "Cambodia"                        
# [37] "Cameroon"                         "Canada"                           "Cape Verde"                       "Cayman Islands"                  
# [41] "Central African Republic"         "Chad"                             "Chile"                            "China"                           
# [45] "Colombia"                         "Comoros"                          "Congo"                            "Cook Islands"                    
# [49] "Costa Rica"                       "Cote d'Ivoire"                    "Croatia"                          "Cuba"                            
# [53] "Curacao"                          "Cyprus"                           "Czechia"                          "Democratic Republic of Congo"    
# [57] "Denmark"                          "Djibouti"                         "Dominica"                         "Dominican Republic"              
# [61] "Ecuador"                          "Egypt"                            "El Salvador"                      "Equatorial Guinea"               
# [65] "Eritrea"                          "Estonia"                          "Eswatini"                         "Ethiopia"                        
# [69] "Europe"                           "European Union"                   "Faeroe Islands"                   "Falkland Islands"                
# [73] "Fiji"                             "Finland"                          "France"                           "French Polynesia"                
# [77] "Gabon"                            "Gambia"                           "Georgia"                          "Germany"                         
# [81] "Ghana"                            "Gibraltar"                        "Greece"                           "Greenland"                       
# [85] "Grenada"                          "Guatemala"                        "Guinea"                           "Guinea-Bissau"                   
# [89] "Guyana"                           "Haiti"                            "High income"                      "Honduras"                        
# [93] "Hong Kong"                        "Hungary"                          "Iceland"                          "India"                           
# [97] "Indonesia"                        "International"                    "Iran"                             "Iraq"                            
# [101] "Ireland"                          "Isle of Man"                      "Israel"                           "Italy"                           
# [105] "Jamaica"                          "Japan"                            "Jordan"                           "Kazakhstan"                      
# [109] "Kenya"                            "Kiribati"                         "Kosovo"                           "Kuwait"                          
# [113] "Kyrgyzstan"                       "Laos"                             "Latvia"                           "Lebanon"                         
# [117] "Lesotho"                          "Liberia"                          "Libya"                            "Liechtenstein"                   
# [121] "Lithuania"                        "Low income"                       "Lower middle income"              "Luxembourg"                      
# [125] "Macao"                            "Madagascar"                       "Malawi"                           "Malaysia"                        
# [129] "Maldives"                         "Mali"                             "Malta"                            "Marshall Islands"                
# [133] "Mauritania"                       "Mauritius"                        "Mexico"                           "Micronesia (country)"            
# [137] "Moldova"                          "Monaco"                           "Mongolia"                         "Montenegro"                      
# [141] "Montserrat"                       "Morocco"                          "Mozambique"                       "Myanmar"                         
# [145] "Namibia"                          "Nauru"                            "Nepal"                            "Netherlands"                     
# [149] "New Caledonia"                    "New Zealand"                      "Nicaragua"                        "Niger"                           
# [153] "Nigeria"                          "North America"                    "North Korea"                      "North Macedonia"                 
# [157] "Norway"                           "Oceania"                          "Oman"                             "Pakistan"                        
# [161] "Palau"                            "Palestine"                        "Panama"                           "Papua New Guinea"                
# [165] "Paraguay"                         "Peru"                             "Philippines"                      "Poland"                          
# [169] "Portugal"                         "Qatar"                            "Romania"                          "Russia"                          
# [173] "Rwanda"                           "Saint Helena"                     "Saint Kitts and Nevis"            "Saint Lucia"                     
# [177] "Saint Pierre and Miquelon"        "Saint Vincent and the Grenadines" "Samoa"                            "San Marino"                      
# [181] "Sao Tome and Principe"            "Saudi Arabia"                     "Senegal"                          "Serbia"                          
# [185] "Seychelles"                       "Sierra Leone"                     "Singapore"                        "Slovakia"                        
# [189] "Slovenia"                         "Solomon Islands"                  "Somalia"                          "South Africa"                    
# [193] "South America"                    "South Korea"                      "South Sudan"                      "Spain"                           
# [197] "Sri Lanka"                        "Sudan"                            "Suriname"                         "Sweden"                          
# [201] "Switzerland"                      "Syria"                            "Taiwan"                           "Tajikistan"                      
# [205] "Tanzania"                         "Thailand"                         "Timor"                            "Togo"                            
# [209] "Tonga"                            "Trinidad and Tobago"              "Tunisia"                          "Turkey"                          
# [213] "Turks and Caicos Islands"         "Tuvalu"                           "Uganda"                           "Ukraine"                         
# [217] "United Arab Emirates"             "United Kingdom"                   "United States"                    "Upper middle income"             
# [221] "Uruguay"                          "Uzbekistan"                       "Vanuatu"                          "Vatican"                         
# [225] "Venezuela"                        "Vietnam"                          "Wallis and Futuna"                "World"                           
# [229] "Yemen"                            "Zambia"                           "Zimbabwe"                        

kraj = "Poland"
# wszystkie przypadki vs wszystkie zgony (problem z danymi w hospitalizacji)
# korelacja pomiedzy iloscia wszystkich przypadkow a iloscia zgonow, patrzymy, czy jest jakas zaleznosc (czy zgony rosna)
fig <- plot_ly(full_data_Johns_Hopkins_University %>% filter(location == kraj),
               x = ~date,
               y = ~total_cases,
               type = 'bar',
               name = 'total cases')
fig <- fig %>% add_trace(y = ~total_deaths, name = 'total deaths')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
fig

# kor <- cor.test(filter(full_data_Johns_Hopkins_University, location == kraj)$total_cases,
#                 filter(full_data_Johns_Hopkins_University, location == kraj)$total_deaths,
#                 method=c("pearson", "kendall", "spearman"))
# ifelse(kor$p.value < 0.05,
#        paste("Korelacja pomiędzy łączną ilością rozpoznanych zachorowań i łączną liczbą śmierci w wyniku choroby jest statystycznie istotna i wynosi:",round(kor$estimate,2)),
#        paste("Korelacja pomiędzy łączną ilością rozpoznanych zachorowań i łączną liczbą śmierci w wyniku choroby jest statystycznie nieistotna i wynosi:",round(kor$estimate,2)))
# # nic diwnego ze mocno skorelowane, skoro smiertelnosc to jakis % przypadkow - jedyny wniosek jest taki,
# # ze smiertelnosc nie maleje, tj. jak bylo na poczatku tak bylo potem - nie wiadomo czy bylo dobrze czy zle, wiemy tylko, ze bez zmian

# mortality <- full_data_Johns_Hopkins_University %>% full_join(excess_mortality, by = c("location", "date")) - nie zadziala, bo hopkins to dane dzienne, a excess dane tygodniowe

wspolna_os <- function(baza1, baza2) { # problem, bo tygodniowe vs dzienne dane
  max1 <- baza1$date %>% max()
  min1 <- baza1$date %>% min()
  
  max2 <- baza2$date %>% max()
  min2 <- baza2$date %>% min()
  
  if (max1 < max2){
    new_max <- max1
  } else {
    new_max <- max2
  }
  
  # max <- baza2 %>% filter(date <= new_max) %>% max()
  
  if (min1 > min2){
    new_min <- min1
  } else {
    new_min <- min2
  }
  print(new_min)
  # min <- baza2 %>% filter(date >= new_min) %>% min()
  
  return(c(min, max))
}

full_data_Johns_Hopkins_University_filtered <- full_data_Johns_Hopkins_University %>% filter(location == kraj)
excess_mortality_filtered <- excess_mortality %>%  filter(location == kraj)
min_max <- wspolna_os(full_data_Johns_Hopkins_University_filtered, excess_mortality_filtered)

# ten wykres jest raczej do spekulowania fali zachorowan, widac przyrost w konkretnych miesiacach (nic nowego) - rozwinieto o procent smiertelnosci (w porownaniu do poprzednich lat)
# analogicznie na nowych przypadkach (teoretycznie, jezeli dobra wydolnosc, to poziom nowych zgonow niezmienna) - malo czytelne
fig1 <- plot_ly(full_data_Johns_Hopkins_University_filtered %>% filter(date >= min_max[1] & date <= min_max[2]),
               x = ~date,
               y = ~new_cases,
               type = 'bar',
               name = 'nowe przypadki')
# fig <- fig %>% add_trace(y = ~new_deaths, name = 'new deaths')
fig1 <- fig1 %>% layout(xaxis = list(showticklabels = FALSE),
                        yaxis = list(title = 'Count'),
                        barmode = 'stack',
                        annotations = a,
                        shapes = l)

# We calculate P-scores using the reported deaths data from HMD and WMD and the projected deaths from WMD,
# as an estimate of expected deaths. The P-score is the percentage difference between the reported number of weekly
# or monthly deaths in 2020–2022 and the projected number of deaths for the same period based on previous years.
# - p_proj_all_ages: P-scores using projected baseline for all ages; see note below for the definition of the P-score, how we calculate it, and changes implemented on 20 September 2021,
# - p_proj_0_14: P-scores using projected baseline for ages 0–14,
# - p_proj_15_64: P-scores using projected baseline for ages 15–64,
# - p_proj_65_74: P-scores using projected baseline for ages 65–74,
# - p_proj_75_84: P-scores using projected baseline for ages 75–84,
# - p_proj_85p: P-scores using projected baseline for ages 85 and above,
# - p_scores_all_ages: P-scores using 5-year average baseline for all ages,
# - p_scores_0_14: P-scores using 5-year average baseline for ages 0–14,
# - p_scores_15_64: P-scores using 5-year average baseline for ages 15–64,
# - p_scores_65_74: P-scores using 5-year average baseline for ages 65–74,
# - p_scores_75_84: P-scores using 5-year average baseline for ages 75–84,
# - p_scores_85p: P-scores using 5-year average baseline for ages 85 and above.
# projected baseline - prognozowana wartosc
# 5-year average baseline - srednia z 5 ostatnich lat
fig2 <- plot_ly(excess_mortality_filtered %>% filter(date >= min_max[1] & date <= min_max[2]),
               x = ~date,
               y = ~p_proj_all_ages/100,
               type = 'scatter',
               mode = "lines",
               name = 'all') %>% 
  add_trace(y = ~p_proj_0_14/100,
            name = "0 - 14",
            mode = "lines") %>% 
  add_trace(y = ~p_proj_15_64/100,
            name = "15 - 64",
            mode = "lines") %>% 
  add_trace(y = ~p_proj_65_74/100,
            name = "65 - 74",
            mode = "lines") %>% 
  add_trace(y = ~p_proj_75_84/100,
            name = "75 - 84",
            mode = "lines") %>% 
  add_trace(y = ~p_proj_85p/100,
            name = "85p",
            mode = "lines")
fig2 <- fig2 %>% layout(title = paste("Zmiana procentowa smiertelnosci (wzgledem poprzednich lat) -",kraj),
                        yaxis = list(title = "% wzrostu/spadku umieralności", tickformat = ".0%"),
                        shapes = l)

# zlaczenie tych wykresow razem da nam ciekawsze spojrzenie na to, kto umieral w tym czasie
fig <- subplot(fig1, fig2, nrows = 2)
fig

COVID_dead <- full_data_Johns_Hopkins_University %>%
  mutate(wskaznik_smiertelnosci_total_open = total_deaths/total_cases,
         wskaznik_smiertelnosci_new = new_deaths/new_cases) %>% 
  left_join(full_data_Johns_Hopkins_University %>%
              mutate(date = as.Date(date)+1,
                     wskaznik_smiertelnosci_total_close = total_deaths/total_cases),
            by = c("location", "date")) %>% 
  left_join(vaccinations_by_manufacturer %>%
              select(c(location, date)) %>%
              group_by(location) %>%
              summarize(first_vacc = min(date)),
            by = "location") %>% 
  filter(location == kraj)

# W apce zrobic zmienna wygladzania wykresu - najlepiej jako wskaznik suwakowy+-
k = 7 # (minimum 1)
rolling <- zoo::rollmean(x = COVID_dead$wskaznik_smiertelnosci_new,
                         k = k,
                         align = "right")

daty <- seq.Date(from = min(COVID_dead$date),
                 to = max(COVID_dead$date),
                 length.out = length(rolling))

fig <- plot_ly(x = daty,
               y = rolling,
               type = 'scatter',
               mode = "lines",
               name = 'wskaznik_smiertelnosci_new')
# annotation
a <- list(text = paste("Pierwsza szczepionka (",if_else(is.na(min(COVID_dead$first_vacc)),
                                                        as.Date("2999-12-31"), # wymuszone przez if_else()-a
                                                        min(COVID_dead$first_vacc)),")"),
          x = if_else(is.na(min(COVID_dead$first_vacc)),as.Date(max(COVID_dead$date))+14,min(COVID_dead$first_vacc)),
          y = 1.02,
          xref = 'x',
          yref = 'paper',
          xanchor = 'left',
          showarrow = FALSE
)
# use shapes to create a line
l <- list(type = line,
          x0 = if_else(is.na(min(COVID_dead$first_vacc)),as.Date(max(COVID_dead$date))+14,min(COVID_dead$first_vacc)),
          x1 = if_else(is.na(min(COVID_dead$first_vacc)),as.Date(max(COVID_dead$date))+14,min(COVID_dead$first_vacc)),
          y0 = 0,
          y1 = 1,
          xref = 'x',
          yref = 'paper',
          line = list(color = 'black',
                      width = 0.5)
)
fig <- fig %>% layout(yaxis = list(title = "", tickformat = ".2%"),
                      annotations = a,
                      shapes = l)
fig

# przez cos takiego mozna stwierdzic przystosowanie sie panstwa do sytuacji 
# (dodac linie odciecia na wysokosci podania pierwszysch szczepionek)
# fig <- plot_ly(tst,
#                x = ~date,
#                y = ~wskaznik_smiertelnosci_open,
#                type = 'scatter',
#                mode = 'line')
# fig

# annotation
# a <- list(text = paste("Pierwsza szczepionka (",if_else(is.na(min(COVID_dead$first_vacc)),
#                                                        as.Date("2999-12-31"), # wymuszone przez if_else()-a
#                                                        min(COVID_dead$first_vacc)),")"),
#           x = if_else(is.na(min(COVID_dead$first_vacc)),as.Date(max(COVID_dead$date))+14,min(COVID_dead$first_vacc)),
#           y = 1.02,
#           xref = 'x',
#           yref = 'paper',
#           xanchor = 'left',
#           showarrow = FALSE
# )
# # use shapes to create a line
# l <- list(type = line,
#           x0 = if_else(is.na(min(COVID_dead$first_vacc)),as.Date(max(COVID_dead$date))+14,min(COVID_dead$first_vacc)),
#           x1 = if_else(is.na(min(COVID_dead$first_vacc)),as.Date(max(COVID_dead$date))+14,min(COVID_dead$first_vacc)),
#           y0 = 0,
#           y1 = 1,
#           xref = 'x',
#           yref = 'paper',
#           line = list(color = 'black',
#                       width = 0.5)
# )
fig <- plot_ly(COVID_dead,
               x = ~date,
               open = ~wskaznik_smiertelnosci_total_open,
               close = ~wskaznik_smiertelnosci_total_close,
               high = ~wskaznik_smiertelnosci_total_close,
               low = ~wskaznik_smiertelnosci_total_open,
               type = 'candlestick')
fig <- fig %>% layout(title = paste("Zmiana procentowa smiertelnosci (total) -",kraj),
                      xaxis = list(rangeslider = list(visible = F),
                                   range = list(as.Date(min(COVID_dead$date))-7, as.Date(max(COVID_dead$date))+7)),
                      yaxis = list(tickformat = ".2%"),
                      annotations = a,
                      shapes = l)
fig

# # musi byc takie zlaczenie z uwagi na ogromne braki w informacji o hospitalizacji
# baza <- covid_hospitalizations %>%
#   filter(indicator %in% c("Daily hospital occupancy", "Daily ICU occupancy")) %>% 
#   tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
#   left_join(full_data_Johns_Hopkins_University, by = c("entity"="location", "date"))
# 
# baza$`Daily ICU occupancy` %>% summary()
# baza$`Daily hospital occupancy` %>% summary()
# baza %>% colnames()
# # [1] "entity"                   "iso_code"                 "date"                    
# # [4] "Daily ICU occupancy"      "Daily hospital occupancy" "new_cases"               
# # [7] "new_deaths"               "total_cases"              "total_deaths"            
# # [10] "weekly_cases"             "weekly_deaths"            "biweekly_cases"          
# # [13] "biweekly_deaths"
# 
# baza$iso_code %>% unique() # - niewydolnosc mozna policzyc tylko z smiertelnosc/nowe przypadki poniewaz duze braki w danych o hospitalizacji
# 
# # hospitalizacje vs smiertelnosc - malo czytelne w takiej formie
# fig <- plot_ly(baza %>% filter(entity == kraj),
#                x = ~date,
#                y = ~new_cases,
#                type = 'bar',
#                name = 'new cases')
# fig <- fig %>% add_trace(y = ~coalesce(filter(baza, entity == kraj)$`Daily hospital occupancy`,
#                                        filter(baza, entity == kraj)$`Daily ICU occupancy`), name = 'hospitlization (normal/ICU)')
# fig <- fig %>% add_trace(y = ~new_deaths, name = 'new deaths')
# fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
# fig


