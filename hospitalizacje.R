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

baza <- covid_hospitalizations %>%
  filter(indicator %in% c("Daily hospital occupancy", "Daily ICU occupancy")) %>% 
  tidyr::pivot_wider(names_from = indicator, values_from = value) %>% 
  right_join(full_data_Johns_Hopkins_University, by = c("entity"="location", "date"))

baza$`Daily ICU occupancy` %>% summary()
baza$`Daily hospital occupancy` %>% summary()
baza %>% colnames()
# [1] "entity"                   "iso_code"                 "date"                    
# [4] "Daily ICU occupancy"      "Daily hospital occupancy" "new_cases"               
# [7] "new_deaths"               "total_cases"              "total_deaths"            
# [10] "weekly_cases"             "weekly_deaths"            "biweekly_cases"          
# [13] "biweekly_deaths"

kraj = "Poland"
# wszystkie przypadki vs wszystkie zgony (problem z danymi w hospitalizacji)
fig <- plot_ly(baza %>% filter(entity == kraj),
               x = ~date,
               y = ~total_cases,
               type = 'bar',
               name = 'total cases')
fig <- fig %>% add_trace(y = ~total_deaths, name = 'total deaths')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
fig

# hospitalizacje vs smiertelnosc
fig <- plot_ly(baza %>% filter(entity == kraj),
               x = ~date,
               y = ~new_cases,
               type = 'bar',
               name = 'new cases')
fig <- fig %>% add_trace(y = ~coalesce(filter(baza, entity == kraj)$`Daily hospital occupancy`,
                                       filter(baza, entity == kraj)$`Daily ICU occupancy`), name = 'hospitlization (normal/ICU)')
fig <- fig %>% add_trace(y = ~new_deaths, name = 'new deaths')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
fig




