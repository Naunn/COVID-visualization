source("dane.R")

library(dplyr)
library(countrycode) # do kodów państw
library(WDI) # do wskaznika PKB
library(plotly)

# https://plotly.com/python/builtin-colorscales/ - wbudowane skale kolorow w plotly
# przyjmijmy liczbe ludnosci pod koniec 2019r.
population <- read.csv("https://gist.githubusercontent.com/curran/0ac4077c7fc6390f5dd33bf5c06cb5ff/raw/605c54080c7a93a417a3cea93fd52e7550e76500/UN_Population_2019.csv",
                      sep = ",") %>%
  select(c("Country", "Country.code", "X2019")) %>%
  mutate(X2019 = X2019*1000) %>% 
  left_join(codelist %>% select(c(iso3c, iso3n)), by = c("Country.code" = "iso3n")) %>% 
  tidyr::drop_na() %>% 
  left_join(WDI(country = "all", start = 2019, end = 2019) %>% select(c(iso3c, NY.GDP.PCAP.KD)), by = "iso3c")

l <- list(color = toRGB("black"), width = 0.5)

fig <- plot_geo(population)
fig <- fig %>% add_trace(
  z = ~NY.GDP.PCAP.KD, color = ~NY.GDP.PCAP.KD, colors = 'Greens',
  text = ~Country, locations = ~iso3c, marker = list(line = l)
)
fig

# zamiast shiny, to w dasha - proba, a jak nie, to wpisywanianie panstwa na pale (wybieranie z listy pod mapą)

# dorobić mapke rozprzestrzeniania sie covidu wraz ze sliderem https://plotly.com/r/cumulative-animations/