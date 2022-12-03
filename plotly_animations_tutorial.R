library(plotly)
library(tidyverse)
library(knitr)
library(countrycode) # for ISO3 country codes

### Dane =====================================================================================================
covid_testing_all_observations <- read.csv(
  paste0(lokalizacja,"/data/testing/covid-testing-all-observations.csv"),
  header = TRUE,
  sep = ",")
covid_testing_all_observations$Date <- as.Date(covid_testing_all_observations$Date)
covid_testing_all_observations$X7.day.smoothed.daily.change <- as.double(covid_testing_all_observations$X7.day.smoothed.daily.change)
# Rows: 106,788
# Columns: 14
# $ Entity                                        <chr> "Afghanistan - te~
# $ ISO.code                                      <chr> "AFG", "AFG", "AF~
# $ Date                                          <date> 2022-01-29, 2022~
# $ Source.URL                                    <chr> "http://www.emro.~
# $ Source.label                                  <chr> "WHO Regional Off~
# $ Notes                                         <chr> "", "", "", "", "~
# $ Cumulative.total                              <dbl> 853003, NA, NA, N~
# $ Daily.change.in.cumulative.total              <dbl> NA, NA, NA, NA, N~
# $ Cumulative.total.per.thousand                 <dbl> 21.272, NA, NA, N~
# $ Daily.change.in.cumulative.total.per.thousand <dbl> NA, NA, NA, NA, N~
# $ X7.day.smoothed.daily.change                  <dbl> NA, NA, NA, NA, N~
# $ X7.day.smoothed.daily.change.per.thousand     <dbl> NA, NA, NA, NA, N~
# $ Short.term.positive.rate                      <dbl> NA, NA, NA, NA, N~
# $ Short.term.tests.per.case                     <dbl> NA, NA, NA, NA, N~


# Agregacja przypadków do tygodni (jeżeli NA, to srednia z daty przed i daty po)
covid_testing_all_observations$week <- lubridate::ceiling_date(covid_testing_all_observations$Date,
                                                               "week",
                                                               week_start = getOption("lubridate.week.start", 1),
                                                               change_on_boundary = TRUE)

# Podglad ilosci danych/tygodni (niezerowych)
covid_testing_count <- covid_testing_all_observations %>%
  select(week, ISO.code, Cumulative.total) %>%
  group_by(ISO.code, week) %>%
  summarize(sum = sum(Cumulative.total, na.rm = TRUE)) %>%
  filter(sum > 0) %>%
  .$ISO.code %>%
  table() %>%
  as.data.frame()

# Uspójnienie osi (mozna teraz łatwo rysować wartosci)
covid_testing_agg <- covid_testing_all_observations %>%
  select(week,
         ISO.code,
         X7.day.smoothed.daily.change,
         X7.day.smoothed.daily.change.per.thousand,
         Cumulative.total,
         Short.term.positive.rate) %>%
  group_by(ISO.code, week) %>%
  summarize(suma_testow_tyg = sum(X7.day.smoothed.daily.change, na.rm = TRUE),
            avg_test_tyg_1000 = round(mean(X7.day.smoothed.daily.change.per.thousand, na.rm = TRUE),4),
            cumulative_per_week = sum(Cumulative.total, na.rm = TRUE),
            Short.term.positive.rate = round(mean(Short.term.positive.rate, na.rm = TRUE),4) %>%
              format(9999, scientific = FALSE)) %>% # Usuniecie zapisu naukowego, tj. z "e"
  # jeszcze trzeba usunac "NaN'-y ze sredniej
  filter(cumulative_per_week > 0) %>% 
  inner_join(covid_testing_count %>% filter(Freq > 90), by = c("ISO.code" = ".")) %>% 
  select(!Freq) %>% 
  mutate(Short.term.positive.rate = as.numeric(Short.term.positive.rate)) %>% 
  replace(is.na(.), 0)

# short.term.positive.rate - 100 x Liczba nowych potwierdzonych przypadków/liczba testów wykonanych w tygodniu

# Interaktywna (statyczna) wizualizacja ======================================================================

kraj = "POL"
baza <- covid_testing_agg %>% filter(ISO.code == kraj)

fig1 <- plot_ly(baza,
                x = ~week,
                y = ~suma_testow_tyg,
                name = ~ISO.code,
                type = 'scatter',
                mode = 'lines+markers',
                hovertemplate = paste("Suma testów (tyg): %{y}<extra></extra>")) %>% 
  layout(xaxis = list(showticklabels = FALSE)) %>% 
  animation_opts(transition = 0)
fig2 <- plot_ly(baza,
                x = ~week,
                y = ~Short.term.positive.rate,
                name = ~ISO.code,
                type = 'scatter',
                mode = 'lines+markers',
                hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%")) %>% 
  animation_opts(transition = 0)
fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = kraj,
         showlegend = FALSE,
         hovermode = "x unified")
fig

# Akcja animacja-wizualizacja ================================================================================
# tworzenie animacji klatka po klatce

# FRAME - klatka (w kazdej klatce, musi byc zachowane kilka obrazow)

df <- data.frame(
  x = c(1,2,1), 
  y = c(1,2,1), 
  f = c(1,2,3)
)

kable(df)

df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )

kable(baza)

plot_ly(baza,
        x = ~week,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~week,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"))
# Problem, może brak ciągłości w kolumnie week?
kable(baza$week) # to są dane tygodniowe, także ..., ale spróbujmy

plot_ly(baza %>% cbind(seq.Date(as.Date("2020-03-16"), by = "day", length.out = 119)) %>% rename(ID = ...7),
        x = ~week,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~ID,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"))

# Nie dałoby nic ...
# Zatem, dodajmy ID tradycyjne, tj. 1, 2, ...

plot_ly(baza %>% cbind(seq(1,nrow(baza),1)) %>% rename(ID = ...7),
        x = ~week,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~ID,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"))

# Hmmm, podejrzenie pada na daty
plot_ly(baza %>% cbind(seq(1,nrow(baza),1)) %>% rename(ID = ...7),
        x = ~ID,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~week,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"))

# Działa? No tak średnio bym powiedział. Ewidentnie ma problem z datami, to może ...
plot_ly(baza %>% cbind(seq(1,nrow(baza),1)) %>% rename(ID = ...7) %>% mutate(week_str=as.character(week)),
        x = ~ID,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~ID,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"))

# Sukces? Technicznie tak, ale nie o to nam chodziło. Dlaczego tak jest? (pytanie do grupy)

## Animacja - wykres skumulowany (film) ======================================================================
# https://plotly.com/ggplot2/cumulative-animations/

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

baza_acc <- baza %>%
  cbind(seq(1,nrow(baza),1)) %>%
  rename(ID = ...7) %>%
  accumulate_by(~ID)

baza_acc %>% head(15) %>% kable()
baza_acc %>% nrow()

plot_ly(baza_acc,
        x = ~ID,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~frame,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"))

# Czas na upiększanie
plot_ly(baza_acc,
        x = ~ID,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~frame,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"), showlegend = FALSE) %>% 
  animation_opts(frame = 10, 
                 transition = 10,
                 easing = "linear",
                 redraw = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "week "))

# Dlaczego prefix nie dziala?

baza_acc <- baza %>%
  cbind(seq(1,nrow(baza),1)) %>%
  rename(ID = ...7) %>%
  accumulate_by(~week)

baza_acc %>% head(15) %>% kable()
baza_acc %>% nrow()

plot_ly(baza_acc,
        x = ~ID,
        y = ~Short.term.positive.rate,
        name = ~ISO.code,
        type = 'scatter',
        mode = 'lines+markers',
        frame = ~frame,
        hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"), showlegend = FALSE) %>% 
  animation_opts(frame = 10, 
                 transition = 10,
                 easing = "linear",
                 redraw = TRUE) %>% 
  animation_slider(currentvalue = list(prefix = "week "))

# Zatem spróbujmy podwójny plot
fig1 <- plot_ly(baza_acc,
                x = ~ID,
                y = ~suma_testow_tyg,
                name = ~ISO.code,
                type = 'scatter',
                mode = 'lines+markers',
                frame = ~frame,
                hovertemplate = paste("Suma testów (tyg): %{y}<extra></extra>")) %>% 
  layout(yaxis = list(showlegend = FALSE)) %>% 
  animation_slider(currentvalue = list(prefix = "week "))
fig2 <- plot_ly(baza_acc,
                x = ~ID,
                y = ~Short.term.positive.rate,
                type = 'scatter',
                mode = 'lines+markers',
                frame = ~frame,
                hovertemplate = paste("Testy pozytywne: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".2%"), showlegend = FALSE) %>% 
  animation_slider(currentvalue = list(prefix = "week "))
fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = kraj,
         showlegend = FALSE) %>% 
  animation_opts(frame = 10, 
                 transition = 10,
                 easing = "linear",
                 redraw = TRUE)
fig

# A kiedy wykorzystać nieskumulowany wykres? Może mapa?

## Animacja - wykres przejściowy (pokaz slajdów) =============================================================
# https://plotly.com/r/animations/

### Dane =====================================================================================================
deaths_ts <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
  select(!c(Province.State, Lat, Long)) %>% 
  aggregate(.~Country.Region, FUN = sum) %>% 
  pivot_longer(!Country.Region, names_to = "date", values_to = "deaths") %>% 
  mutate(date = as.POSIXct(reduce2(c('X', '\\.'), c('', '-'),  .init = date, str_replace_all), format = "%m-%d-%y"))

recovered_ts <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>% 
  select(!c(Province.State, Lat, Long)) %>% 
  aggregate(.~Country.Region, FUN = sum) %>% 
  pivot_longer(!Country.Region, names_to = "date", values_to = "recovery") %>% 
  mutate(date = str_replace(date, "X", "")) %>% 
  mutate(date = as.POSIXct(reduce2(c('X', '\\.'), c('', '-'),  .init = date, str_replace_all), format = "%m-%d-%y"))

df_ts <- deaths_ts %>%
  left_join(recovered_ts, by = c("Country.Region", "date")) %>% 
  mutate(iso3c = countrycode(Country.Region, origin = "country.name", destination = "iso3c")) %>% 
  drop_na() %>% 
  group_by(iso3c) %>% 
  mutate(id = 1:n()) %>% # zbędne, ale zostawię jako dobry trik
  ungroup()

df_ts %>% glimpse()
# Rows: 203,970
# Columns: 6
# Groups: iso3c [195]
# $ Country.Region <chr> "Afghanistan", "Afghanistan", "Afghanistan", "Afghanistan", "…
# $ date           <dttm> 2020-01-22, 2020-01-23, 2020-01-24, 2020-01-25, 2020-01-26, …
# $ deaths         <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ recovery       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
# $ iso3c          <chr> "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG", "AFG"…
# $ id             <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18…

## "Pokaz slajdów" ===========================================================================================
# tst <- df_ts %>% filter(Country.Region %in% c("Poland","France")) %>% filter(date >= as.Date("2021-08-01") & date <= as.Date("2021-08-04"))
d_vs_r <- df_ts %>%
  filter(Country.Region %in% c("Poland", "France", "Germany", "Italy", "Czechia")) %>% 
  filter(date >= as.Date("2020-04-01") & date <= as.Date("2021-08-01"))

plot_ly(data = d_vs_r,
        x = ~Country.Region,
        y = ~deaths,
        name = "deaths",
        frame = ~date,
        marker = list(color = "red"),
        type = 'bar',
        # text = ~deaths,
        # textposition = 'top',
        hovertemplate = paste("Deaths: %{y}<extra></extra>")) %>% 
  add_trace(y = ~recovery,
            name = 'recovery',
            marker = list(color = "green"),
            # text = ~recovery,
            # textposition = 'top',
            hovertemplate = paste("Recoveries: %{y}<extra></extra>")) %>% 
  layout(title = "deaths vs recovery",
         xaxis = list(title = "Country"),
         yaxis = list(title = ""),
         showlegend = FALSE) %>% 
  animation_opts(frame = 100, 
                 transition = 100,
                 easing = "linear",
                 redraw = TRUE) %>% 
  animation_slider(currentvalue = list(prefix = "date "))































# Budnopis ===================================================================================================

df <- data.frame(
  x = c(1,2,1), 
  y = c(1,2,1), 
  f = c(1,2,3)
)

fig <- df %>%
  plot_ly(
    x = ~x,
    y = ~y,
    frame = ~f,
    type = 'scatter',
    mode = 'markers',
    showlegend = F
  )

fig

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- txhousing 
fig <- df %>%
  filter(year > 2005, city %in% c("Abilene", "Bay Area"))
fig <- fig %>% accumulate_by(~date)


fig <- fig %>%
  plot_ly(
    x = ~date, 
    y = ~median,
    split = ~city,
    frame = ~frame, 
    type = 'scatter',
    mode = 'lines', 
    line = list(simplyfy = F)
  )
fig <- fig %>% layout(
  xaxis = list(
    title = "Date",
    zeroline = F
  ),
  yaxis = list(
    title = "Median",
    zeroline = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  hide = T
)
fig <- fig %>% animation_button(
  x = 1, xanchor = "right", y = 0, yanchor = "bottom"
)

fig

library(quantmod)
getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)
df$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

df <- df %>% accumulate_by(~ID)

fig <- df %>% plot_ly(
  x = ~ID, 
  y = ~AAPL.Close, 
  frame = ~frame,
  type = 'scatter', 
  mode = 'lines', 
  fill = 'tozeroy', 
  fillcolor='rgba(114, 186, 59, 0.5)',
  line = list(color = 'rgb(114, 186, 59)'),
  text = ~paste("Day: ", ID, "<br>Close: $", AAPL.Close), 
  hoverinfo = 'text'
)
fig <- fig %>% layout(
  title = "AAPL: Last 30 days",
  yaxis = list(
    title = "Close", 
    range = c(0,250), 
    zeroline = F,
    tickprefix = "$"
  ),
  xaxis = list(
    title = "Day", 
    range = c(0,30), 
    zeroline = F, 
    showgrid = F
  )
) 
fig <- fig %>% animation_opts(
  frame = 100, 
  transition = 0, 
  redraw = FALSE
)
fig <- fig %>% animation_slider(
  currentvalue = list(
    prefix = "Day "
  )
)

fig

x <- c('Product A', 'Product B', 'Product C')
y <- c(20, 14, 23)
text <- c('27% market share', '24% market share', '19% market share')
data <- data.frame(x, y, text)

fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar',
               text = y, textposition = 'up',
               marker = list(color = 'rgb(158,202,225)',
                             line = list(color = 'rgb(8,48,107)', width = 1.5)))
fig <- fig %>% layout(title = "January 2013 Sales Report",
                      xaxis = list(title = ""),
                      yaxis = list(title = ""))

fig
