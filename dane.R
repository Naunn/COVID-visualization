# COVID-19 Dataset by Our World in Data
# https://github.com/owid/covid-19-data/tree/master/public/data
library(dplyr)

lokalizacja <- getwd()

vaccinations_by_age_group_countries <- read.csv(
  paste0(lokalizacja,"/data/vaccinations/vaccinations-by-age-group.csv"),
  header = TRUE,
  sep = ",")
vaccinations_by_age_group_countries$date <- as.Date(vaccinations_by_age_group_countries$date)
# Rows: 49,284
# Columns: 6
# $ location                            <chr> "Argentina", "Argentina"~
# $ date                                <date> 2020-01-01, 2020-01-01,~
# $ age_group                           <chr> "100+", "12-17", "18-29"~
# $ people_vaccinated_per_hundred       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, ~
# $ people_fully_vaccinated_per_hundred <dbl> 0, 0, 0, 0, 0, 0, 0, 0, ~
# $ people_with_booster_per_hundred     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, ~

# vaccine booster - A COVID booster shot is an additional dose or doses of
# a vaccine given after the protection provided by the original shot(s)
# has begun to decrease over time.

vaccinations_by_manufacturer <- read.csv(
  paste0(lokalizacja,"/data/vaccinations/vaccinations-by-manufacturer.csv"),
  header = TRUE,
  sep = ",")
vaccinations_by_manufacturer$date <- as.Date(vaccinations_by_manufacturer$date)
# Rows: 47,251
# Columns: 4
# $ location           <chr> "Argentina", "Argentina", "Argentina", "Argentina", "Arge~
# $ date               <date> 2020-12-29, 2020-12-29, 2020-12-29, 2020-12-30, 2020-12-~
# $ vaccine            <chr> "Oxford/AstraZeneca", "Sinopharm/Beijing", "Sputnik V", "~
# $ total_vaccinations <int> 1, 1, 20487, 40589, 43395, 3, 43520, 4, 1, 46830, 2, 4727~

# # Passing argument files
# myData <- rjson::fromJSON(file=paste0(lokalizacja,"/data/vaccinations/vaccinations.json"))
# # Convert JSON file to dataframe.
# vaccinations <- as.data.frame(myData)
# print(vaccinations)
# Zdecydowanie zbyt duży plik, lepszym pomysłem byłby automat do zaczytywania
# danych z folderu "country_data".

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

covid_hospitalizations <- read.csv(
  paste0(lokalizacja,"/data/hospitalizations/covid-hospitalizations.csv"),
  header = TRUE,
  sep = ",")
covid_hospitalizations$date <- as.Date(covid_hospitalizations$date)
# Rows: 160,860
# Columns: 5
# $ entity    <chr> "Algeria", "Algeria", "Algeria", "Algeria", "Algeria~
# $ iso_code  <chr> "DZA", "DZA", "DZA", "DZA", "DZA", "DZA", "DZA", "DZ~
# $ date      <date> 2020-07-17, 2020-07-17, 2020-07-18, 2020-07-18, 202~
# $ indicator <chr> "Daily ICU occupancy", "Daily ICU occupancy per mill~
# $ value     <dbl> 62.000, 1.403, 67.000, 1.517, 64.000, 1.449, 56.000,~
# Intensive Care Unit (ICU)

excess_mortality <- read.csv(
  paste0(lokalizacja,"/data/excess_mortality/excess_mortality.csv"),
  header = TRUE,
  sep = ",")
excess_mortality$date <- as.Date(excess_mortality$date)
# Rows: 9,962
# Columns: 38
# $ location                             <chr> "Albania", "Albania", "Albania"~
# $ date                                 <date> 2020-01-31, 2020-02-29, 2020-0~
# $ p_scores_all_ages                    <dbl> -10.65, 2.17, 0.62, 3.23, 6.15,~
# $ p_scores_15_64                       <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ p_scores_65_74                       <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ p_scores_75_84                       <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ p_scores_85plus                      <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ deaths_2020_all_ages                 <dbl> 2187, 2018, 1993, 1804, 1803, 1~
# $ average_deaths_2015_2019_all_ages    <dbl> 2447.8, 1975.2, 1980.8, 1747.6,~
# $ deaths_2015_all_ages                 <dbl> 2490, 2139, 2051, 1906, 1709, 1~
# $ deaths_2016_all_ages                 <dbl> 2065, 1905, 1910, 1652, 1716, 1~
# $ deaths_2017_all_ages                 <dbl> 3129, 1845, 1817, 1665, 1660, 1~
# $ deaths_2018_all_ages                 <dbl> 2158, 2013, 2136, 1828, 1664, 1~
# $ deaths_2019_all_ages                 <dbl> 2397, 1974, 1990, 1687, 1744, 1~
# $ deaths_2010_all_ages                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ deaths_2011_all_ages                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ deaths_2012_all_ages                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ deaths_2013_all_ages                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ deaths_2014_all_ages                 <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ deaths_2021_all_ages                 <dbl> 3002, 3773, 3447, 2419, 1874, 1~
# $ time                                 <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ~
# $ time_unit                            <chr> "monthly", "monthly", "monthly"~
# $ p_scores_0_14                        <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ projected_deaths_2020_2022_all_ages  <dbl> 2434.2, 1961.6, 1967.2, 1734.0,~
# $ excess_proj_all_ages                 <dbl> -247.2, 56.4, 25.8, 70.0, 118.0~
# $ cum_excess_proj_all_ages             <dbl> -247.2, -190.8, -165.0, -95.0, ~
# $ cum_proj_deaths_all_ages             <dbl> 2434.2, 4395.8, 6363.0, 8097.0,~
# $ cum_p_proj_all_ages                  <dbl> -10.16, -4.34, -2.59, -1.17, 0.~
# $ p_proj_all_ages                      <dbl> -10.16, 2.88, 1.31, 4.04, 7.00,~
# $ p_proj_0_14                          <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ p_proj_15_64                         <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ p_proj_65_74                         <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ p_proj_75_84                         <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ p_proj_85p                           <dbl> NA, NA, NA, NA, NA, NA, NA, NA,~
# $ cum_excess_per_million_proj_all_ages <dbl> -86.59373, -66.83691, -57.79922~
# $ excess_per_million_proj_all_ages     <dbl> -86.593735, 19.756823, 9.037696~
# $ deaths_2022_all_ages                 <dbl> 3294, 2752, 1892, NA, NA, NA, N~
# $ deaths_2020_2022_all_ages            <dbl> 2187, 2018, 1993, 1804, 1803, 1~

# Excess mortality is a term used in epidemiology and public health that refers
# to the number of deaths from all causes during a crisis above and beyond what
# we would have expected to see under ‘normal’ conditions.
  
excess_mortality_economist_estimates <- read.csv(
  paste0(lokalizacja,"/data/excess_mortality/excess_mortality_economist_estimates.csv"),
  header = TRUE,
  sep = ",")
excess_mortality_economist_estimates$date <- as.Date(excess_mortality_economist_estimates$date)
# Rows: 33,120
# Columns: 14
# $ country                                                     <chr> "Aruba", "Arub~
# $ date                                                        <date> 2020-01-01, 2~
# $ cumulative_estimated_daily_excess_deaths                    <dbl> 0.0000000, 0.0~
# $ cumulative_estimated_daily_excess_deaths_ci_95_top          <dbl> 0.0000000, 0.0~
# $ cumulative_estimated_daily_excess_deaths_ci_95_bot          <dbl> 0.0000000, 0.0~
# $ cumulative_estimated_daily_excess_deaths_per_100k           <dbl> 0.0000000, 0.0~
# $ cumulative_estimated_daily_excess_deaths_ci_95_top_per_100k <dbl> 0.0000000, 0.0~
# $ cumulative_estimated_daily_excess_deaths_ci_95_bot_per_100k <dbl> 0.0000000, 0.0~
# $ estimated_daily_excess_deaths                               <dbl> 0.00000000, 0.~
# $ estimated_daily_excess_deaths_ci_95_top                     <dbl> 0.00000000, 0.~
# $ estimated_daily_excess_deaths_ci_95_bot                     <dbl> 0.00000000, 0.~
# $ estimated_daily_excess_deaths_per_100k                      <dbl> 0.000000000, 0~
# $ estimated_daily_excess_deaths_ci_95_top_per_100k            <dbl> 0.000000000, 0~
# $ estimated_daily_excess_deaths_ci_95_bot_per_100k            <dbl> 0.000000000, 0~

full_data_Johns_Hopkins_University <- read.csv(
  paste0(lokalizacja,"/data/jhu/full_data.csv"),
  header = TRUE,
  sep = ",")
full_data_Johns_Hopkins_University$date <- as.Date(full_data_Johns_Hopkins_University$date)
# Rows: 212,541
# Columns: 10
# $ date            <date> 2020-02-24, 2020-02-25, 2020-02-26, 2020-02-2~
# $ location        <chr> "Afghanistan", "Afghanistan", "Afghanistan", "~
# $ new_cases       <dbl> 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0~
# $ new_deaths      <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
# $ total_cases     <dbl> 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 8, 8, 8, 8~
# $ total_deaths    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
# $ weekly_cases    <dbl> NA, NA, NA, NA, NA, 5, 5, 0, 0, 0, 0, 0, 3, 3,~
# $ weekly_deaths   <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
# $ biweekly_cases  <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~
# $ biweekly_deaths <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA~









