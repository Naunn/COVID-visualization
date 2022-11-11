library(dplyr)
library(ggplot2)

udemy <- read.csv("zad5/Entry Level Project Sheet - 3.1-data-sheet-udemy-courses-web-development.csv",
         sep = ",") %>% 
  mutate(Date = as.Date(Date))
udemy %>% glimpse()
# Rows: 3,676
# Columns: 14
# $ course_id           <int> 41295, 59014, 625204, 173548, 764164, 19421, 473160, 94430, 130064, 364426, 238934,…
# $ course_title        <chr> "Learn HTML5 Programming From Scratch", "Coding for Entrepreneurs Basic", "The Web …
# $ url                 <chr> "https://www.udemy.com/learn-html5-programming-from-scratch/", "https://www.udemy.c…
# $ price               <int> 0, 0, 200, 0, 200, 0, 0, 30, 0, 175, 200, 190, 0, 0, 0, 120, 0, 0, 0, 175, 195, 195…
# $ num_subscribers     <int> 268923, 161029, 121584, 120291, 114512, 101154, 98867, 84897, 83737, 79612, 75499, …
# $ num_reviews         <int> 8629, 279, 27445, 5924, 22412, 1042, 6512, 2685, 4598, 16976, 7676, 19649, 1716, 25…
# $ num_lectures        <int> 45, 27, 342, 30, 304, 95, 20, 10, 45, 85, 362, 329, 22, 21, 50, 197, 24, 17, 24, 55…
# $ level               <chr> "Beginner Level", "Expert Level", "Beginner Level", "All Levels", "Beginner Level",…
# $ rating              <dbl> 0.82, 0.69, 0.89, 0.78, 0.55, 0.88, 0.82, 0.79, 0.85, 0.69, 0.96, 0.90, 0.94, 0.39,…
# $ content_duration    <dbl> 10.5, 3.5, 43.0, 3.0, 30.5, 4.5, 3.0, 2.0, 6.5, 11.5, 30.0, 22.0, 2.0, 1.0, 6.0, 27…
# $ published_timestamp <chr> "2013-02-14T07:03:41Z", "2013-06-09T15:51:55Z", "2015-11-02T21:13:27Z", "2014-04-08…
# $ subject             <chr> "Web Development", "Web Development", "Web Development", "Web Development", "Web De…
# $ Date                <date> 2013-02-14, 2013-06-09, 2015-11-02, 2014-04-08, 2016-03-08, 2012-06-15, 2015-04-13…
# $ Free.Paid           <chr> "Free", "Free", "Paid", "Free", "Paid", "Free", "Free", "Paid", "Free", "Paid", "Pa…

# Zobrazować zmianę ilości proponowanych kursów w kolejnych latach w poszczególnych tematykach.
udemy_interest <- udemy %>% 
  select(Date, subject) %>% 
  transmute(Year = format(Date, "%Y"),
            subject = subject) %>%
  group_by(Year, subject) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct()

ggplot(udemy_interest, aes(x= Year, y= n, group= subject)) +
  geom_line(aes(color= subject)) +
  geom_point(aes(color= subject)) +
  labs(title="Ilości kursów \n (w podziale na tematykę)", y ="") +
  theme(legend.position = c(0.2, 0.79),
        legend.title = element_blank(),
        aspect.ratio=5/7)

# Jak w kolejnych latach zmieniała się proporcja ilości kurów proponowanych na różnych poziomach zaawansowania w poszczególnych tematykach?
udemy_interest_level <- udemy %>% 
  select(Date, subject, level) %>% 
  transmute(Year = format(Date, "%Y"),
            subject = subject,
            level = level) %>%
  group_by(Year, subject, level) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  distinct()

ggplot(udemy_interest_level, aes(x= Year, y= n, fill= level)) +
  geom_bar(stat = "identity", position= "dodge") +
  labs(title= "Ilości kursów \n (w podziale na tematykę i poziom)",
       y= "",
       x= "") +
  geom_text(aes(label= n),
            size= 3,
            position = position_dodge(0.9),
            vjust = -.25) +
  facet_wrap(~subject,nrow=4) +
  theme(legend.position="none") +
  ylim(0,250)
  
# Jak się zmieniała w kolejnych latach cena kursu w zależności od tematyki (i/lub) stopnia zaawansowania?
udemy_interest_price <- udemy %>% 
  select(Date, subject, level, price) %>% 
  transmute(Year = format(Date, "%Y"),
            subject = subject,
            level = level,
            price = price) %>%
  group_by(Year, subject, level) %>% 
  mutate(mean_price = round(mean(price),2)) %>% 
  select(!price) %>% 
  ungroup() %>% 
  distinct()

ggplot(udemy_interest_price, aes(x= Year, y= mean_price, fill= level)) +
  geom_bar(stat = "identity", position= "dodge") +
  labs(title= "Średnia cena kursów \n (w podziale na tematykę i poziom)",
       y= "",
       x= "") +
  geom_text(aes(label= paste0(mean_price,"$")),
            size= 2,
            position = position_dodge(0.9),
            vjust = -.25) +
  facet_wrap(~subject,nrow=4) +
  theme(legend.position="none") +
  ylim(0,120)






















