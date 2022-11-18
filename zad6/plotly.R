library(titanic)
library(ggplot2)
library(dplyr)
library(babynames)

titanic <- titanic_train

# 1. Poprawić wykres
ggplot(data = titanic) +
  geom_histogram(mapping = aes(x = Pclass), color = "black", fill = "green", binwidth = 0.5) +
  scale_x_continuous(name = "Klasa", limits = c(0,4),breaks = c(1, 2, 3)) +
  ylab("Liczba osób") + ggtitle("Liczba osób w klasach")

p1 <- ggplot(data = titanic) +
  geom_histogram(mapping = aes(x = Pclass), color = "black", fill = "green", binwidth = 0.5) + 
  scale_x_continuous(name = "Klasa", limits = c(0,4),breaks = c(1, 2, 3))+ylab("Liczba osób") + 
  ggtitle("Liczba osób w klasach")
fig1 <- ggplotly(p1, tooltip="count")
fig1

# 2. Poprawić wykres
nms <- filter(babynames, name %in% c("Sam", "Alex"))
nms %>% glimpse()

ggplot(nms, aes(year, prop, color = sex, linetype = name)) + 
  geom_line()

p2 <- ggplot(nms, aes(year, prop, color = sex, linetype = name)) + 
  geom_line()
fig2 <- ggplotly(p2, tooltip=c("prop", "sex", "name"))
fig2 <- fig2 %>% layout(hovermode = "x unified", showlegend = F)
fig2
