library(titanic)
library(ggplot2)
library(dplyr)
library(babynames)
library(plotly)

titanic <- titanic_train

# 1. Poprawić wykres ====
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

# 2. Poprawić wykres ====
nms <- filter(babynames, name %in% c("Sam", "Alex"))
nms %>% glimpse()

ggplot(nms, aes(year, prop, color = sex, linetype = name)) + 
  geom_line()

p2 <- ggplot(nms, aes(year, prop, color = sex, linetype = name)) + 
  geom_line()
fig2 <- ggplotly(p2, tooltip=c("year","prop", "sex", "name"))
fig2 <- fig2 %>% layout(hovermode = "x unified", showlegend = F)
fig2
 # Zrobimy od nowa ...
nms_wide <- nms %>% tidyr::pivot_wider(names_from = name, values_from = c(n, prop))

fig1 <- plot_ly(nms_wide %>% filter(sex == "M"),
                x = ~year,
                y = ~prop_Sam,
                type = "scatter",
                mode = "lines",
                line = list(color = "aqua"),
                hovertemplate = paste("Sam: %{y}<extra></extra>")) %>%
  add_trace(x = ~year,
            y = ~prop_Alex,
            line = list(color = "blue"),
            hovertemplate = paste("Alex: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".3%"),
         hovermode = "x unified",
         showlegend = FALSE)

fig2 <- plot_ly(nms_wide %>% filter(sex == "F"),
                x = ~year,
                y = ~prop_Sam,
                type = "scatter",
                mode = "lines",
                line = list(color = "red"),
                hovertemplate = paste("Sam: %{y}<extra></extra>")) %>%
  add_trace(x = ~year,
            y = ~prop_Alex,
            line = list(color = "pink"),
            hovertemplate = paste("Alex: %{y}<extra></extra>")) %>% 
  layout(yaxis = list(tickformat = ".4%"),
         hovermode = "x unified",
         showlegend = FALSE)

fig <- subplot(fig1, fig2, nrows = 2) %>%
  layout(title = "Proportion of babynames by sex",
         showlegend = FALSE,
         plot_bgcolor='#e5ecf6')
fig
