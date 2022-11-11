library(dplyr)
library(ggplot2)

# Mamy 5 wiodących firm A, B, C, D, E , F produkcyjnych działających w tym samym sektorze. Ich udział w rynku jest odpowiednio 
# A - 17%
# B - 19%
# C - 18%
# D - 22%
# E - 23%
# Zobrazować podane dane na wykresach słupkowych, słupkowych zgrupowanych, kołowych. Która z wizualizacji jest najtrafniejsza?

udzialy <- tibble(
  sklep = c("A","B","C","D","E"), 
  udzial = c(.17,.19,.18,.22,.24),
  rok = 2022)

# Wykres słupkowy
ggplot(udzialy, aes(x= sklep, y= udzial, fill= udzial)) +
  geom_col() +
  labs(title= "Udziały sklepów w sektorze sprzedaży",
       y= "Udział",
       x= "Sklep") +
  scale_fill_viridis_c(begin= 0.4,
                       end= 0.7,
                       direction= 1) +
  geom_text(aes(label= paste0(round(udzial*100),"%")),
            size= 5,
            position = position_dodge(0.9),
            vjust = 1.5) +
  theme(legend.position="none")

# Wykres słupkowy zgrupowany
ggplot(udzialy, aes(x= rok, y= udzial, fill= sklep)) +
  geom_bar(stat = "identity", position= "dodge") +
  labs(title= "Udziały sklepów w sektorze sprzedaży",
       y= "Udział",
       x= "") +
  geom_text(aes(label= paste0(round(udzial*100),"%")),
            size= 5,
            position = position_dodge(0.9),
            vjust = 1.5) +
  scale_x_discrete(limit = c(2022))

# Wykres kołowy
ggplot(udzialy, aes(x= rok, y= udzial, fill= sklep)) +
  geom_bar(stat = "identity", width=1,  color="black") +
  labs(title= "Udziały sklepów w sektorze sprzedaży",
       y= "",
       x= "") +
  geom_text(aes(label= paste0(round(udzial*100),"%")),
            position = position_stack(vjust = 0.5),
            size= 7,
            color = "white") +
  theme_void() +
  coord_polar("y") +
  theme(plot.title = element_text(hjust = 0.5))




