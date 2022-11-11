library(dplyr)
library(ggplot2)
library(scatterpie) # do rysowania wykresow kolowych jako punktow
library(plotly)
library(e1071) # Bayes
library(titanic)
# knitr::kable(head(titanic_train))

Titanic # Zagregowana tabela
titanic_train %>% colnames()
titanic_test %>% colnames() # tutaj brak kolumny "Survived"


titanic_train %>% glimpse()
# Rows: 891
# Columns: 12
# $ PassengerId <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 1~
# $ Survived    <int> 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1~
# $ Pclass      <int> 3, 1, 3, 1, 3, 3, 1, 3, 3, 2, 3, 1, 3, 3, 3, 2, 3, 2~
# $ Name        <chr> "Braund, Mr. Owen Harris", "Cumings, Mrs. John Bradl~
# $ Sex         <chr> "male", "female", "female", "female", "male", "male"~
# $ Age         <dbl> 22, 38, 26, 35, 35, NA, 54, 2, 27, 14, 4, 58, 20, 39~
# $ SibSp       <int> 1, 1, 0, 1, 0, 0, 0, 3, 0, 1, 1, 0, 0, 1, 0, 0, 4, 0~
# $ Parch       <int> 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 1, 0, 0, 5, 0, 0, 1, 0~
# $ Ticket      <chr> "A/5 21171", "PC 17599", "STON/O2. 3101282", "113803~
# $ Fare        <dbl> 7.2500, 71.2833, 7.9250, 53.1000, 8.0500, 8.4583, 51~
# $ Cabin       <chr> "", "C85", "", "C123", "", "", "E46", "", "", "", "G~
# $ Embarked    <chr> "S", "C", "S", "S", "S", "Q", "S", "S", "S", "C", "S~

# 1. Zwizualizować szanse na przeżycie katastrofy w zależności od klasy, którą podróżował pasażer, od płci, oraz od obu tych cech jednocześnie. ----
df <- titanic_train %>% select(Survived, Pclass, Sex)
# df$Survived <- df$Survived %>% as.logical()
df$Pclass <- df$Pclass %>% as.character()
df %>% glimpse()

# Odsetek uratowanych według klasy
surv_by_class <- df %>%
  select(!Sex) %>% 
  group_by(Pclass) %>% 
  # summarise(Prob_surv = sum(Survived)/length(Pclass)) %>% 
  transmute(Prob_surv = sum(Survived)/length(Pclass),
            Pclass = factor(Pclass, levels= c("3","2","1"))) %>% 
  ungroup() %>% 
  distinct() %>% 
  arrange(Pclass)
  
ggplot(surv_by_class, aes(x= Pclass, y= Prob_surv, fill= Prob_surv)) +
  geom_bar(stat = "identity") + 
  geom_col(colour = "black") +
  coord_flip() +
  # scale_x_reverse() +
  scale_y_continuous(labels= scales::percent) +
  ggtitle("Odsetek przeżyć według klasy") +
  xlab("Klasa pasażerska") +
  ylab("") +
  scale_fill_viridis_c(begin= 0.7,
                       direction= -1) +
  geom_text(aes(label= paste0(round(Prob_surv*100,2),"%")), hjust = 1.15, size= 5) +
  theme(legend.position="none")

# Szanse na przeżycie według klasy (Naiwny Bayes)
nb_class <- naiveBayes(formula = Survived~Pclass, data = df)
bayes_class <- nb_class$tables %>%
  as.data.frame() %>% 
  mutate(Pclass.Y = if_else(Pclass.Y == "1", "Live", "Dead"))

ggplot(bayes_class, aes(x= Pclass.Pclass, y= Pclass.Freq, fill= Pclass.Y)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  # coord_flip() +
  # scale_x_reverse() +
  scale_y_continuous(labels= scales::percent, name="") +
  labs(title="Szanse na przeżycie w podziale na klasę \n (Twierdzenie Bayes'a)",
       x ="Klasa pasażerska", y = "", fill = "") +
  geom_text(aes(label= paste0(round(Pclass.Freq*100,2),"%")),
            size= 5,
            position = position_dodge(0.9),
            vjust = 1.5)

# Odsetek uratowanych według płci
surv_by_sex <- df %>%
  select(!Pclass) %>% 
  group_by(Sex) %>% 
  transmute(Prob_surv = sum(Survived)/length(Sex),
            Sex = factor(Sex, levels= c("male","female"))) %>% 
  ungroup() %>% 
  distinct()

ggplot(surv_by_sex, aes(x= Sex, y= Prob_surv, fill= Prob_surv)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels= scales::percent) +
  labs(title="Odsetek przeżyć według płci",
       x ="", y = "") +
  scale_fill_viridis_c(begin= 0.7,
                       direction= -1) +
  geom_text(aes(label= paste0(round(Prob_surv*100,2),"%")), hjust = 1.15, size= 5) +
  theme(legend.position="none")

# Szanse na przeżycie według płci (Naiwny Bayes)
nb_sex <- naiveBayes(formula = Survived~Sex, data = df)
bayes_sex <- nb_sex$tables %>%
  as.data.frame() %>% 
  mutate(Sex.Y = if_else(Sex.Y == "1", "Live", "Dead"))

ggplot(bayes_sex, aes(x= Sex.Sex, y= Sex.Freq, fill= Sex.Y)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  # coord_flip() +
  # scale_x_reverse() +
  scale_y_continuous(labels= scales::percent, name="") +
  labs(title="Szanse na przeżycie w podziale na płeć \n (Twierdzenie Bayes'a)",
       x ="Płeć pasażera", y = "", fill = "") +
  geom_text(aes(label= paste0(round(Sex.Freq*100,2),"%")),
            size= 5,
            position = position_dodge(0.9),
            vjust = 1.5)

# Szanse na przeżycie katastrofy w zależności od klasy i płci
# Matematycznie, wydaje mi się, że jest to trudne do osiągnięcia. Zostaje prosta wizualizacja
df_count <- df %>%
  group_by(Survived, Pclass, Sex) %>% 
  summarise(n= n()) %>% 
  ungroup() %>% 
  mutate(Survived = if_else(Survived == 1, "Live", "Dead"),
         Pclass = if_else(Pclass == '1', 1L, if_else(Pclass == '2', 2L, 3L)),
         Sex = if_else(Sex == 'female', 1L, 0L)) %>% 
  tidyr::pivot_wider(names_from = Survived, values_from = n) %>% 
  mutate(All = Dead+Live,
         All_r = All/sum(All))

ggplot() +
  geom_scatterpie(aes(x= Pclass, y= Sex, r= All_r),
                  data= df_count,
                  cols= c("Dead", "Live")) +
  coord_equal() +
  scale_x_discrete(limit = c(1,2,3)) +
  scale_y_discrete(limit = c(1,0),
                   labels = c("female", "male")) +
  labs(title="Statystyki umieralności w podziale na płeć i klasy",
       y ="", x = "Klasa pasażerska", fill = "")

# 2. A co z zasadą najpierw kobiety i dzieci? ----
df_age_count <- titanic_train %>%
  select(Survived, Pclass, Sex, Age) %>%
  group_by(Survived, Pclass, Sex) %>% 
  # Uzupełnienie braków średnią z danej kategorii
  mutate(Age= coalesce(Age, round(mean(Age, na.rm = TRUE)))) %>%
  ungroup() %>% 
  mutate(Survived = if_else(Survived == 1, "Live", "Dead"))

plot_ly(df_age_count,
        x= ~Pclass,
        y= ~Sex,
        z= ~Age,
        color= ~Survived,
        colors= c('Red', 'Green'),
        hoverinfo = 'text',
        text = ~paste('</br> Pclass: ', Pclass,
                      '</br> Sex: ', Sex,
                      '</br> Age: ', Age)) %>% 
  add_markers() %>% 
  layout(title = "Wykres umieralności w podziale na płeć, klasy i wiek",
         scene = list(aspectmode = "manual", aspectratio = list(x=.5, y=.5, z=2)),
         legend = list(orientation = 'h'))
# (*) Zastanowić się czy wizualizacja wzglęgem liczby osób, które przeżyły jest tu adekwatna? ----
# Wykorzystałem to do "2."









