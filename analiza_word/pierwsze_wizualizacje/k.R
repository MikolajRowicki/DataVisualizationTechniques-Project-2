R <- read.csv("C:\\Users\\HP\\PycharmProjects\\skrypty\\R analiza\\Malgosia-R.csv")
word <- read.csv("C:\\Users\\HP\\PycharmProjects\\skrypty\\word analiza\\Malgosia-word.csv")
matlab <- read.csv("C:\\Users\\HP\\Downloads\\Mikolaj_matlab.csv")
java <- read.csv("C:\\Users\\HP\\Downloads\\Mikolaj_java.csv")
drugi_word <- read.csv("C:\\Users\\HP\\Downloads\\Sebastian-word.csv")


library(ggplot2)
library(dplyr)
library(tidyr)
### OŚ CZASU WORD

word$month <- as.integer(substring(word$Data.utworzenia.pliku, 6, 7))
word$year <- as.integer(substring(word$Data.utworzenia.pliku, 1, 4))
word$month <- sprintf("%d", word$month)
word1 <- word %>% 
  select(year, month, Rozszerzenie) %>% 
  group_by(year, month, Rozszerzenie) %>% 
  summarise(n = n())
full <- expand.grid(year = unique(word1$year), month = 1:12)
word1 <- merge(word1, full, by = c("year", "month"), all = TRUE)
word1[is.na(word1)] <- 0
word1$year <- as.integer(word1$year)
word1$month <- as.integer(word1$month)
word1 <- word1 %>% 
  arrange(month)%>% 
  arrange(year)  %>% 
  mutate(Rozszerzenie = "docx")

word1$data <- as.Date(paste(word1$year, word1$month, "01", sep = "-"), format = "%Y-%m-%d")

drugi_word$month <- as.integer(substring(drugi_word$Data.utworzenia.pliku, 6, 7))
drugi_word$year <- as.integer(substring(drugi_word$Data.utworzenia.pliku, 1, 4))
drugi_word$month <- sprintf("%d", drugi_word$month)
drugi_word1 <- drugi_word %>% 
  select(year, month, Rozszerzenie) %>% 
  group_by(year, month, Rozszerzenie) %>% 
  summarise(n = n())
full <- expand.grid(year = unique(drugi_word1$year), month = 1:12)
drugi_word1 <- merge(drugi_word1, full, by = c("year", "month"), all = TRUE)
drugi_word1[is.na(drugi_word1)] <- 0
drugi_word1$year <- as.integer(drugi_word1$year)
drugi_word1$month <- as.integer(drugi_word1$month)
drugi_word1 <- drugi_word1 %>% 
  arrange(month)%>% 
  arrange(year)  %>% 
  mutate(Rozszerzenie = "docx")

drugi_word1$data <- as.Date(paste(drugi_word1$year, drugi_word1$month, "01", sep = "-"), format = "%Y-%m-%d")

#kolumnowy

ggplot(word1, aes(data, y = n)) +
  geom_col(color = "darkgreen", fill = "darkgreen") +
  labs(title = "tworzenie plików word",
       x = "Czas",
       y = "Ilość") +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) + 
  theme_minimal()


### oś czasu wszystkie rozszerzenia

matlab$month <- as.integer(substring(matlab$Data.modyfikacji, 6, 7))
matlab$year <- as.integer(substring(matlab$Data.modyfikacji, 1, 4))
matlab$month <- sprintf("%d", matlab$month)
matlab1 <- matlab %>% 
  select(year, month, Rozszerzenie) %>% 
  group_by(year, month, Rozszerzenie) %>% 
  summarise(n = n())
full <- expand.grid(year = unique(word$year), month = 1:12)
matlab1 <- merge(matlab1, full, by = c("year", "month"), all = TRUE)
matlab1[is.na(matlab1)] <- 0
matlab1$year <- as.integer(matlab1$year)
matlab1$month <- as.integer(matlab1$month)
matlab1 <- matlab1 %>% 
  arrange(month)%>% 
  arrange(year) %>% 
  mutate(Rozszerzenie = "m")

matlab1$data <- as.Date(paste(matlab1$year, matlab1$month, "01", sep = "-"), format = "%Y-%m-%d")


R$month <- as.integer(substring(R$Data.utworzenia.pliku, 6, 7))
R$year <- as.integer(substring(R$Data.utworzenia.pliku, 1, 4))
R$month <- sprintf("%d", R$month)
R1 <- R %>% 
  select(year, month, Rozszerzenie) %>% 
  group_by(year, month, Rozszerzenie) %>% 
  summarise(n = n())
full <- expand.grid(year = unique(R1$year), month = 1:12)
R1 <- merge(R1, full, by = c("year", "month"), all = TRUE)
R1[is.na(R1)] <- 0
R1$year <- as.integer(R1$year)
R1$month <- as.integer(R1$month)
R1 <- R1 %>% 
  arrange(month)%>% 
  arrange(year) %>% 
  mutate(Rozszerzenie = "R")

R1$data <- as.Date(paste(R1$year, R1$month, "01", sep = "-"), format = "%Y-%m-%d")


R1 <- R1 %>% 
  select(year, month, data, n, Rozszerzenie)

matlab1 <- matlab1 %>% 
  select(year, month, data, n, Rozszerzenie)

word1 <- word1 %>% 
  select(year, month, data, n, Rozszerzenie)

java1 <- java1 %>% 
  select(year, month, data, n, Rozszerzenie)

m <- rbind(R1, matlab1, word1, java1)


#histogram
full <- expand.grid(year = unique(java$year), month = 1:12)
java2 <- java %>% 
  select(year, month, Rozszerzenie)
java2 <- merge(java2, full, by = c("year", "month"), all = TRUE)
java2$data <- as.Date(paste(java2$year, java2$month, "01", sep = "-"), format = "%Y-%m-%d")
java2 <- java2 %>% 
  mutate(Rozszerzenie = "java")
full <- expand.grid(year = unique(word$year), month = 1:12)
word2 <- word %>% 
  select(year, month, Rozszerzenie)
word2 <- merge(word2, full, by = c("year", "month"), all = TRUE)
word2$data <- as.Date(paste(word2$year, word2$month, "01", sep = "-"), format = "%Y-%m-%d")
word2 <- word2 %>% 
  mutate(Rozszerzenie = "docx")
full <- expand.grid(year = unique(matlab$year), month = 1:12)
matlab2 <- matlab %>% 
  select(year, month, Rozszerzenie)
matlab2 <- merge(matlab2, full, by = c("year", "month"), all = TRUE)
matlab2$data <- as.Date(paste(matlab2$year, matlab2$month, "01", sep = "-"), format = "%Y-%m-%d")
matlab2 <- matlab2 %>% 
  mutate(Rozszerzenie = "m")
full <- expand.grid(year = unique(R$year), month = 1:12)
R2 <- R %>% 
  select(year, month, Rozszerzenie)
R2 <- merge(R2, full, by = c("year", "month"), all = TRUE)
R2$data <- as.Date(paste(R2$year, R2$month, "01", sep = "-"), format = "%Y-%m-%d")
R2 <- R2 %>% 
  mutate(Rozszerzenie = "R")
R2 <- R2 %>% 
  select(year, month, data, Rozszerzenie)

matlab2 <- matlab2 %>% 
  select(year, month, data, Rozszerzenie)

word2 <- word2 %>% 
  select(year, month, data, Rozszerzenie)

java2 <- java2 %>% 
  select(year, month, data, Rozszerzenie)

m2 <- rbind(R2, matlab2, word2, java2)
m2 %>% 
  filter(year > 2020) %>% 
  ggplot(aes(x = data, group = Rozszerzenie, fill = Rozszerzenie)) +
    geom_density(alpha = 0.4) +
    scale_fill_manual(values = c("docx" = "black", "m" = "#f3149a", "R" = "#0920f0", 'java' = "#077a04")) +
    labs(title = "Tworzenie plików - różne rozszerzenia",
        x = "Czas",
        y = "") +
    theme_minimal()


### analiza interpunkcji word
#kolumnowy

word3 <- word %>% 
  summarise(kropka = sum(Ilosc.kropek), przecinek = sum(Ilosc.przecinkow), dwukropek = sum(Ilosc.dwukropkow), pozostale = sum(Ilosc.pozostalych.znakow), pytahjnik = sum(Ilosc.pytajnikow), wykrzyknik = sum(Ilosc.wykrzyknikow), myslnik= sum(Ilosc.myslnikow))
word3 <- gather(word3, key = "Kolumna", value = "Wartość")

ggplot(word3, aes(x = reorder(Kolumna, -Wartość), y = Wartość)) +
  geom_col(fill = "darkgreen") +
  labs(
    title = "Interpunkcja - word",
    x = "Interpunkcja",
    y = "Wartość"
  ) +
  theme_minimal()


### liczba zdań i przecinków word
#scatterplot
w <- word %>% 
  select(Ilosc.kropek, Ilosc.słow., Ilosc.przecinkow, Imie)
ww <- drugi_word %>% 
  select(Ilosc.kropek, Ilosc.słow., Ilosc.przecinkow, Imie)
w <- rbind(ww, w)
w %>% 
  filter(Ilosc.kropek < 100) %>% 
  ggplot(aes(x = Ilosc.kropek, y = Ilosc.przecinkow, size = Ilosc.słow., color = Imie)) +
    geom_point() +
    scale_color_manual(values = c("Malgosia" = "#01741e", "Sebastian" = "#3ff125"))


#Ilość plików z danego przedziału (categories) ... 

categories <- as.factor(c("short", "medium", "long"))

word4 <- word %>% 
  select(Ilosc.słow., Zlozonosc.zdan, Ilosc.kropek, year) %>% 
  mutate(categories = case_when(
    year == 2023 ~ "new",
    year == 2022 ~ "mid",
    .default = 'old'
  ))

word4$categories <- as.factor(word4$categories)

ggplot(word4, aes(x = categories, y = Ilosc.słow.)) +
  geom_boxplot()


### pakiety w R
#kolumnowy


ramka_danych_rozbita <- R %>%
  tidyr::separate_rows(Uzyte.pakiety, sep = ",")
ramka_danych_rozbita$Uzyte.pakiety <- gsub(",", "", ramka_danych_rozbita$Uzyte.pakiety)
ramka_danych_rozbita$Uzyte.pakiety <- gsub("'", "", ramka_danych_rozbita$Uzyte.pakiety)
ramka_danych_rozbita$Uzyte.pakiety <- gsub("]", "", ramka_danych_rozbita$Uzyte.pakiety)
ramka_danych_rozbita$Uzyte.pakiety <- gsub("\\[", "", ramka_danych_rozbita$Uzyte.pakiety)
ramka_danych_rozbita$Uzyte.pakiety <- gsub("\"", "", ramka_danych_rozbita$Uzyte.pakiety)
ramka_danych_rozbita$Uzyte.pakiety <- gsub(" ", "", ramka_danych_rozbita$Uzyte.pakiety)

ramka_danych_rozbita1 <- ramka_danych_rozbita %>% 
  select(Uzyte.pakiety) %>% 
  group_by(Uzyte.pakiety) %>% 
  summarise(n = n()) %>% 
  filter(Uzyte.pakiety != "") %>% 
  arrange(desc(n)) %>% 
  head(10)

ggplot(ramka_danych_rozbita1, aes(x = reorder(Uzyte.pakiety, -n), y = n)) +
  geom_col(width = 0.8, fill = "darkgreen") +
  labs(
    title = "pakiety R",
    x = "nazwa pakietu",
    y = "uzycie pakietu"
  ) +
  theme_minimal()


java$month <- as.integer(substring(java$Data_ostatniej_modefikacji, 6, 7))
java$year <- as.integer(substring(java$Data_ostatniej_modefikacji, 1, 4))
java$month <- sprintf("%d", java$month)
java1 <- java %>% 
  select(year, month, Rozszerzenie) %>% 
  group_by(year, month, Rozszerzenie) %>% 
  summarise(n = n())
full <- expand.grid(year = unique(java1$year), month = 1:12)
java1 <- merge(java1, full, by = c("year", "month"), all = TRUE)
java1[is.na(java1)] <- 0
java1$year <- as.integer(java1$year)
java1$month <- as.integer(java1$month)
java1 <- java1 %>% 
  arrange(month)%>% 
  arrange(year)  %>% 
  mutate(Rozszerzenie = "java")

java1$data <- as.Date(paste(java1$year, java1$month, "01", sep = "-"), format = "%Y-%m-%d")

