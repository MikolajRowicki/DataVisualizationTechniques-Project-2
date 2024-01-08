#setwd("C:\\Users\\HP\\R\\TWD_semestr3\\DataVisualizationTechniques-Project-2\\aplikacja")
library(dplyr)
library(tidyr)
library(ggplot2)
library(extrafont)

### wczytanie potrzebnych danych:

malgosia_word <- read.csv("Malgosia-word.csv")
sebastian_word <- read.csv("Sebastian-word.csv")
mikolaj_word <- read.csv("Mikolaj-word.csv")

word <- rbind(malgosia_word, sebastian_word, mikolaj_word)


### ramka danych do pierwszego wykresu (tworzenie plikow)

word1 <- word %>% 
  select(Imie, Data.utworzenia.pliku)

word1$month <- as.integer(substring(word1$Data.utworzenia.pliku, 6, 7))
word1$year <- as.integer(substring(word1$Data.utworzenia.pliku, 1, 4))
word1$day <- as.integer(substring(word1$Data.utworzenia.pliku, 9, 10))
word1$month <- sprintf("%d", word1$month)

word1 <- word1 %>% 
  select(year, month, Imie) %>% 
  group_by(year, month, Imie) %>% 
  summarise(n = n())


full <- expand.grid(year = unique(word1$year), month = 1:12, Imie = unique(word1$Imie))
word1 <- merge(word1, full, by = c("year", "month", "Imie"), all = TRUE)
word1[is.na(word1)] <- 0
word1$year <- as.integer(word1$year)
word1$month <- as.integer(word1$month)
word1 <- word1 %>% 
  arrange(month)%>% 
  arrange(year)

word1$data <- as.Date(paste(word1$year, word1$month, "01", sep = "-"), format = "%Y-%m-%d")
word1 <- word1 %>% 
  filter(Imie == "Malgosia")

ggplot(word1, aes(data, y = n)) +
  geom_col(color = "darkgreen", fill = "darkgreen", width = 22) +
  labs(title = "tworzenie plików word",
       x = "Czas",
       y = "Ilość") +
  scale_y_continuous(breaks = seq(0, 12, by = 2)) + 
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    panel.grid.major = element_line(size = 0.5, color = "#66685f"),
    panel.grid.minor = element_blank(),#31322e
    plot.title = element_text(family = "Consolas", size = 22, hjust = 0.5, colour = "black"),
    axis.title = element_text(family = "Consolas", size = 16, color = "black")
    
  )


#write.csv(word1, "word_wykres1.csv")

### ramka danych do drugiego wykresu (interpunkcja)

word2 <- word %>% 
  group_by(Imie) %>% 
  summarise(kropka = sum(Ilosc.kropek), przecinek = sum(Ilosc.przecinkow), dwukropek = sum(Ilosc.dwukropkow), pozostale = sum(Ilosc.pozostalych.znakow), pytahjnik = sum(Ilosc.pytajnikow), wykrzyknik = sum(Ilosc.wykrzyknikow), myslnik= sum(Ilosc.myslnikow))
colnames(word2)[colnames(word2) == "pytahjnik"] <- "Pytajnik"
colnames(word2)[colnames(word2) == "wykrzyknik"] <- "Wykrzyknik"
colnames(word2)[colnames(word2) == "pozostale"] <- "Pozostałe"
colnames(word2)[colnames(word2) == "dwukropek"] <- "Dwukropek"
colnames(word2)[colnames(word2) == "myslnik"] <- "Myślnik"
colnames(word2)[colnames(word2) == "kropka"] <- "Kropka"
colnames(word2)[colnames(word2) == "przecinek"] <- "Przecinek"


#write.csv(word2, "word_wykres2.csv")

### ramka danych do trzeciego wykresu (kropki / przecinki)

word3 <- word %>% 
  select(Ilosc.kropek, Ilosc.słow., Ilosc.przecinkow, Imie)

#write.csv(word3, "word_wykres3.csv")

