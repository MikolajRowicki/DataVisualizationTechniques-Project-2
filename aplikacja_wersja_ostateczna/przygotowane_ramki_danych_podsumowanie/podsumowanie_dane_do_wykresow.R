#setwd("C:\\Users\\HP\\R\\TWD_semestr3\\DataVisualizationTechniques-Project-2\\aplikacja")

library(dplyr)
### pierwszy wykres -> zakładka ogólna


word1 <- read.csv("Malgosia-word.csv")
word2 <- read.csv("Mikolaj-word.csv")
word3 <- read.csv("Sebastian-word.csv")

matlab1 <- read.csv("Malgosia_matlab.csv")
matlab2 <- read.csv("Sebastian_matlab.csv")
matlab3 <- read.csv("Mikolaj_matlab.csv")

java1 <- read.csv("Sebastian_java.csv")
java3 <- read.csv("Mikolaj_java.csv")
java5 <- read.csv("Malgosia_java.csv")

word <- rbind(word1, word2, word3)
matlab <- rbind(matlab1, matlab2, matlab3)

word <- word %>% 
  mutate(data = Data.utworzenia.pliku) %>% 
  select(Imie, data, Rozszerzenie)

matlab <- matlab %>% 
  mutate(data = Data.modyfikacji) %>% 
  select(Imie, data, Rozszerzenie)

java1 <- java1 %>% 
  mutate(data = Data_ostatniej_modefikacji) %>% 
  select(Imie, data, Rozszerzenie)

java3 <- java3 %>% 
  mutate(data = Data_ostatniej_modefikacji) %>% 
  select(Imie, data, Rozszerzenie)

java5 <- java5 %>% 
  mutate(data = Data_ostatniej_modefikacji) %>% 
  select(Imie, data, Rozszerzenie)

java <- rbind(java1, java3, java5)
x <- rbind(java, matlab, word)
write.csv(x, "ogolny_wykres2.csv")
x$month <- as.integer(substring(x$data, 6, 7))
x$year <- as.integer(substring(x$data, 1, 4))
x$month <- sprintf("%d", x$month)


x <- x %>% 
  select(year, month, Rozszerzenie, Imie) %>% 
  group_by(year, month, Rozszerzenie, Imie) %>% 
  summarise(n = n())
full <- expand.grid(year = unique(x$year), Imie = unique(x$Imie), Rozszerzenie = unique(x$Rozszerzenie), month = 1:12)
x <- merge(x, full, by = c("year", "month", "Imie", "Rozszerzenie"), all = TRUE)
x$year <- as.integer(x$year)
x$month <- as.integer(x$month)
x[is.na(x)] <- 0



write.csv(x, "ogolny_wykres1.csv")

### drugi wykres -> rozkład plików dla danej osoby

