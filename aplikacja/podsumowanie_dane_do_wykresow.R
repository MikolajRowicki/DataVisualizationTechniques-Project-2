#setwd("C:\\Users\\HP\\R\\TWD_semestr3\\DataVisualizationTechniques-Project-2\\aplikacja")

library(dplyr)
library(treemap)
library(plotly)

### pierwszy wykres -> zakładka ogólna


word1 <- read.csv("Malgosia-word.csv")
word2 <- read.csv("Mikolaj-word.csv")
word3 <- read.csv("Sebastian-word.csv")

matlab1 <- read.csv("Malgosia_matlab.csv")
matlab2 <- read.csv("Sebastian_matlab.csv")
matlab3 <- read.csv("Mikolaj_matlab.csv")

java1 <- read.csv("Sebastian_java.csv")
java2 <- read.csv("Sebastian2_java.csv")
java4 <- read.csv("Sebastian3_java.csv")
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

java2 <- java2 %>% 
  mutate(Imie = "Sebastian") %>% 
  mutate(data = Data_ostatniej_modefikacji) %>% 
  select(Imie, data, Rozszerzenie)

java3 <- java3 %>% 
  mutate(data = Data_ostatniej_modefikacji) %>% 
  select(Imie, data, Rozszerzenie)

java4 <- java4 %>% 
  mutate(Imie = "Sebastian") %>% 
  mutate(data = Data_ostatniej_modefikacji) %>% 
  select(Imie, data, Rozszerzenie)

java5 <- java5 %>% 
  mutate(data = Data_ostatniej_modefikacji) %>% 
  select(Imie, data, Rozszerzenie)

java <- rbind(java1, java2, java3, java4, java5)

df <- rbind(java, matlab, word)



df$month <- as.integer(substring(df$data, 6, 7))
df$year <- as.integer(substring(df$data, 1, 4))
df$month <- sprintf("%d", df$month)


df <- df %>% 
  select(year, month, Rozszerzenie, Imie) %>% 
  group_by(year, month, Rozszerzenie, Imie) %>% 
  summarise(n = n())
full <- expand.grid(year = unique(df$year), Imie = unique(df$Imie), Rozszerzenie = unique(df$Rozszerzenie), month = 1:12)
df <- merge(df, full, by = c("year", "month", "Imie", "Rozszerzenie"), all = TRUE)
df[is.na(df)] <- 0
df$year <- as.integer(df$year)
df$month <- as.integer(df$month)
df <- df %>% 
  arrange(month)%>% 
  arrange(year)

#write.csv(df, "ogolny_wykres1.csv")

### drugi wykres -> rozkład plików dla danej osoby

df1 <- rbind(java, matlab, word)
df1 <- df1 %>% 
  select(Imie, Rozszerzenie) %>% 
  group_by(Imie, Rozszerzenie) %>% 
  summarise(n = n())

#write.csv(df1, "ogolny_wykres2.csv")


