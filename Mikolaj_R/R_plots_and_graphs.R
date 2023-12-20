library(dplyr)
library(ggplot2)

mikolaj_matlab <- read.csv("data/Mikolaj_matlab.csv")
sebastian_matlab <- read.csv("data/Sebastian_matlab.csv")

mikolaj_matlab <- mikolaj_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)

sebastian_matlab <- sebastian_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)

sebastian_java <- read.csv("data/Sebastian_java.csv")
mikolaj_java <- read.csv("data/Mikolaj_java.csv")

p1 <- ggplot() +
  geom_point(data = mikolaj_matlab, aes(x = Liczba.wierszy, y = Liczba.znaków., colour = "Mikołaj"), alpha = 0.7) +
  geom_point(data = sebastian_matlab, aes(x = Liczba.wierszy, y = Liczba.znaków., colour = "Sebastian"), alpha = 0.7) +
  labs(title = "Zależność liczby znaków od liczby wierszy", x = "Liczba wierszy", y = "Łączna liczba znaków w pliku", colour = "Autor")+
  theme_minimal() +
  theme(legend.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 20)), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = 30)))

p2 <- ggplot() +
  geom_point(data = mikolaj_matlab, aes(x = Liczba.wierszy, y = Laczna.dlugosc.komentarzy, colour = "Mikołaj"), alpha = 0.7) +
  geom_point(data = sebastian_matlab, aes(x = Liczba.wierszy, y = Laczna.dlugosc.komentarzy, colour = "Sebastian"), alpha = 0.7) +
  labs(title = "Zależność łącznej długości komentarzy od liczby wierszy", x = "Liczba wierszy", y = "Łączna długośc komentarzy", colour = "Autor")+
  theme_minimal() +
  theme(legend.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 20)), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = 30)))

p3 <- ggplot() +
  geom_point(data = mikolaj_matlab, aes(x = Liczba.operatorow, y = Liczba.operatorow.otoczonych.spacjami, colour = "Mikołaj"), alpha = 0.7) +
  geom_point(data = sebastian_matlab, aes(x = Liczba.operatorow, y = Liczba.operatorow.otoczonych.spacjami, colour = "Sebastian"), alpha = 0.7) +
  labs(title = "Konwencja zapisu operatorów matematycznych", x = "Liczba użytych operatorów matematycznych", y = "Liczba operatorów zapisanych ze spacjami wokół nich", colour = "Autor")+
  theme_minimal() +
  theme(legend.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 20)), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = 30)))

p4 <- ggplot() +
  geom_point(data = mikolaj_matlab, aes(x = Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, y = Liczba.wierszy.zakonczonych.srednikiem, colour = "Mikołaj"), alpha = 0.7) +
  geom_point(data = sebastian_matlab, aes(x = Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, y = Liczba.wierszy.zakonczonych.srednikiem, colour = "Sebastian"), alpha = 0.7) +
  labs(title = "Stawianie średników na końcu linii", x = "Liczba wierszy, które powinny sie kończyć średnikiem", y = "Liczba wierszy, które kończą się średnikiem", colour = "Autor")+
  theme_minimal() +
  theme(legend.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 20)), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = 30)))

p5 <- ggplot() +
  geom_point(data = mikolaj_matlab, aes(y = Laczna.dlugosc.komentarzy, x = Długośc.pierwszego.komentarza, colour = "Mikołaj"), alpha = 0.7) +
  geom_point(data = sebastian_matlab, aes(y = Laczna.dlugosc.komentarzy, x = Długośc.pierwszego.komentarza, colour = "Sebastian"), alpha = 0.7) +
  labs(title = "Zależność łącznej długości komentarzy od długości pierwszego", x = "Długość pierwszego komentarza", y = "Długość wszystkich komentarzy łącznie", colour = "Autor")+
  theme_minimal() +
  theme(legend.title = element_text(hjust = 0.5),
        axis.title.y = element_text(margin = margin(r = 30)),
        axis.title.x = element_text(margin = margin(t = 20)), 
        plot.title = element_text(hjust = 0.5, margin = margin(b = 30)))
