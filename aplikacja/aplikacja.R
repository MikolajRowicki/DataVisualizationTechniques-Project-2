library(shiny)
library(dplyr)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(rlang)
library(plotly)
library(dashboardthemes)
library(tidyr)
library(treemap)

# Przygotowywane danych --------------------------------------------------------
df1 <- read.csv("Sebastian_java.csv")
df2 <- read.csv("Sebastian2_java.csv")
df3 <- read.csv("Sebastian3_java.csv")
df2$Imie <- "Sebastian"
df3$Imie <- "Sebastian"
df4 <- read.csv("Mikolaj_java.csv")
df <- bind_rows(df1, df2, df3, df4)
df$Data_ostatniej_modefikacji <- as.Date(substr(df$Data_ostatniej_modefikacji,1,10))
colnames(df)[which(names(df) == "if.")] <- "if"
colnames(df)[which(names(df) == "else.")] <- "else"

zmienne <- c("Sebastian", "Malgosia", "Mikolaj")
word_wykres1 <- read.csv("./przygotowane_ramki_danych_do_wykresow_word/word_wykres1.csv")
word_wykres2 <- read.csv(".//przygotowane_ramki_danych_do_wykresow_word//word_wykres2.csv")
word_wykres3 <- read.csv("./przygotowane_ramki_danych_do_wykresow_word/word_wykres3.csv")


mikolaj_matlab <- read.csv("Mikolaj_matlab.csv")
sebastian_matlab <- read.csv("Sebastian_matlab.csv")
malgosia_matlab <- read.csv("Malgosia_matlab.csv")

mikolaj_matlab <- mikolaj_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................) 

sebastian_matlab <- sebastian_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)

malgosia_matlab <- malgosia_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)

podsumowanie_wykres1 <- read.csv("./przygotowane_ramki_danych_podsumowanie/ogolny_wykres1.csv")
podsumowanie_wykres2 <- read.csv("./przygotowane_ramki_danych_podsumowanie/ogolny_wykres2.csv")

# Serwer ***********************************************************************
server <- function(input, output, session) {
  
  # Wykres 1 -------------------------------------------------------------------
  output$JavaWykres1 <- renderPlot({
    ggplot(df %>% group_by(Data_ostatniej_modefikacji, Imie) %>% summarise(liczba= n()) %>% 
             filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]),
           aes(x = Data_ostatniej_modefikacji, y = liczba, color = Imie, fill = Imie)) +
      geom_col() +
      # geom_point() +
      # geom_line() +
      theme_bw() +
      labs(
        x = "Data",
        y = "Liczba utworzonych plików",
        title = "Liczba utworzonych plików w czasie"
      )
  })
  # Wykres 2 -------------------------------------------------------------------
  output$JavaWykres2 <- renderPlot({
    new_df <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
      group_by(Imie) %>%
      summarise(liczba_komentarzy_w_linii = mean(liczba_komentarzy_w_linii)) %>%
      filter(liczba_komentarzy_w_linii > 0)
    ggplot(new_df, aes(x = Imie, y = liczba_komentarzy_w_linii, fill = Imie)) +
      geom_col() +
      theme_bw() +
      labs(
        x = "Osoba",
        y = "Średnia liczba komentarzy w linii na plik"
      )
  })
  # Wykres 3 -------------------------------------------------------------------
  output$JavaWykres3 <- renderPlot({
    new_df <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             liczba_komentarzy_w_linii > 0) %>%
      group_by(Imie) %>%
      summarise(liczba_znakow_w_komantarzu_w_linii = mean(liczba_znakow_w_komantarzu_w_linii/liczba_komentarzy_w_linii))
    ggplot(new_df, aes(x = Imie, y = liczba_znakow_w_komantarzu_w_linii, fill = Imie)) +
      geom_col() +
      theme_bw() +
      labs(
        x = "Osoba",
        y = "Średnia liczba znaków w komentarzu w linii"
      )
  })
  # Wykres 4 -------------------------------------------------------------------
  output$JavaWykres4 <- renderPlot({
    new_df <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
      group_by(Imie) %>%
      summarise(liczba_komentarzy_w_bloku = mean(liczba_komentarzy_w_bloku)) %>%
      filter(liczba_komentarzy_w_bloku > 0)
    ggplot(new_df, aes(x = Imie, y = liczba_komentarzy_w_bloku, fill = Imie)) +
      geom_col() +
      theme_bw() +
      labs(
        x = "Osoba",
        y = "Średnia liczba komentarzy w bloku na plik"
      )
  })
  # Wykres 5 -------------------------------------------------------------------
  output$JavaWykres5 <- renderPlot({
    new_df <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             liczba_komentarzy_w_bloku > 0) %>%
      group_by(Imie) %>%
      summarise(liczba_znakow_w_komentarzu_w_bloku = mean(liczba_znakow_w_komentarzu_w_bloku/liczba_komentarzy_w_bloku))
    ggplot(new_df, aes(x = Imie, y = liczba_znakow_w_komentarzu_w_bloku, fill = Imie)) +
      geom_col() +
      theme_bw() +
      labs(
        x = "Osoba",
        y = "Średnia liczba znaków w komentarzu w bloku"
      )
  })
  # Wykres 6 -------------------------------------------------------------------
  output$JavaWykres6 <- renderText({
    new_df_Mikolaj <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Mikolaj")
    new_df_Sebastian <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Sebastian")
    paste("Mikołaj: ",
      new_df_Mikolaj$Najdluzszy_wyraz[nchar(new_df_Mikolaj$Najdluzszy_wyraz) == max(nchar(new_df_Mikolaj$Najdluzszy_wyraz))] %>%
      head(1), "Sebastian: ",
      new_df_Sebastian$Najdluzszy_wyraz[nchar(new_df_Sebastian$Najdluzszy_wyraz) == max(nchar(new_df_Sebastian$Najdluzszy_wyraz))] %>%
      head(1))
    
  })
  # Wykres 7 -------------------------------------------------------------------
  output$JavaWykres7 <- renderText({
    Mikolaj_txt <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Mikolaj") %>%
      summarise(sum(!!sym(input$txtIn))) %>% pull()
    Sebastian_txt <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Sebastian") %>%
      summarise(sum(!!sym(input$txtIn))) %>% pull()
    paste0("Mikołaj: ", Mikolaj_txt, " ",
           "Sebastian: ", Sebastian_txt)
    
  })

    # WORD -----------------------------------------------------------------------
  # word - wykres tworzenia plikow ---------------------------------------------
  #word_wykres1 <- read.csv("./przygotowane_ramki_danych_do_wykresow_word/word_wykres1.csv")
  output$wykres1_1 <- renderPlot({
    
    word11 <- word_wykres1 %>% 
      filter(Imie == input$zmienna)
    word11$year <- as.integer(word11$year)
    word11$month <- as.integer(word11$month)
    word11 <- word11 %>% 
      arrange(month)%>% 
      arrange(year)
    word11$data <- as.Date(paste(word11$year, word11$month, "01", sep = "-"), format = "%Y-%m-%d")
    
    ggplot(word11, aes(data, y = n)) +
      geom_col(color = "#d11a05", fill = "#d11a05", width = 22)+
      labs(title = paste("Pliki Word ", input$zmienna),
           x = "Czas",
           y = "Ilość") +
      scale_y_continuous(breaks = seq(0, round(max(word11$n)), by = 2)) +
      theme_minimal() +
      theme(
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(size = 0.3, color = "#fbbe9d"),
        panel.grid.minor = element_blank(),#31322e
        plot.title = element_text(family = "Consolas", size = 22, hjust = 0.5, colour = "#fbbe9d"),
        axis.title = element_text(family = "Consolas", size = 16, color = "#ff7355"),
        axis.text.x = element_text(size = 14, color = "#ff7355"),
        axis.text.y = element_text(size = 14, color = "#ff7355")
        
      )
    
    
    
  }, bg = "transparent")
  
  # word - analiza interpunkcji ------------------------------------------------
  #word_wykres2 <- read.csv(".//przygotowane_ramki_danych_do_wykresow_word//word_wykres2.csv")
  output$wykres2 <- renderPlot({
    
    word_wykres2 %>% 
      select(-X) %>% 
      pivot_longer(cols = -Imie, names_to = "Kolumna", values_to = "Wartosc") %>% 
      filter(Imie == input$zmienna) %>% 
      ggplot(aes(x = reorder(Kolumna, -Wartosc), y = Wartosc)) +
      geom_col(fill = "#fc0703") +
      labs(
        title = "Interpunkcja - word",
        x = "Znaki Interpunkcjne",
        y = "Ilość"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(size = 0.5, color = "#66685f", linety = 'dashed'),
        panel.grid.minor = element_blank(),#31322e
        plot.title = element_text(family = "Consolas", size = 23, hjust = 0.5, colour = "#ff9a64"),
        axis.title = element_text(family = "Consolas", size = 16, color = "#ff9a64"),
        axis.text.x = element_text(size = 14, color = "#ff9a64"),
        axis.text.y = element_text(size = 14, color = "#ff9a64"),
        axis.title.x = element_text(vjust = -1),
        axis.ticks.x = element_blank()
      )
    
    
  }, bg = "transparent")
  
  
  # word - zaleznosc miedzy kropkami / przecinkami ... -------------------------
  #word_wykres3 <- read.csv("./przygotowane_ramki_danych_do_wykresow_word/word_wykres3.csv")
  
  output$wykres3 <- renderPlot({
    
    word_wykres3 %>% 
      filter(Imie == input$wybor_zmiennych) %>% 
      filter(Ilosc.kropek < 100) %>% 
      ggplot(aes(x = Ilosc.kropek, y = Ilosc.przecinkow, size = Ilosc.słow., color = Imie)) +
      geom_point() +
      scale_color_manual(values = c("Malgosia" = "#fc0000", "Sebastian" = "#ff8c00", Mikolaj = "#fff200")) +
      theme(#panel.background = element_rect(fill = "white"),
            panel.grid.major = element_line(size = 0.5, color = "#66685f", linety = 'dashed'),
            panel.grid.minor = element_blank(),#31322e
            plot.title = element_text(family = "Consolas", size = 20, hjust = 0.5, colour = "#e0cdbf"),
            axis.title = element_text(family = "Consolas", size = 14, color = "#e0cdbf"),
            panel.background = element_rect(fill = "transparent"), 
            plot.background = element_rect(fill = "transparent"), 
            legend.title = element_text(family = "Consolas", size = 16, color = "#e0cdbf"),
            legend.text = element_text(family = "Consolas", size = 14, color = "#e0cdbf"),
            axis.text.x = element_text(size = 14, color = "#e0cdbf"),
            axis.text.y = element_text(size = 14, color = "#e0cdbf"),
            axis.title.x = element_text(vjust = -1),
            legend.background = element_rect(fill = "transparent")
            
            
      ) +
      guides(size = 'none') +
      labs(title = "Zależność Między Ilością kropek a Ilością Linijek Tekstu", x = "Ilość Kropek", y = "Ilość Linii")
    
    
    
  }, bg = "transparent")
  
  # Wykres 1 dla zakładki MATLAB ------------------------------------------------
  output$MATLABWykres1 <- renderPlotly({
    p <- plot_ly()
    
    if ("Mikołaj" %in% input$imiona) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.operatorow < 70)
      
      p <-    add_trace(p,
                        data = mikolaj_matlab_filtered,
                        type = "scatter",
                        mode = "markers",
                        x = ~Liczba.operatorow, 
                        y = ~Liczba.operatorow.otoczonych.spacjami, 
                        marker = list(color = "blue"),
                        name = "Mikołaj",
                        alpha = 0.7
      )
      
    }
    
    if ("Sebastian" %in% input$imiona) {
      sebastian_matlab_filtered <- sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.operatorow < 70)
      
      
      p <-   add_trace(p,
                       data = sebastian_matlab_filtered,
                       type = "scatter",
                       mode = "markers",
                       x = ~Liczba.operatorow, 
                       y = ~Liczba.operatorow.otoczonych.spacjami, 
                       marker = list(color = "red"),
                       name = "Sebastian",
                       alpha = 0.7
      )
      
    }
    
    if ("Małgosia" %in% input$imiona) {
      malgosia_matlab_filtered <- malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      
      
      p <-   add_trace(p,
                       data = malgosia_matlab_filtered,
                       type = "scatter",
                       mode = "markers",
                       x = ~Liczba.operatorow, 
                       y = ~Liczba.operatorow.otoczonych.spacjami, 
                       marker = list(color = "green"),
                       name = "Małgosia",
                       alpha = 0.7
      )
      
    }
    
    names <- input$imiona  # Get the selected names
    if (length(names) == 1) {
      names_str <- paste(names, collapse = "")  # Combine names into a single string
      title <- paste("Konwencja zapisu operatorów matematycznych -", names_str)
    } else {
      title <- "Konwencja zapisu operatorów matematycznych"
    }
    
    p %>% 
      layout(title = title, 
             xaxis = list(title = "Liczba użytych operatorów matematycznych"), 
             yaxis = list(title = "Liczba operatorów zapisanych ze spacjami wokół nich"), 
             plot_bgcolor = "#232323",  # Kolor tła wykresu
             paper_bgcolor = "#232323",
             font = list(color = "white"),
             aspectratio = list(x = 1, y = 1),  # Ustawienie stosunku osi X do Y
             xaxis = list(scaleanchor = "y", scaleratio = 1)  # Ustawienie skali osi X
  )
    
  })
  
  
  
  # Wykres 2 dla zakładki MATLAB ------------------------------------------------
  
  
  output$MATLABWykres2 <- renderPlotly({
    p <- plot_ly()
    
    if ("Mikołaj" %in% input$imiona) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem < 60)
      
              p <-    add_trace(p,
                    data = mikolaj_matlab_filtered,
                    type = "scatter",
                    mode = "markers",
                    x = ~Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem,
                    y = ~Liczba.wierszy.zakonczonych.srednikiem,
                    marker = list(color = "blue"),
                    name = "Mikołaj",
                    alpha = 0.7
                  )
      
    }
    
    if ("Sebastian" %in% input$imiona) {
      sebastian_matlab_filtered <- sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem < 60)
      
    
                p <-   add_trace(p,
                    data = sebastian_matlab_filtered,
                    type = "scatter",
                    mode = "markers",
                    x = ~Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem,
                    y = ~Liczba.wierszy.zakonczonych.srednikiem,
                    marker = list(color = "red"),
                    name = "Sebastian",
                    alpha = 0.7
                  )
      
    }
    
    if ("Małgosia" %in% input$imiona) {
      malgosia_matlab_filtered <- malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      

                p <-   add_trace(p,
                    data = malgosia_matlab_filtered,
                    type = "scatter",
                    mode = "markers",
                    x = ~Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem,
                    y = ~Liczba.wierszy.zakonczonych.srednikiem,
                    marker = list(color = "green"),
                    name = "Małgosia",
                    alpha = 0.7
                  )

    }
    
    names <- input$imiona  # Get the selected names
    if (length(names) == 1) {
      names_str <- paste(names, collapse = "")  # Combine names into a single string
      title <- paste("Stawianie średników na końcu linii -", names_str)
    } else {
      title <- "Stawianie średników na końcu linii"
    }
    
    p %>% 
      layout(title = title, 
             xaxis = list(title = "Liczba wierszy, które powinny się kończyć średnikiem"), 
             yaxis = list(title = "Liczba wierszy, które kończą się średnikiem"),
             plot_bgcolor = "#232323",  # Kolor tła wykresu
             paper_bgcolor = "#232323",
             font = list(color = "white"),
             aspectratio = list(x = 1, y = 1),  # Ustawienie stosunku osi X do Y
             xaxis = list(scaleanchor = "y", scaleratio = 1),  # Ustawienie skali osi X# Kolor tekstu
             grid = list(
               gridwidth = 5,  # Grubość siatki
               gridcolor = "white"  # Kolor siatki
             ),
             legend = list(title = "Autor")) 
  })
  
  
  # Wykres 3 dla zakładki MATLAB ------------------------------------------------
  output$MATLABWykres3 <- renderPlotly({
    p <- plot_ly()
    
    if ("Mikołaj" %in% input$imiona) {
      mikolaj_matlab_filtered <-  mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.)
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Mikołaj")
      
      
      p <- add_bars(p,
                    data = mikolaj_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "blue"),
                    name = "Mikołaj"
      )
    }
    
    if ("Sebastian" %in% input$imiona) {
      sebastian_matlab_filtered <-  sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.) 
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Sebastian")
      
      
      p <- add_bars(p,
                    data = sebastian_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "red"),
                    name = "Sebastian"
      )
    }
    
    
    if ("Małgosia" %in% input$imiona) {
      malgosia_matlab_filtered <-  malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.)
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Małgosia")
      
      
      
      p <- add_bars(p,
                    data = malgosia_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "green"),
                    name = "Małgosia"
      )
    }
    
    # (analogiczne bloki dla innych autorów)
    
    names <- input$imiona  # Get the selected names
    if (length(names) == 1) {
      names_str <- paste(names, collapse = "")  # Combine names into a single string
      title <- paste("Średnia liczba znaków w wierszu -", names_str)
    } else {
      title <- "Średnia liczba znaków w wierszu"
    }
    
    p %>% 
      layout(title = title, 
             yaxis = list(title = "Średnia liczba znaków w niepustym wierszu"), 
             xaxis = list(title = "Autor"),
             plot_bgcolor = "#232323",  # Kolor tła wykresu
             paper_bgcolor = "#232323",
             font = list(color = "white"),
             grid = list(
               gridwidth = 5,  # Grubość siatki
               gridcolor = "white"  # Kolor siatki
             )) 
  })
  
  
  
  
  
  
  
  # Zmiana stylu ---------------------------------------------------------------
  # observeEvent(input$navbarID, {
  #   if(input$menu == "Word"){
  #     theme_word <- theme_java
  #   } else {
  #     session$sendCustomMessage("background-color", "red")
  #   }
  # })
}

# Styl -------------------------------------------------------------------------
theme_java <- shinyDashboardThemeDIY(
  appFontFamily = "FuturaMedium"
  ,appFontColor = "white"
    ,primaryFontColor = "#434C5E"
    ,infoFontColor = "#434C5E"
    ,successFontColor = "#434C5E"
    ,warningFontColor = "#434C5E"
    ,dangerFontColor = "#434C5E"
  ,bodyBackColor = "#232323" 
    ,logoBackColor = "#151515" 
  ,headerButtonBackColor = "#151515"
    ,headerButtonIconColor = "#D8DEE9"
    ,headerButtonBackColorHover = "#fc0703"
    ,headerButtonIconColorHover = "#151515" 
    ,headerBackColor = "#151515"
    ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  ,sidebarBackColor = "#151515"
    ,sidebarPadding = 0
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 5
  ,sidebarShadowRadius = "" 
  ,sidebarShadowColor = "0px 0px 0px"
  ,sidebarUserTextColor = "#D8DEE9"
  ,sidebarSearchBackColor = "#4C566A"
    ,sidebarSearchIconColor = "#151515"
    ,sidebarSearchBorderColor = "#4C566A"
  ,sidebarTabTextColor = "#ECEFF4"
    ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "#000000"
    ,sidebarTabBorderWidth = 0
  ,sidebarTabBackColorSelected = "#fc0703"
    ,sidebarTabTextColorSelected = "#000000" 
    ,sidebarTabRadiusSelected = "20px" 
  ,sidebarTabBackColorHover = "#fc0703"
    ,sidebarTabTextColorHover = "#000000"
    ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "20px" 
  ,boxBackColor = "#232323" 
    ,boxBorderRadius = 5
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = ""
  ,boxTitleSize = 18
  ,boxDefaultColor = "#232323"
    ,boxPrimaryColor = "#232323"
    ,boxInfoColor = "#232323"
    ,boxSuccessColor = "#232323"
    ,boxWarningColor = "#232323"
    ,boxDangerColor = "#232323"
  ,tabBoxTabColor = "#151515"
    ,tabBoxTabTextSize = 16
  ,tabBoxTabTextColor = "#151515"
    ,tabBoxTabTextColorSelected = "#151515"
    ,tabBoxBackColor = "#BF616A"
    ,tabBoxHighlightColor = "#4C566A"
    ,tabBoxBorderRadius = 5 
  ,buttonBackColor = "#151515"
    ,buttonTextColor = "#2E3440"
    ,buttonBorderColor = "#2E3440"
    ,buttonBorderRadius = 5
  ,buttonBackColorHover = "#151515"
    ,buttonTextColorHover = "#232323"
    ,buttonBorderColorHover = "#2E3440"
  ,textboxBackColor = "#151515" 
    ,textboxBorderColor = "#fc0703" 
    ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "#151515"
    ,textboxBorderColorSelect = "#03a1fc"
  ,tableBackColor = "#151515"
    ,tableBorderColor = "#2E3440"
    ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)


# Tworzenie UI -----------------------------------------------------------------
app_ui <- dashboardPage(
  dashboardHeader(
    title = "Prototyp",
    titleWidth = 250
    ),
  dashboardSidebar(
    sidebarMenu(id = "menu", sidebarMenuOutput("menu"),
                menuItem("Java", tabName = "Java"),
                menuItem("Word", tabName = "Word"),
                menuItem("MATLAB", tabName = "MATLAB")
    ),
    sliderInput(
      inputId = "data",
      label = "Ustaw przedział czasu",
      min = min(df$Data_ostatniej_modefikacji),
      max = max(df$Data_ostatniej_modefikacji),
      value = c(min(df$Data_ostatniej_modefikacji), max(df$Data_ostatniej_modefikacji))
    ),
    selectInput(
      inputId = "imiona",
      label = "Wybierz imiona",
      choices = c("Mikołaj", "Małgosia", "Sebastian"),
      multiple = TRUE
    ),
    width = 250
  ),
  dashboardBody(
    theme_java,
    # tags$head(
    #   tags$style(
    #     HTML('
    #       #shiny-tab-Java h1 {
    #         background-color: red;
    #         color: white;
    #       }
    #       #shiny-tab-Word h2 {
    #         background-color: blue;
    #         color: red;
    #       }
    #       #shiny-tab-MATLAB h3 {
    #         background-color: yellow;
    #         color: red;
    #       }
    #     ')
    #   )
    # ),
    tabItems(
      tabItem(
        tabName = "Java",
        fluidRow(
          box(title = "Java"),
          column(width = 12,
                 plotOutput("JavaWykres1")
          )
        ),
        fluidRow(
          column(width = 3,
                 plotOutput("JavaWykres2")
          ),
          column(width = 3,
                 plotOutput("JavaWykres3")
          ),
          column(width = 3,
                 plotOutput("JavaWykres4")
          ),
          column(width = 3,
                 plotOutput("JavaWykres5")
          )
        ),
        fluidRow(
          column(width = 6,
                 box(
                   title = "Najdłuższy wyraz:",
                   textOutput("JavaWykres6"),
                   width = 12
                   )
                 ),
          column(width = 6,
                 box(
                   title = "Ile wyrazów charakterystycznych dla javy (else, private, abstract itp.) zostało napisanych:",
                   textInput("txtIn", "Wpisz wyraz", value = "protected"),
                   textOutput("JavaWykres7"),
                   width = 12
                   )
                 )
        )
        # ,tags$h1('')
      ),
      tabItem(
        tabName = "Word",
        
        fluidRow(
          column(
            width = 12,
            plotOutput("wykres1_1")
          )
          
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("wykres1_2")
          )
          
        ),
        fluidRow(
          column(
            width = 12,
            plotOutput("wykres1_3")
          )
          
        ),
        fluidRow(
          column(
            width = 12,
            selectInput("zmienna",
                        "Wybierz Imię",
                        zmienne),
            plotOutput("wykres2")
          )
          
        ),
        fluidRow(
          column(
            width = 12,
            checkboxGroupInput("wybor_zmiennych", "Wybierz Imię (Imiona)", choices = c("Malgosia", "Mikolaj", "Sebastian")),
            plotOutput("wykres3")
          )
          
        )
        # ,tags$h2('')
      ),
      tabItem(
        tabName = "MATLAB",
        fluidRow(
          box(title = "MATLAB"),
          column(width = 8, style = "margin-bottom: 80px;",
                 plotlyOutput("MATLABWykres1")
          )
        ),
        fluidRow(
          column(width = 8, style = "margin-bottom: 80px;",
                 plotlyOutput("MATLABWykres2")
          )
        ),
        fluidRow(
          column(width = 8, style = "margin-bottom: 80px;",
                 plotlyOutput("MATLABWykres3")
          )
        )
      )
    ),
    
  )
)




shinyApp(app_ui, server)
