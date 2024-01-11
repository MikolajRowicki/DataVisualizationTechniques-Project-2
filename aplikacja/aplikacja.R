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
df5 <- read.csv("Malgosia_java.csv")
df <- bind_rows(df1, df2, df3, df4, df5)
df$Data_ostatniej_modefikacji <- as.Date(substr(df$Data_ostatniej_modefikacji,1,10))
colnames(df)[which(names(df) == "if.")] <- "if"
colnames(df)[which(names(df) == "else.")] <- "else"
# kolory_java <- c(java_blue, java_orange, hot_red, mellow_red, pronounced_blue, mellow_blue)
kolory_java <- c('#5382a1', '#f89820', '#fc0703', '#DD4B39', '#1666de', '#03a1fc')

zmienne <- c("Sebastian", "Malgosia", "Mikolaj")
word_wykres1 <- read.csv("./przygotowane_ramki_danych_do_wykresow_word/word_wykres1.csv")
word_wykres2 <- read.csv(".//przygotowane_ramki_danych_do_wykresow_word//word_wykres2.csv")
word_wykres3 <- read.csv("./przygotowane_ramki_danych_do_wykresow_word/word_wykres3.csv")


mikolaj_matlab <- read.csv("Mikolaj_matlab.csv")
sebastian_matlab <- read.csv("Sebastian_matlab.csv")
malgosia_matlab <- read.csv("Malgosia_matlab.csv")


mikolaj_matlab <- mikolaj_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................) %>% 
  mutate(Imie = "Mikołaj")

sebastian_matlab <- sebastian_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................) %>% 
  mutate(Imie = "Sebastian")

malgosia_matlab <- malgosia_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................) %>% 
  mutate(Imie = "Małgosia")


matlab_merged <- bind_rows(mikolaj_matlab, sebastian_matlab, malgosia_matlab)
matlab_merged$Data.modyfikacji <- as.Date(substr(matlab_merged$Data.modyfikacji,1,10))
podsumowanie_wykres1 <- read.csv("./przygotowane_ramki_danych_podsumowanie/ogolny_wykres1.csv")
podsumowanie_wykres2 <- read.csv("./przygotowane_ramki_danych_podsumowanie/ogolny_wykres2.csv")

# Serwer ***********************************************************************
server <- function(input, output, session) {
  
  # Wykres 1 -------------------------------------------------------------------
  output$JavaWykres1 <- renderPlotly({
    df_date <- df %>% group_by(Data_ostatniej_modefikacji, Imie) %>% summarise(liczba= n()) %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>% 
      mutate(Rok_i_Miesiac = substr(Data_ostatniej_modefikacji, 1, 7)) %>%
      group_by(Rok_i_Miesiac, Imie) %>% mutate(liczba_w_miesiacu = sum(liczba))
    plot_ly(data = df_date %>% filter(Imie == "Sebastian"),
            x = ~Data_ostatniej_modefikacji, y = ~liczba,
            type = "bar", name = "Sebastian",
            hoverinfo = 'text',
            hovertext = ~paste0("Liczba stworzonych plików \nw ",
                                c("styczniu", "lutym", "marcu", "kwietniu", "maju", "czerwcu",
                                  "lipcu", "sierpniu", "wrześniu", "październiku", "listopadzie", "grudniu"
                                )[as.numeric(substr(Data_ostatniej_modefikacji, 6, 7))],
                                " w ", substr(Data_ostatniej_modefikacji, 1, 4), " roku",
                                "\nu ", c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikolaj", "Malgosia"))],
                                ": ", liczba_w_miesiacu,
                                "\nLiczba stworzonych plików\nw ", Data_ostatniej_modefikacji,
                                ": ", liczba),
            textposition = "none",
            marker = list(color = kolory_java[6]),
            xperiod="M1", xperiodalignment="middle"
    ) %>%
      add_trace(data = df_date %>% filter(Imie == "Mikolaj"),
                name = "Mikołaj",
                marker = list(color = kolory_java[2])
      ) %>%
      add_trace(data = df_date %>% filter(Imie == "Malgosia"),
                name = "Małgosia",
                marker = list(color = kolory_java[1])
      ) %>%
      layout(barmode = 'stack',
             title = "Tworzenie plików .java w czasie",
             xaxis = list(fixedrange = TRUE,
                          title = "Data"),
             yaxis=list(fixedrange=TRUE,
                        title = "Liczba utworzoych plików",
                        gridcolor = "grey"),
             legend = list(
               itemclick = FALSE,
               itemdoubleclick = FALSE,
               groupclick = FALSE
             ),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             font = list(color = "white")) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
  })
  # Wykres 2 -------------------------------------------------------------------
  output$JavaWykres2 <- renderPlotly({
    df_date <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
      group_by(Imie) %>%
      summarise(liczba_komentarzy_w_linii = mean(liczba_komentarzy_w_linii),
                liczba_komentarzy_w_bloku = mean(liczba_komentarzy_w_bloku)) %>%
      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikolaj", "Malgosia"))])
    plot_ly(data = df_date,
            x = ~Imie, y = ~liczba_komentarzy_w_linii,
            type = "bar", name = 'Komentarze\njednoliniowe;\n<i><sup>//komentarz</sup></i>',
            hoverinfo = 'text',
            hovertext = ~paste0("Średnia liczba\nkomentarzy\njednoliniowych na plik\nu ",
                                c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                ":\n", round(liczba_komentarzy_w_linii, 2)),
            textposition = "none",
            marker = list(color = kolory_java[5])
    ) %>%
      add_trace(name = "Komentarze\nwieloliniowych;\n<i><sup>/*komentarz*/</sup></i>",
                y = ~liczba_komentarzy_w_bloku,
                hoverinfo = 'text',
                hovertext = ~paste0("Średnia liczba\nkomentarzy\nwieloliniowych na plik\nu ",
                                    c("Sebastiana", "Mikołaja", "Małgosi"
                                    )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                    ":\n", round(liczba_komentarzy_w_bloku, 2)),
                marker = list(color = kolory_java[4])
      ) %>%
      layout(barmode = 'group',
             title = "Średnia liczba komentarzy na plik",
             xaxis = list(fixedrange = TRUE,
                          title = "Osoba"),
             yaxis=list(fixedrange=TRUE,
                        title = "Liczba komentarzy"),
             legend = list(
               itemclick = FALSE,
               itemdoubleclick = FALSE,
               groupclick = FALSE
             ),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             font = list(color = "white")
      ) %>%
      config(displayModeBar = FALSE)
  })
  # Wykres 3 -------------------------------------------------------------------
  output$JavaWykres3 <- renderPlotly({
    df_date <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
      group_by(Imie) %>%
      summarise(liczba_komentarzy_w_linii = sum(liczba_komentarzy_w_linii),
                liczba_komentarzy_w_bloku = sum(liczba_komentarzy_w_bloku),
                liczba_znakow_w_komantarzu_w_linii = sum(liczba_znakow_w_komantarzu_w_linii),
                liczba_znakow_w_komentarzu_w_bloku = sum(liczba_znakow_w_komentarzu_w_bloku)) %>%
      mutate(srednia_liczba_znakow_w_komantarzu_w_linii = if_else(liczba_komentarzy_w_linii == 0, 0, liczba_znakow_w_komantarzu_w_linii/liczba_komentarzy_w_linii),
             srednia_liczba_znakow_w_komentarzu_w_bloku = if_else(liczba_komentarzy_w_bloku == 0, 0, liczba_znakow_w_komentarzu_w_bloku/liczba_komentarzy_w_bloku)) %>%
      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikolaj", "Malgosia"))])
    plot_ly(data = df_date,
            x = ~Imie, y = ~srednia_liczba_znakow_w_komantarzu_w_linii,
            type = "bar", name = "Komentarze\njedniolinowe;\n<i><sup>//komentarz</sup></i>",
            hoverinfo = 'text',
            hovertext = ~paste0("Średnia liczba znaków\nw komentarzach jedniolinowych\nu ",
                                c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                ":\n", round(srednia_liczba_znakow_w_komantarzu_w_linii, 2)),
            textposition = "none",
            marker = list(color = kolory_java[5])
    ) %>%
      add_trace(name = "Komentarze\nwieloliniowe;\n<i><sup>/*komentarz*/</sup></i>",
                y = ~srednia_liczba_znakow_w_komentarzu_w_bloku,
                hoverinfo = 'text',
                hovertext = ~paste0("Średnia liczba znaków\nw komentarzach wieloliniowych\nu ",
                                    c("Sebastiana", "Mikołaja", "Małgosi"
                                    )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                    ":\n", round(srednia_liczba_znakow_w_komentarzu_w_bloku, 2)),
                marker = list(color = kolory_java[4])
      ) %>%
      layout(barmode = 'group',
             title = "Średnia liczba znaków na komentarz",
             xaxis = list(fixedrange = TRUE,
                          title = "Osoba"),
             yaxis=list(fixedrange=TRUE,
                        title = "Liczba znaków"),
             legend = list(
               itemclick = FALSE,
               itemdoubleclick = FALSE,
               groupclick = FALSE
             ),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             font = list(color = "white")
      ) %>%
      config(displayModeBar = FALSE)
  })
  # Wykres 4 (nieczynny) -------------------------------------------------------------------
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
  # Wykres 5 (nieczynny) -------------------------------------------------------------------
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
  output$JavaWykres6 <- renderUI({
    new_df_Mikolaj <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Mikolaj")
    najdluzszy_wyraz_Mikolaj <- new_df_Mikolaj$Najdluzszy_wyraz[nchar(new_df_Mikolaj$Najdluzszy_wyraz) == max(nchar(new_df_Mikolaj$Najdluzszy_wyraz))] %>%
      head(1)
    new_df_Sebastian <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Sebastian")
    najdluzszy_wyraz_Sebastian <- new_df_Sebastian$Najdluzszy_wyraz[nchar(new_df_Sebastian$Najdluzszy_wyraz) == max(nchar(new_df_Sebastian$Najdluzszy_wyraz))] %>%
      head(1)
    new_df_Malgosia <- df %>% 
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Malgosia")
    najdluzszy_wyraz_Malgosia <- new_df_Malgosia$Najdluzszy_wyraz[nchar(new_df_Malgosia$Najdluzszy_wyraz) == max(nchar(new_df_Malgosia$Najdluzszy_wyraz))] %>%
      head(1)
    HTML(paste("<b>Małgosia</b>:<br/>",
               najdluzszy_wyraz_Malgosia, 
               "<br/><small>(", nchar(najdluzszy_wyraz_Malgosia), "znaków)</small>"),
         "<br/><b>Mikołaj</b>:<br/>",
         najdluzszy_wyraz_Mikolaj, 
         "<br/><small>(", nchar(najdluzszy_wyraz_Mikolaj), "znaków)</small>",
         "<br><b>Sebastian</b>:<br/>",
         najdluzszy_wyraz_Sebastian, 
         "<br/><small>(", nchar(najdluzszy_wyraz_Sebastian), "znaków)</small>",
    )
    
  })
  # Wykres 7 -------------------------------------------------------------------
  output$JavaWykres7 <- renderUI({
    Mikolaj_txt <- df %>%
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Mikolaj") %>%
      summarise(sum(!!sym(input$txtIn))) %>% pull()
    Sebastian_txt <- df %>%
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Sebastian") %>%
      summarise(sum(!!sym(input$txtIn))) %>% pull()
    Malgosia_txt <- df %>%
      filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2],
             Imie == "Malgosia") %>%
      summarise(sum(!!sym(input$txtIn))) %>% pull()
    HTML(paste0("<b>Małgosia</b>: ", Malgosia_txt,
                "<br/><b>Mikołaj</b>: ", Mikolaj_txt,
                "<br/><b>Sebastian</b>: ", Sebastian_txt
    ))
  })
  
  # Wykres 8 -------------------------------------------------------------------
  output$JavaWykres8 <- renderValueBox({
    valueBox(tags$p(501232, style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p("znaków napisanych całkowicie", style = "font-size: 125%; text-align: center;color: #FFFFFF;"), 
             color = "red")    
  })
  
  # Wykres 9 -------------------------------------------------------------------
  output$JavaWykres9 <- renderValueBox({
    valueBox(tags$p(9012, style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p("linijek napisanych", style = "font-size: 125%; text-align: center;color: #FFFFFF;"), 
             color = "red")    
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
        axis.text.y = element_text(size = 14, color = "#ff7355"),
        rect = element_rect(colour = "#232323")
        
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


  initial_plot <- ggplot() + theme_minimal()
  
  reactive_plot <- reactive({
    df <- word_wykres3 %>% 
      filter(Imie == input$wybor_zmiennych) %>% 
      filter(Ilosc.kropek < 100)
    
    ggplot(df, aes(x = Ilosc.kropek, y = Ilosc.przecinkow, size = Ilosc.słow., color = Imie)) +
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
    
  })

  observe({
    output$wykres3 <- renderPlot({
      if (is.null(input$wybor_zmiennych)) {
        print(initial_plot)
      } else {
        print(reactive_plot())
      }
    }, bg = "transparent")
  })

  # Wykres 1 dla zakładki MATLAB ------------------------------------------------
  output$MATLABWykres1 <- renderPlotly({
    p <- plot_ly()
    
    if ("Mikołaj" %in% input$matlab1) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.operatorow < 70)
      
      p <- add_trace(p,
                     data = mikolaj_matlab_filtered,
                     type = "violin",
                     x = ~Imie,
                     y = ~Liczba.operatorow.otoczonych.spacjami * 100 / Liczba.operatorow,
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     spanmode = "hard",
                     name = "Mikołaj"
      )
    }
    
    if ("Sebastian" %in% input$matlab1) {
      sebastian_matlab_filtered <- sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.operatorow < 70)
      
      p <- add_trace(p,
                     data = sebastian_matlab_filtered,
                     type = "violin",
                     x = ~Imie,
                     y = ~Liczba.operatorow.otoczonych.spacjami * 100 / Liczba.operatorow,
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     spanmode = "hard",
                     name = "Sebastian"
      )
    }
    
    if ("Małgosia" %in% input$matlab1){
      malgosia_matlab_filtered <- malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      
      p <- add_trace(p,
                     data = malgosia_matlab_filtered,
                     margin = list(l = 50, r = 50, b = 50, t = 100),
                     type = "violin",
                     x = ~Imie,
                     y = ~Liczba.operatorow.otoczonych.spacjami * 100 / Liczba.operatorow,
                     box = list(visible = FALSE),
                     meanline = list(visible = TRUE),
                     spanmode = "hard",
                     name = "Małgosia"
      )
    }
    
    names <- input$matlab1  # Get the selected names
    if (length(names) == 1) {
      names_str <- paste(names, collapse = "")  # Combine names into a single string
      title <- paste("Konwencja zapisu operatorów matematycznych -", names_str)
    } else {
      title <- "Konwencja zapisu operatorów matematycznych"
    }
    
    p %>% 
      layout(title = title, 
             xaxis = list(title = "Autor"), 
             yaxis = list(title = "Procent operatorów zapisanych ze spacjami wokół"), 
             plot_bgcolor = "#232323",  # Kolor tła wykresu
             paper_bgcolor = "#232323",
             font = list(color = "white") # Ustawienie stosunku osi X do Y
      )
  })
  
  
  
  
  # Wykres 2 dla zakładki MATLAB ------------------------------------------------
  
  
  output$MATLABWykres2 <- renderPlotly({
    p <- plot_ly()
    
    if ("Mikołaj" %in% input$matlab2) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem < 60)
      
              p <-    add_trace(p,
                    data = mikolaj_matlab_filtered,
                    type = "scatter",
                    mode = "markers",
                    x = ~Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem,
                    y = ~Liczba.wierszy.zakonczonych.srednikiem,
                    marker = list(color = "#1F77B4"),
                    name = "Mikołaj",
                    alpha = 0.7
                  )
      
    }
    
    if ("Sebastian" %in% input$matlab2) {
      sebastian_matlab_filtered <- sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem < 60)
      
    
                p <-   add_trace(p,
                    data = sebastian_matlab_filtered,
                    type = "scatter",
                    mode = "markers",
                    x = ~Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem,
                    y = ~Liczba.wierszy.zakonczonych.srednikiem,
                    marker = list(color = "#FF7F0E"),
                    name = "Sebastian",
                    alpha = 0.7
                  )
      
    }
    
    if ("Małgosia" %in% input$matlab2) {
      malgosia_matlab_filtered <- malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      

                p <-   add_trace(p,
                    data = malgosia_matlab_filtered,
                    type = "scatter",
                    mode = "markers",
                    x = ~Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem,
                    y = ~Liczba.wierszy.zakonczonych.srednikiem,
                    marker = list(color = "#2C9B2C"),
                    name = "Małgosia",
                    alpha = 0.7
                  )

    }
    
    names <- input$matlab2  # Get the selected names
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
             font = list(color = "white"), # Ustawienie stosunku osi X do Y
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
    

      mikolaj_matlab_filtered <-  mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.)
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Mikołaj")
      
      
      p <- add_bars(p,
                    data = mikolaj_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "#1F77B4"),
                    name = "Mikołaj"
      )

    
      sebastian_matlab_filtered <-  sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.) 
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Sebastian")
      
      
      p <- add_bars(p,
                    data = sebastian_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "#FF7F0E"),
                    name = "Sebastian"
      )
    
    
      malgosia_matlab_filtered <-  malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        summarise(Srednia_liczba_znakow_w_niepustym_wierszu = sum(Liczba.znaków.)
                  / (sum(Liczba.wierszy) - sum(Liczba.pustych.linii))) %>% 
        mutate(Imie = "Małgosia")
      
      
      
      p <- add_bars(p,
                    data = malgosia_matlab_filtered,
                    x = ~Imie,
                    y = ~Srednia_liczba_znakow_w_niepustym_wierszu,
                    marker = list(color = "#2C9B2C"),
                    name = "Małgosia"
      )
    
    
    # (analogiczne bloki dla innych autorów)
    

    
    p %>% 
      layout(title = "Liczba znaków w niepustym wierszu", 
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
  
  
  # Wykres 4 dla zakładki MATLAB ------------------------------------------------
  
  output$MATLABWykres4 <- renderPlotly({
    p_boxplot <- plot_ly()
    
    if ("Mikołaj" %in% input$matlab4) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>%
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        mutate(Imie = "Mikołaj")
      
      p_boxplot <- add_trace(p_boxplot,
                             data = mikolaj_matlab_filtered,
                             type = "box",
                             x = ~Imie,
                             y = ~Laczna.dlugosc.komentarzy / Liczba.wierszy,
                             name = "Mikołaj",
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    if ("Sebastian" %in% input$matlab4) {
      sebastian_matlab_filtered <- sebastian_matlab %>%
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])%>% 
        mutate(Imie = "Sebastian")
      
      p_boxplot <- add_trace(p_boxplot,
                             data = sebastian_matlab_filtered,
                             type = "box",
                             x = ~Imie,
                             y = ~Laczna.dlugosc.komentarzy / Liczba.wierszy,
                             name = "Sebastian",
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    if ("Małgosia" %in% input$matlab4) {
      malgosia_matlab_filtered <- malgosia_matlab %>%
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])%>% 
        mutate(Imie = "Małgosia")
      
      p_boxplot <- add_trace(p_boxplot,
                             data = malgosia_matlab_filtered,
                             type = "box",
                             x = ~Imie,
                             y = ~Laczna.dlugosc.komentarzy / Liczba.wierszy,
                             name = "Małgosia",
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    p_boxplot %>%
      layout(title = "Jaką część pliku stanowią komentarze?",
             xaxis = list(title = "Autor"),
             yaxis = list(title = "Stosunek długości komentarzy do długości pliku"),
             plot_bgcolor = "#232323",
             paper_bgcolor = "#232323",
             font = list(color = "white"),
             xaxis2 = list(domain = c(0.8, 1), anchor = "y2"),
             grid = list(gridwidth = 5, gridcolor = "white"),
             legend = list(title = "Autor")
      )
  })
  
  # Jaką część pliku stanowią komentarze?
  
  ### Wykres 5 dla zakładki MATLAB-------------
  
  output$MATLABWykres5 <- renderPlotly({
    
    df_date_matlab <- matlab_merged %>%
      group_by(Data.modyfikacji, Imie) %>%
      summarise(liczba = n()) %>%
      filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>%
      mutate(Rok_i_Miesiac = substr(Data.modyfikacji, 1, 7)) %>%
      group_by(Rok_i_Miesiac, Imie) %>%
      mutate(liczba_w_miesiacu = sum(liczba))
    
    selected_names <- input$matlab5
    
    plot_ly(data = df_date_matlab %>% filter(Imie %in% selected_names),
            x = ~Data.modyfikacji, y = ~liczba,
            type = "bar", color = ~Imie,
            hoverinfo = 'text',
            hovertext = ~paste0("Liczba stworzonych plików \nw ",
                                c("styczniu", "lutym", "marcu", "kwietniu", "maju", "czerwcu",
                                  "lipcu", "sierpniu", "wrześniu", "październiku", "listopadzie", "grudniu"
                                )[as.numeric(substr(Data.modyfikacji, 6, 7))],
                                " w ", substr(Data.modyfikacji, 1, 4), " roku",
                                "\nu ", c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikołaj", "Małgosia"))],
                                ": ", liczba_w_miesiacu,
                                "\nLiczba stworzonych plików\nw ", Data.modyfikacji,
                                ": ", liczba),
            textposition = "none",
            colors = kolory_java,
            xperiod = "M1", xperiodalignment = "middle"
    ) %>%
      layout(barmode = 'stack',
             title = "Tworzenie plików .m w czasie",
             xaxis = list(fixedrange = TRUE,
                          title = "Data"),
             yaxis = list(fixedrange = TRUE,
                          title = "Liczba utworzonych plików",
                          gridcolor = "grey"),
             legend = list(
               itemclick = FALSE,
               itemdoubleclick = FALSE,
               groupclick = FALSE
             ),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)",
             font = list(color = "white")) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
  })


  ### podsumowanie -> wykres numer 1--------------------------------------------
  output$Podsumowanie_wykres1 <- renderPlot({
    df <- podsumowanie_wykres1
    df <- podsumowanie_wykres1 %>% 
      filter(Imie == input$podsumowanie_11)
    df$year <- as.integer(df$year)
    df$month <- as.integer(df$month)
    df$data <- as.Date(paste(df$year, "-", df$month, "-01", sep = ""), format = "%Y-%m-%d")
    df <- df %>% 
      filter(data >= input$podsumowanie_1[1] & data <= input$podsumowanie_1[2]) %>% 
      arrange(month)%>% 
      arrange(year) 
    
    ggplot(df, aes(x = data, y = n, group = Rozszerzenie, color = Rozszerzenie)) +
      geom_line(size = 1) +
      geom_ribbon(aes(ymax = n, ymin = 0, fill = Rozszerzenie), alpha = 0.4) +
      theme_minimal() +
      scale_color_manual(values = c("docx" = "#ef2009", "java" = "#ffd200", "m" = "#ff00d2")) +
      scale_fill_manual(values = c("docx" = "#fb6e5f", "java" = "#fbe995", "m" = "#feadf0")) +
      theme(
        plot.background = element_rect(fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(size = 0.2, color = "#ffdba9"),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Consolas", size = 22, hjust = 0.5, colour = "#ffdba9"),
        axis.title = element_text(family = "Consolas", size = 16, color = "#ffeca9"),
        axis.text.x = element_text(size = 14, color = "#ffeca9"),
        axis.text.y = element_text(size = 14, color = "#ffeca9"),
        panel.border = element_blank(),
        legend.text = element_text(color = "#ffeca9"),
        legend.title = element_text(color = "#ffeca9"),
        rect = element_rect(colour = "#232323")
      ) +
      labs(title = "Tworzenie Plików o Danym Rozszerzeniu", x = "Czas", y = "Ilość Utworzonych Plików")
    
  }, bg = "transparent")
  
  
  ### podsumowanie -> wykres numer 2 -------------------------------------------
  
  
  output$Podsumowanie_wykres2 <- renderPlot({
    
    df1 <- podsumowanie_wykres2 %>% 
      filter(Imie == input$podsumowanie_2)
    kolory <- c("#d71528", "#f38b2c", "#f8c01a")
    
    treemap(
      df1,
      index = "Rozszerzenie",
      vSize = "n",
      #type = "index",
      #vColor = "Rozszerzenie",
      palette = kolory
    )
    
  }, bg = "transparent")

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
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: red}")),
    sidebarMenu(id = "menu", sidebarMenuOutput("menu"),
                menuItem("Java", tabName = "Java"),
                menuItem("Word", tabName = "Word"),
                menuItem("MATLAB", tabName = "MATLAB"),
                menuItem("Podsumowanie", tabName = "Podsumowanie")
    ),
    sliderInput(
      inputId = "data",
      label = "Ustaw przedział czasu",
      min = min(df$Data_ostatniej_modefikacji),
      max = max(df$Data_ostatniej_modefikacji),
      value = c(min(df$Data_ostatniej_modefikacji), max(df$Data_ostatniej_modefikacji))
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
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      tabItem(
        tabName = "Java",
        #     tags$style(type = "text/css", "
        #   .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
        #   .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
        #   .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
        #   .irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
        #   .irs-grid-pol {display: none;}
        #   .irs-max {font-family: 'arial'; color: black;}
        #   .irs-min {font-family: 'arial'; color: black;}
        #   .irs-single {color:black; background:#6666ff;}
        #   .irs-slider {width: 30px; height: 30px; top: 22px;}
        # "),
        fluidRow(
          style = "margin-bottom: 80px;",
          box(title = "Java"),
          # selectInput(
          #   inputId = "JavaSelectInput2 (jeszcze nie działa)",
          #   label = "Wybierz imię",
          #   choices = c("Mikołaj", "Małgosia", "Sebastian"),
          #   selected = c("Mikołaj", "Małgosia", "Sebastian"),
          #   multiple = TRUE
          # ),
          column(width = 12,
                 plotlyOutput("JavaWykres1")
          )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 6,
                 plotlyOutput("JavaWykres2")
          ),
          column(width = 6,
                 plotlyOutput("JavaWykres3")
          )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 6,
                 box(
                   title = "Najdłuższy wyraz:",
                   htmlOutput("JavaWykres6"),
                   width = 12
                 )
          ),
          column(width = 6,
                 box(
                   title = "Ile wyrazów charakterystycznych dla javy (else, private, abstract itp.) zostało napisanych:",
                   textInput("txtIn", "Wpisz wyraz", value = "protected"),
                   htmlOutput("JavaWykres7"),
                   width = 12
                 )
          )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 3,
                 selectInput(
                   inputId = "JavaSelectInput1",
                   label = "Wybierz imiona (jeszcze nie działa)",
                   choices = c("Mikołaj", "Małgosia", "Sebastian"),
                   selected = c("Mikołaj", "Małgosia", "Sebastian"),
                   multiple = TRUE
                 )
          ),
          column(width = 4.5,
                 valueBoxOutput('JavaWykres8')
                 ),
          column(width = 4.5,
                 valueBoxOutput('JavaWykres9')
          )
        )
        # ,tags$h1('')
      ),
      tabItem(
        tabName = "Word",
        
        fluidRow(
          column(
            width = 12,
            selectInput("zmienna",
                        "Wybierz Imię",
                        zmienne),
            plotOutput("wykres1_1")
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
            checkboxGroupInput("wybor_zmiennych", "Wybierz Imię (Imiona)", choices = zmienne),
            plotOutput("wykres3")
          )
          
        )
        # ,tags$h2('')
      ),
      tabItem(
        tabName = "MATLAB",
        
        fluidRow(
          box(title = "MATLAB",
              width = 6, style = "margin-bottom: 80px; margin-left: 20px; margin-top: 60 px",
              selectInput(
                inputId = "matlab5",
                label = "Wybierz imię",
                choices = c("Mikołaj", "Małgosia", "Sebastian"),
                selected = c("Mikołaj", "Małgosia", "Sebastian"),
                multiple = TRUE
              ),
              plotlyOutput("MATLABWykres5")
          )
        ),
        fluidRow(
          box(
            width = 6, style = "margin-bottom: 80px; margin-left: 20px;",
            selectInput(
              inputId = "matlab4",
              label = "Wybierz imię",
              choices = c("Mikołaj", "Małgosia", "Sebastian"),
              selected = c("Mikołaj", "Małgosia", "Sebastian"),
              multiple = TRUE
            ),
            plotlyOutput("MATLABWykres4")
          ),
          box(width = 6,
              style = "margin-bottom: 20px; margin-left: 10px; margin-top: 150 px",
              renderText("Wykres przedstawia wykres")
          )
        ),

        fluidRow(
          box(
              width = 6, style = "margin-bottom: 80px; margin-left: 20px;",
                 selectInput(
                   inputId = "matlab1",
                   label = "Wybierz imię",
                   choices = c("Mikołaj", "Małgosia", "Sebastian"),
                   selected = c("Mikołaj", "Małgosia", "Sebastian"),
                   multiple = TRUE
                 ),
                 plotlyOutput("MATLABWykres1")
          ),
          box(width = 6,
              style = "margin-bottom: 20px; margin-left: 10px;",
              "Tekst opisujący wykres."
          )
        ),
        fluidRow(
          column(width = 6, style = "margin-bottom: 80px; margin-left: 20px;",
                 selectInput(
                   inputId = "matlab2",
                   label = "Wybierz imię",
                   choices = c("Mikołaj", "Małgosia", "Sebastian"),
                   selected = c("Mikołaj", "Małgosia", "Sebastian"),
                   multiple = TRUE
                 ),
                 plotlyOutput("MATLABWykres2")
          )
        ),
        fluidRow(
          column(width = 6, style = "margin-bottom: 80px;",
                 plotlyOutput("MATLABWykres3")
          )
        )

      ),

      tabItem(
        tabName = "Podsumowanie",
        fluidRow(
          column(width = 10, style = "margin-bottom: 80px; margin-left: 20px;",
                 selectInput("podsumowanie_11", "Wybierz Imię", zmienne),
                 sliderInput(
                   inputId = "podsumowanie_1",
                   label = "Ustaw przedział czasu",
                   min = as.Date("2018-01-01"),
                   max = as.Date("2024-01-01"),
                   value = c(as.Date("2018-01-01"), as.Date("2024-01-01"))
                 ),
                 plotOutput("Podsumowanie_wykres1"))
        ),
        fluidRow(
            column(width = 8, style = "margin-left: 20px;",
                 selectInput("podsumowanie_2", "Wybierz Imię", zmienne),
                 plotOutput("Podsumowanie_wykres2")))
      )
      
    ),
    
  )
)




shinyApp(app_ui, server)
