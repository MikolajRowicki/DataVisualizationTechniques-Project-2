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

################################################################################
# Przygotowywane danych
################################################################################
# java
# df_test <- read.csv("Test_java.csv")
# df_test <- read.csv("Test2_java.csv")
df1 <- read.csv("Sebastian_java.csv")
df2 <- read.csv("Sebastian2_java.csv")
df3 <- read.csv("Sebastian3_java.csv")
df2$Imie <- "Sebastian"
df3$Imie <- "Sebastian"
df4 <- read.csv("Mikolaj_java.csv")
df5 <- read.csv("Malgosia_java.csv")
df <- bind_rows(df1, df2, df3, df4, df5)
df[is.na(df)] <- 0
df$Data_ostatniej_modefikacji <- as.Date(substr(df$Data_ostatniej_modefikacji,1,10))
colnames(df)[which(names(df) == "break.")] <- "break"
colnames(df)[which(names(df) == "else.")] <- "else"
colnames(df)[which(names(df) == "for.")] <- "for"
colnames(df)[which(names(df) == "if.")] <- "if"
colnames(df)[which(names(df) == "non.sealed")] <- "non-sealed"
colnames(df)[which(names(df) == "while.")] <- "while"
java_keyword_list <- sort(colnames(df)[15:45])
kolory_java <- c('#5382a1', '#f89820', '#fc0703', '#DD4B39', '#1666de', '#03a1fc')

# word
malgosia_word <- read.csv("Malgosia-word.csv")
sebastian_word <- read.csv("Sebastian-word.csv")
mikolaj_word <- read.csv("Mikolaj-word.csv")
word <- rbind(malgosia_word, mikolaj_word, sebastian_word)
zmienne <- c("Mikolaj", "Sebastian", "Malgosia")
kolory_word <- c("#7780f8", "#0a2051", "#39bacc", "#78dfd0", "#2929b3")

# matlab
mikolaj_matlab <- read.csv("Mikolaj_matlab.csv")
sebastian_matlab <- read.csv("Sebastian_matlab.csv")
malgosia_matlab <- read.csv("Malgosia_matlab.csv")

mikolaj_matlab <- mikolaj_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................) 

sebastian_matlab <- sebastian_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)

malgosia_matlab <- malgosia_matlab %>%
  rename(Liczba.operatorow = Liczba.operatorów..........................)
matlab_merged <- bind_rows(mikolaj_matlab, sebastian_matlab, malgosia_matlab)
matlab_merged$Data.modyfikacji <- as.Date(substr(matlab_merged$Data.modyfikacji,1,10))
# zakładka domowa
podsumowanie_wykres1 <- read.csv("./przygotowane_ramki_danych_podsumowanie/ogolny_wykres1.csv")
podsumowanie_wykres2 <- read.csv("./przygotowane_ramki_danych_podsumowanie/ogolny_wykres2.csv")
kolory_ogolny <- c("#0d5630", "#6b3c02", "#1b0952", "#6af1ab", "#f7be79", "#64b5f8")

kolor_przewodni_java <- c('#fc0703', '#03a1fc')
kolor_przewodni_word <- c('#1B5EBE', '#41A5EE')
kolor_przewodni_matlab <- c('#ed9242', '#fcf647')
# styl domyślny ----
theme_default <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "FuturaMedium"
  ,appFontColor = "white"
    ,primaryFontColor = "#434C5E"
    ,infoFontColor = "#434C5E"
    ,successFontColor = "#434C5E"
    ,warningFontColor = "#434C5E"
    ,dangerFontColor = "#434C5E"
    ,bodyBackColor = "#232323" 
    
  ### header
  ,logoBackColor = "#151515" 
    
  ,headerButtonBackColor = "#151515"
  ,headerButtonIconColor = "#D8DEE9"
  ,headerButtonBackColorHover = "#48e0ab"
  ,headerButtonIconColorHover = "#151515" 
    
  ,headerBackColor = "#151515"
  ,headerBoxShadowColor = ""
  ,headerBoxShadowSize = "0px 0px 0px"
  
  ### sidebar
  # ,sidebarBackColor = "#151515"
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "#151515"
    ,colorMiddle = "#151515"
    ,colorEnd = "#232323"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
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
  
  # ,sidebarTabBackColorSelected = "#fc0703"
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "#42c798"
    ,colorMiddle = "#48e0ab"
    ,colorEnd = "#55f2ba"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "#000000" 
    ,sidebarTabRadiusSelected = "20px" 
  
  # ,sidebarTabBackColorHover = "#fc0703"
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "#42c798"
      ,colorMiddle = "#48e0ab"
      ,colorEnd = "#55f2ba"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "#000000"
    ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "20px"
  
  ### boxes
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
  
  ### inputs
  ,buttonBackColor = "#151515"
    ,buttonTextColor = "#2E3440"
    ,buttonBorderColor = "#2E3440"
    ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "#151515"
    ,buttonTextColorHover = "#232323"
    ,buttonBorderColorHover = "#2E3440"
    
  ,textboxBackColor = "#151515" 
    ,textboxBorderColor = "#48e0ab" 
    ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "#151515"
    ,textboxBorderColorSelect = "#47fcf6"
    
  ### tables
  ,tableBackColor = "#151515"
    ,tableBorderColor = "#2E3440"
    ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
)

################################################################################
# Serwer
################################################################################
server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  # Wykresy Java
  #-----------------------------------------------------------------------------
  
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
                        title = "Liczba utworzoych pkików",
                        gridcolor = "grey"),
             # legend = list(
             #   itemclick = FALSE,
             #   itemdoubleclick = FALSE,
             #   groupclick = FALSE
             # ),
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
                        title = "Liczba komentarzy",
                        gridcolor = "grey"),
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
                        title = "Liczba znaków",
                        gridcolor = "grey"),
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
    df_tmp <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2], input$txtIn %in% colnames(df)[15:45])
    Mikolaj_df_tmp <- df_tmp %>% filter(Imie == "Mikolaj")
    Sebastian_df_tmp <- df_tmp %>% filter(Imie == "Sebastian")
    Malgosia_df_tmp <- df_tmp %>% filter(Imie == "Malgosia")
    HTML(paste0("<b>Małgosia</b>: ", round(Malgosia_df_tmp %>% summarise(sum(!!sym(input$txtIn))) %>% pull() / Malgosia_df_tmp %>% summarise(liczba_plikow = n()), 2),
                "<br/><b>Mikołaj</b>: ", round(Mikolaj_df_tmp %>% summarise(sum(!!sym(input$txtIn))) %>% pull() / Mikolaj_df_tmp %>% summarise(liczba_plikow = n()), 2),
                "<br/><b>Sebastian</b>: ", round(Sebastian_df_tmp %>% summarise(sum(!!sym(input$txtIn))) %>% pull() / Sebastian_df_tmp %>% summarise(liczba_plikow = n()), 2)
    ))
  })
  
  # Wykres 8 -------------------------------------------------------------------
  output$JavaWykres8 <- renderValueBox({
    valueBox(tags$p(df_tmp <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
                      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikolaj", "Malgosia"))]) %>%
                      filter(Imie %in% input$JavaSelectInput1) %>% summarise(ilosc_znakow = sum(Liczba_znaków)) %>% pull(),
                    style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p("znaków napisanych w sumie", style = "font-size: 125%; text-align: center;color: #FFFFFF;"), 
             color = "red")    
  })
  # Wykres 9 -------------------------------------------------------------------
  output$JavaWykres9 <- renderValueBox({
    valueBox(tags$p(df_tmp <- df %>% filter(Data_ostatniej_modefikacji >= input$data[1], Data_ostatniej_modefikacji <= input$data[2]) %>%
                      mutate(Imie = c("Sebastian", "Mikołaj", "Małgosia")[match(Imie, c("Sebastian", "Mikolaj", "Malgosia"))]) %>%
                      filter(Imie %in% input$JavaSelectInput1) %>% summarise(ilosc_znakow = sum(Liczba_linii)) %>% pull(),
                    style = "font-size: 175%; text-align: center;color: #FFFFFF;"),
             tags$p("linijek napisanych w sumie", style = "font-size: 125%; text-align: center;color: #FFFFFF;"), 
             color = "red")    
  })
  # Wykres 10 ------------------------------------------------------------------
  output$JavaWykres10 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 15, "px;",
      "color: ", "white", ";",
      "background-color:", "#DD4B39", ";",
      "border:", "#fc0703", ";",
      "padding: 10px;",
      "margin-top: 10px;",
      "text-align: justify;"
    )
    text <- "Wykres przedstawia liczbę utworzonych plików z rozszerzeniem
                     .java w czasie. Klikając na legendę można wybać osoby, do których
                      dane będą się odnosić. Po najechaniu na słupek w danym kolorze można
                     zobaczyć dokładną liczbę utworzonych plików w wybranym miesiącu. Dodatkowo
                     jeżdżąc kurosrem wzdłuż kolumny wyświetla się w dolnej części opisu informacja
                     o ilości utworzonych plików z podziałem na dni."
    div(style = text_style, HTML(text))
  })

  #-----------------------------------------------------------------------------
  # Wykresy Word ***************************************************************
  #-----------------------------------------------------------------------------
  
  # word - wykres tworzenia plikow ---------------------------------------------
  output$wykres1_1 <- renderPlotly({
    
    word$Data.utworzenia.pliku <- as.Date(substr(word$Data.utworzenia.pliku,1,10))
    word <- word %>% 
      group_by(Data.utworzenia.pliku, Imie) %>%
      summarise(n = n()) %>% 
      mutate(data = substr(Data.utworzenia.pliku, 1, 7)) %>%
      group_by(data, Imie) %>%
      mutate(liczba_w_miesiacu = sum(n)) %>% 
      filter(Data.utworzenia.pliku >= input$data[1], Data.utworzenia.pliku <= input$data[2])
    
    plot_ly(data = word%>% 
            filter(Imie == "Sebastian"),
            x = ~Data.utworzenia.pliku, 
            y = ~n,
            type = "bar",
            name = "Sebastian",
            hoverinfo = 'text',
            hovertext = ~paste0("Liczba stworzonych plików \nw ",
                                c("styczniu", "lutym", "marcu", "kwietniu", "maju", "czerwcu",
                                  "lipcu", "sierpniu", "wrześniu", "październiku", "listopadzie", "grudniu"
                                )[as.numeric(substr(Data.utworzenia.pliku, 6, 7))],
                                " w ", substr(Data.utworzenia.pliku, 1, 4), " roku",
                                "\nu ", c("Sebastiana", "Mikołaja", "Małgosi"
                                )[match(Imie, c("Sebastian", "Mikolaj", "Malgosia"))],
                                ": ", liczba_w_miesiacu,
                                "\nLiczba stworzonych plików\nw ", Data.utworzenia.pliku,
                                ": ", n),
            textposition = "none",
            marker = list(color = kolory_word[1]),
            xperiod="M1", xperiodalignment="middle"
    ) %>%
      add_trace(data = word %>% filter(Imie == "Mikolaj"),
                name = "Mikołaj",
                marker = list(color = kolory_word[2])
      ) %>%
      add_trace(data = word %>% filter(Imie == "Malgosia"),
                name = "Małgosia",
                marker = list(color = kolory_word[3])
      ) %>%
      layout(barmode = 'stack',
             font = list(family = "FuturaMedium", color = "white", size = 14),
             title = list(text = "Tworzenie plików .docx w czasie", font = list(size = 22)),
             margin = list(t = 40),
             xaxis = list(fixedrange = TRUE,
                          title = list(text = "Data", font = list(size = 18))),
             yaxis=list(fixedrange=TRUE,
                        title = list(text = "Liczba utworzoych plików", font = list(size = 18)),
                        gridcolor = "grey"),
             plot_bgcolor = "rgba(0,0,0,0)",
             paper_bgcolor = "rgba(0,0,0,0)"
             ) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
    
  })
  
  # word - analiza interpunkcji ------------------------------------------------
  output$wykres2 <- renderPlotly({
    kolor <- kolory_word[1]

    if (input$zmienna == "Mikolaj")
    {
      kolor <- kolory_word[2]
    } else if (input$zmienna == "Malgosia") {
      kolor <- kolory_word[3]
    }else {
      kolor <- kolory_word[1]
    }
    
    
    word2 <- word %>% 
      filter(Imie == input$zmienna) %>% 
      filter(Data.utworzenia.pliku >= input$data[1], Data.utworzenia.pliku <= input$data[2]) %>% 
      summarise(Kropka = sum(Ilosc.kropek), Przecinek = sum(Ilosc.przecinkow), Dwukropek = sum(Ilosc.dwukropkow), Pozostałe = sum(Ilosc.pozostalych.znakow), Pytajnik = sum(Ilosc.pytajnikow), Wykrzyknik = sum(Ilosc.wykrzyknikow), Myślnik= sum(Ilosc.myslnikow))
    
    df <- word2 %>% 
      pivot_longer(cols = c("Kropka", "Przecinek", "Dwukropek", "Pozostałe", "Pytajnik", "Wykrzyknik", "Myślnik"), names_to = "Kolumna", values_to = "Wartosc")
    
    plot_ly(
      df,
      x = ~reorder(Kolumna, -Wartosc),
      y = ~Wartosc,
      type = 'bar',
      marker = list(color = kolor)
      
    ) %>% 
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 14),
        title = list(text = "Interpunkcja - word", font = list(size = 22)),
        xaxis = list(title = list(text= "Znaki Interpunkcjne", font = list(size = 18))),
        yaxis = list(title = list(text= "Ilość", font = list(size = 18))),
        showlegend = FALSE,
        margin = list(t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      config(displayModeBar = FALSE,
             locale = 'pl') 
    
  })
  
  
  # word - zaleznosc miedzy kropkami / przecinkami ... -------------------------
  
  output$wykres3 <- renderPlotly({
    
    kolor <- c(kolory_word[3], kolory_word[2], kolory_word[1])
    df <- word %>% 
      filter(Data.utworzenia.pliku >= input$data[1], Data.utworzenia.pliku <= input$data[2]) %>% 
      select(Ilosc.kropek, Ilosc.słow., Ilosc.przecinkow, Imie)
    plot_ly(data = df %>% 
            filter(Ilosc.kropek <= 500),
            x = ~Ilosc.kropek,
            y = ~Ilosc.przecinkow,
            type = 'scatter',
            size = ~Ilosc.słow.,
            color = ~Imie,
            colors = kolor,
            mode = 'markers'
    )%>%
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 14),
        title = list(text = "Zależność Między Kropkami i Przecinkami", font = list(size = 22)),
        xaxis = list(title = list(text= "Ilość Kropek", font = list(size = 18))),
        yaxis = list(title = list(text= "Ilość Przecinków", font = list(size = 18))),
        showlegend = list(show = TRUE),
        margin = list(t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% 
      config(displayModeBar = FALSE,
             locale = 'pl') 
    
  })


  #-----------------------------------------------------------------------------
  # Wykresy MATLAB
  #-----------------------------------------------------------------------------
  
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
             font = list(color = "white"),
             aspectratio = list(x = 1, y = 1)  # Ustawienie stosunku osi X do Y
      )
  })
  
  
  
  
  # Wykres 2 dla zakładki MATLAB ------------------------------------------------
  
  
  output$MATLABWykres2 <- renderPlotly({
    p <- plot_ly()
    
    if ("Mikołaj" %in% input$matlab2) {
      mikolaj_matlab_filtered <- mikolaj_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      
          p <- add_trace(p,
                         data = mikolaj_matlab_filtered,
                         type = "box",
                         x = ~Imie,
                         y = ~round(Liczba.wierszy.zakonczonych.srednikiem / Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, 2),
                         
                         name = "Mikołaj",
                         boxmean = TRUE,
                         boxpoints = ""
              )
      
    }
    
    if ("Sebastian" %in% input$matlab2) {
      sebastian_matlab_filtered <- sebastian_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2]) %>% 
        filter(Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem < 60)
      
    
                p <-   add_trace(p,
                    data = sebastian_matlab_filtered,
                    type = "box",
                    x = ~Imie,
                    y = ~round(Liczba.wierszy.zakonczonych.srednikiem / Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, 2),
                    
                    name = "Sebastian",
                    boxmean = TRUE,
                    boxpoints = ""
                  )
      
    }
    
    if ("Małgosia" %in% input$matlab2) {
      malgosia_matlab_filtered <- malgosia_matlab %>% 
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])
      

                p <-   add_trace(p,
                    data = malgosia_matlab_filtered,
                    type = "box",
                    x = ~Imie,
                    y = ~round(Liczba.wierszy.zakonczonych.srednikiem / Liczba.wierszy..ktore.powinny.sie.konczyc.srednikiem, 2)
                    ,
                    name = "Małgosia",
                    boxmean = TRUE,
                    boxpoints = ""
                  )

    }
    
    names <- input$matlab2  # Get the selected names
    if (length(names) == 1) {
      names_str <- paste(names, collapse = "")  # Combine names into a single string
      title <- paste("Stawianie średników na końcu linii -", names_str)
    } else {
      title <- "Stawianie średników na końcu linii"
    }
    y_axis_values <- seq(0, 1, 0.1)
    p %>% 
      layout(title = title, 
             xaxis = list(title = "Liczba wierszy, które powinny się kończyć średnikiem"), 
             yaxis = list(
               title = "Liczba wierszy, które kończą się średnikiem",
               tickvals = y_axis_values,
               ticktext = sprintf("%.0f%%", y_axis_values * 100),
               tickformat = "%"),  # Dodanie formatu procentowego do osi Y
             plot_bgcolor = "#232323",  # Kolor tła wykresu
             paper_bgcolor = "#232323",
             font = list(color = "white"),
             xaxis = list(scaleanchor = "y", scaleratio = 1),  # Ustawienie skali osi X
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
                    name = "Mikołaj",
                    opacity = 0.7
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
                    name = "Sebastian",
                    opacity = 0.7
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
                    name = "Małgosia",
                    opacity = 0.7
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
                             y = ~Laczna.dlugosc.komentarzy * 100/ Liczba.wierszy,
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
                             y = ~Laczna.dlugosc.komentarzy * 100 / Liczba.wierszy,
                             name = "Sebastian",
                             hovertemplate = "Value: %{y}<extra></extra>",
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    if ("Małgosia" %in% input$matlab4) {
      malgosia_matlab_filtered <- malgosia_matlab %>%
        filter(Data.modyfikacji >= input$data[1], Data.modyfikacji <= input$data[2])%>% 
        mutate( przelicznik  = Laczna.dlugosc.komentarzy / Liczba.wierszy) %>% 
        filter(przelicznik < 0.5) %>% 
        mutate(Imie = "Małgosia")
      
      p_boxplot <- add_trace(p_boxplot,
                             data = malgosia_matlab_filtered,
                             type = "box",
                             x = ~Imie,
                             y = ~Laczna.dlugosc.komentarzy * 100/ Liczba.wierszy,
                             name = "Małgosia",
                             boxmean = TRUE,
                             boxpoints = ""
      )
    }
    
    p_boxplot %>%
      layout(title = "Jaką część pliku stanowią komentarze?",
             xaxis = list(title = "Autor"),
             yaxis = list(title = "Stosunek długości komenatrzy do długości pliku"),
             plot_bgcolor = "#232323",
             paper_bgcolor = "#232323",
             font = list(color = "white"),
             aspectratio = list(x = 1, y = 1),
             xaxis2 = list(domain = c(0.8, 1), anchor = "y2"),
             grid = list(gridwidth = 5, gridcolor = "white"),
             legend = list(title = "Autor")
      )
  })
  
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
  
  # Jaką część pliku stanowią komentarze?
  
  #-----------------------------------------------------------------------------
  # Wykresy Podsumowanie
  #-----------------------------------------------------------------------------

  ### podsumowanie -> wykres numer 1--------------------------------------------
  output$Podsumowanie_wykres1 <- renderPlotly({
    
    df <- podsumowanie_wykres1
    df$year <- as.integer(df$year)
    df$month <- as.integer(df$month)
    df$data <- as.Date(paste(df$year, "-", df$month, "-01", sep = ""), format = "%Y-%m-%d")
    df <- df %>% 
      arrange(month) %>% 
      arrange(year) %>% 
      filter(data >= input$data[1] & data <= input$data[2]) %>% 
      filter(Imie == input$podsumowanie_11)
    df <- df %>% 
      mutate( nn = 
                case_when(
                  Rozszerzenie == "docx" ~ n,
                  Rozszerzenie == "m" & month < 9 & year == 2023 ~ NA,
                  Rozszerzenie == "java" & year < 2023 ~ NA,
                  Rozszerzenie == "m" & year < 2023 ~ NA,
                  TRUE ~ n
                )
      )
    df <- df[complete.cases(df[, "nn"]), ]
    
    df1 <- df %>% 
      filter(Rozszerzenie == "java")
    
    df2 <- df %>% 
      filter(Rozszerzenie == "docx")
    
    df3 <- df %>% 
      filter(Rozszerzenie == "m")
    
    w <- plot_ly()
    w %>% 
      add_trace(x = df1$data, y = df1$nn, type = 'scatter', mode = 'lines', line = list(color = kolory_ogolny[2]), color = "java", hoverinfo = "none")%>% 
      add_trace(x = c(df1$data, rev(df1$data)), y = c(df1$nn, rep(min(df1$nn), length(df1$nn))),
                fill = 'toself', type = 'scatter', mode = 'none', fillcolor = kolory_ogolny[5], showlegend = FALSE, hoverinfo = "none") %>% 
      add_trace(x = df2$data, y = df2$nn, type = 'scatter', mode = 'lines', line = list(color = kolory_ogolny[1]), color = "docx", hoverinfo = "none") %>% 
      add_trace(x = c(df2$data, rev(df2$data)), y = c(df2$nn, rep(min(df2$nn), length(df2$nn))),
                fill = 'toself', type = 'scatter', mode = 'none', fillcolor = kolory_ogolny[4], showlegend = FALSE, hoverinfo = "none") %>% 
      add_trace(x = df3$data, y = df3$nn, type = 'scatter', mode = 'lines', line = list(color = kolory_ogolny[3]), color = "m", hoverinfo = "none") %>% 
      add_trace(x = c(df3$data, rev(df3$data)), y = c(df3$nn, rep(min(df3$nn), length(df3$nn))),
                fill = 'toself', type = 'scatter', mode = 'none', fillcolor = kolory_ogolny[6], showlegend = FALSE, hoverinfo = "none") %>% 
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 14),
        title = list(text = "Tworzenie Plików o Danym Rozszerzeniu", font = list(size = 22)),
        xaxis = list(title = list(text= "Czas", font = list(size = 18))),
        yaxis = list(title = list(text= "Ilość Utworzonych Plików", font = list(size = 18))),
        margin = list(t = 40),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% 
      config(displayModeBar = FALSE,
            locale = 'pl') 
    
  })
  
  ### podsumowanie -> wykres numer 2 -------------------------------------------
  
  
  output$Podsumowanie_wykres2 <- renderPlotly({
    
    df1 <- podsumowanie_wykres2 %>% 
      filter(Imie == input$podsumowanie_2) %>% 
      filter(data >= input$data[1] & data <= input$data[2]) %>% 
      group_by(Rozszerzenie) %>% 
      summarise(n = n())
    
    df1$Procent <- (df1$n / sum(df1$n)) * 100
    
    plot_ly(
      data = df1,
      labels = ~Rozszerzenie,
      parents = ~"",
      values = ~n,
      type = "treemap",
      marker = list(
        colors = c(kolory_ogolny[1], kolory_ogolny[2], kolory_ogolny[3]),
        line = list(width = 1, color = "white")
      ),
      hoverinfo = 'text',
      hovertext = ~paste('Rozszerzenie: ', Rozszerzenie, '<br>Procent: ', round(Procent, 2), '%'),
      textposition = "none"
    ) %>%
      layout(
        font = list(family = "FuturaMedium", color = "white", size = 14),
        title = list(text = "Jak Rozkładają Się Pliki na Naszych Komputerach?", font = list(size = 22)),
        margin = list(t = 40),
        treemapcolorway = c(kolory_ogolny[1], kolory_ogolny[2], kolory_ogolny[3]),
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% 
      config(displayModeBar = FALSE,
             locale = 'pl') 
    
  })
  
  
  
  # output$tekst_ogolny----
  output$tekst_ogolny <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 22, "px;",
      "color: ", "#7ed987", ";",
      "background-color: rgba(0, 0, 0, 0);",
      "border: rgba(0, 0, 0, 0);",
      "padding: 10px;",
      "margin-top: 10px;",
      "text-align: justify;"
    )
    text <- "Witamy na stronie domowej! Tutaj znajdziesz szczegółowe informacje dotyczące tworzonych przez nas plików. Aby uzyskać bardziej precyzyjne dane, skorzystaj z suwaka po prawej stronie i wybierz interesujący Cię przedział czasowy."
    div(style = text_style, HTML(text))
  })
  # output$tekst_podsumowanie_1----
  output$tekst_podsumowanie_1 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#48e0ab", ";",
      "background-color: rgba(0, 0, 0, 0);",
      "border: rgba(0, 0, 0, 0);",
      "margin-top: 70px;",
      "padding: 10px;"
    )
    text <- "Na wykresie obserwujemy ilość plików utworzonych przez nas z podziałem na różne rozszerzenia. Po wyborze konkretnej osoby możliwe jest prześledzenie historii tworzenia plików przez daną osobę. Wyraźnie widać, kiedy zaczęliśmy intensywnie tworzyć pliki z rozszerzeniem Matlab i Java.
    W kolejnych zakładkach dostępne są bardziej szczegółowe dane dotyczące tworzenia plików dla każdego z rozszerzeń, co pozwala na dokładniejszą analizę."
    div(style = text_style, HTML(text))
  })
  # output$tekst_podsumowanie_2----
  output$tekst_podsumowanie_2 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#48e0ab", ";",
      "background-color: rgba(0, 0, 0, 0);",
      "border: rgba(0, 0, 0, 0);",
      "margin-top: 60px;",
      "padding: 10px;"
    )
    text <- "Na wykresie porównujemy ilość plików z różnymi rozszerzeniami, umożliwiając identyfikację dominującego formatu plików. Po najechaniu na konkretne pole wykresu uzyskujemy informację o procentowym udziale plików z danym rozszerzeniem. Zauważalne jest, że u Małgosi przeważają pliki Java, podczas gdy u Sebastiana utrzymane są najbardziej zrównoważone proporcje między różnymi formatami plików."
    div(style = text_style, HTML(text))
  })

  
  # output$tekst_word_1----
  output$tekst_word_1 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#c0cae8", ";",
      "background-color: ", "#2949a9", ";",
      "border: 2px solid ", "black", ";",
      "padding: 10px;",
      "margin-top: 40px;",
      "text-align: justify;"
    )
    text <- "Pierwszy wykres prezentuje ilość utworzonych plików przez każdą osobę z naszej grupy. Dane dotyczą liczby plików stworzonych w poszczególnych miesiącach i latach. Przy najeżdżaniu kursorem na konkretną kolumnę możliwe jest uzyskanie szczegółowych informacji na temat ilości utworzonych plików w poszczególnych dniach.
            Zauważalne jest, że Mikołaj regularnie korzysta z programu Word przez wiele lat. Natomiast Sebastian i Małgosia tworzyli pliki głownie w latach 2019 - 2021."
    div(style = text_style, HTML(text))
  })
  # output$tekst_word_2----
  output$tekst_word_2 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#c0cae8", ";",
      "background-color: ", "#2949a9", ";",
      "border: 2px solid ", "black", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Na wykresie po prawej stronie dostępne są informacje dotyczące najczęściej używanych znaków interpunkcyjnych. Dane obejmują okres czasu wybrany za pomocą suwaka umieszczonego w pasku po prawej stronie. Warto zauważyć, że wśród najpopularniejszych znaków interpunkcyjnych dominują przede wszystkim kropki i przecinki. Kategoria 'pozostałe' obejmuje znaki takie jak wielokropki, średniki i cudzysłowia.
            Analizując okres od 2017 do 2023 roku, zauważamy, że u Małgosi i Mikołaja najwięcej używanych jest przecinków, natomiast u Sebastiana przeważają kropki."
    div(style = text_style, HTML(text))
  })
  # output$tekst_word_3----
  output$tekst_word_3 <- renderUI({

    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#c0cae8", ";",
      "background-color: ", "#2949a9", ";",
      "border: 2px solid ", "black", ";",
      "padding: 10px;",
      "margin-top: 40px;",
      "text-align: justify;"
    )
    text <- "Na ostatnim wykresie dokładniej przeanalizowaliśmy zależności pomiędzy najczęściej używanymi przez nas znakami interpunkcyjnymi. Każda kropka na tym wykresie reprezentuje jeden plik. Dodatkowo, rozmiar kropki odzwierciedla liczbę słów w danym pliku - im większa kropka, tym dłuższy tekst."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_1 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#c0cae8", ";",
      "background-color: ", "#2949a9", ";",
      "border: 2px solid ", "black", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Wykres po lewej dotyczy dotyczy tego, jaką część plików o rozszerzeniu .m poświęcamy komentarzom. Badaliśmy
    to zjawisko, zliczając liczbę zakomentowanych wierszy w danym pliku i dzieląc ją przez łączną liczbę wierszy. Wyniki 
    ukazają znaczne różnice między nami - okazuje się, że u Sebastiana zdarzają się pliki zakomentowane
    w ponad 80 procentach, u Małgosi wartość maksymalna to 40 procent. Co ciekawe, jeżeli chodzi o średnią i medianę, są one największe u Mikołaja
    i wynoszą około 35 procent."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_2 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#c0cae8", ";",
      "background-color: ", "#2949a9", ";",
      "border: 2px solid ", "black", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Wykres po lewej dotyczy dotyczy tego, jaką część plików o rozszerzeniu .m poświęcamy komentarzom. Badaliśmy
    to zjawisko, zliczając liczbę zakomentowanych wierszy w danym pliku i dzieląc ją przez łączną liczbę wierszy. Wyniki 
    ukazają znaczne różnice między nami - okazuje się, że u Sebastiana zdarzają się pliki zakomentowane
    w ponad 80 procentach, u Małgosi wartość maksymalna to 40 procent. Co ciekawe, jeżeli chodzi o średnią i medianę, są one największe u Mikołaja
    i wynoszą około 35 procent."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_3 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#c0cae8", ";",
      "background-color: ", "#2949a9", ";",
      "border: 2px solid ", "black", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Wykres po lewej dotyczy dotyczy tego, jaką część plików o rozszerzeniu .m poświęcamy komentarzom. Badaliśmy
    to zjawisko, zliczając liczbę zakomentowanych wierszy w danym pliku i dzieląc ją przez łączną liczbę wierszy. Wyniki 
    ukazają znaczne różnice między nami - okazuje się, że u Sebastiana zdarzają się pliki zakomentowane
    w ponad 80 procentach, u Małgosi wartość maksymalna to 40 procent. Co ciekawe, jeżeli chodzi o średnią i medianę, są one największe u Mikołaja
    i wynoszą około 35 procent."
    div(style = text_style, HTML(text))
  })
  
  output$tekst_matlab_4 <- renderUI({
    
    text_style <- paste0(
      "font-family: '", "FuturaMedium", "';",
      "font-size: ", 18, "px;",
      "color: ", "#c0cae8", ";",
      "background-color: ", "#2949a9", ";",
      "border: 2px solid ", "black", ";",
      "padding: 10px;",
      "margin-top: 70px;",
      "text-align: justify;"
    )
    text <- "Wykres po lewej dotyczy dotyczy tego, jaką część plików o rozszerzeniu .m poświęcamy komentarzom. Badaliśmy
    to zjawisko, zliczając liczbę zakomentowanych wierszy w danym pliku i dzieląc ją przez łączną liczbę wierszy. Wyniki 
    ukazają znaczne różnice między nami - okazuje się, że u Sebastiana zdarzają się pliki zakomentowane
    w ponad 80 procentach, u Małgosi wartość maksymalna to 40 procent. Co ciekawe, jeżeli chodzi o średnią i medianę, są one największe u Mikołaja
    i wynoszą około 35 procent."
    div(style = text_style, HTML(text))
  })

  
  #-----------------------------------------------------------------------------
  # Zmiana stylu
  #-----------------------------------------------------------------------------
  # styl ogólny strony----
  output$style_ogol <- renderUI({
    # Motyw Word ----
    if(input$menu=='Word')
      return(shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "FuturaMedium"
        ,appFontColor = "white"
          ,primaryFontColor = "#434C5E"
          ,infoFontColor = "#434C5E"
          ,successFontColor = "#434C5E"
          ,warningFontColor = "#434C5E"
          ,dangerFontColor = "#434C5E"
          ,bodyBackColor = "#232323" 
          
        ### header
        ,logoBackColor = "#151515" 
          
        ,headerButtonBackColor = "#151515"
          ,headerButtonIconColor = "#D8DEE9"
          ,headerButtonBackColorHover = kolor_przewodni_word[1]
          ,headerButtonIconColorHover = "#151515" 
          
        ,headerBackColor = "#151515"
          ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
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
        
        ,sidebarTabBackColorSelected = kolor_przewodni_word[1]
          ,sidebarTabTextColorSelected = "#000000" 
          ,sidebarTabRadiusSelected = "20px" 
        
        ,sidebarTabBackColorHover = kolor_przewodni_word[1]
          ,sidebarTabTextColorHover = "#000000"
          ,sidebarTabBorderStyleHover = "none"
        ,sidebarTabBorderColorHover = "none"
        ,sidebarTabBorderWidthHover = 0
        ,sidebarTabRadiusHover = "20px"
        
        ### boxes
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
        
        ### inputs
        ,buttonBackColor = "#151515"
          ,buttonTextColor = "#2E3440"
          ,buttonBorderColor = "#2E3440"
          ,buttonBorderRadius = 5
        
        ,buttonBackColorHover = "#151515"
          ,buttonTextColorHover = "#232323"
          ,buttonBorderColorHover = "#2E3440"
          
        ,textboxBackColor = "#151515" 
          ,textboxBorderColor = kolor_przewodni_word[1]
          ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "#151515"
          ,textboxBorderColorSelect = kolor_przewodni_word[2]
          
        ### tables
        ,tableBackColor = "#151515"
          ,tableBorderColor = "#2E3440"
          ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
      ))
    # Motyw Java ----
    if(input$menu=='Java')
      return(shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "FuturaMedium"
        ,appFontColor = "white"
          ,primaryFontColor = "#434C5E"
          ,infoFontColor = "#434C5E"
          ,successFontColor = "#434C5E"
          ,warningFontColor = "#434C5E"
          ,dangerFontColor = "#434C5E"
          ,bodyBackColor = "#232323" 
          
        ### header
        ,logoBackColor = "#151515" 
          
        ,headerButtonBackColor = "#151515"
          ,headerButtonIconColor = "#D8DEE9"
          ,headerButtonBackColorHover = kolor_przewodni_java[1]
          ,headerButtonIconColorHover = "#151515" 
          
        ,headerBackColor = "#151515"
          ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
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
        
        ,sidebarTabBackColorSelected = kolor_przewodni_java[1]
          ,sidebarTabTextColorSelected = "#000000" 
          ,sidebarTabRadiusSelected = "20px" 
        
        ,sidebarTabBackColorHover = kolor_przewodni_java[1]
          ,sidebarTabTextColorHover = "#000000"
          ,sidebarTabBorderStyleHover = "none"
        ,sidebarTabBorderColorHover = "none"
        ,sidebarTabBorderWidthHover = 0
        ,sidebarTabRadiusHover = "20px"
        
        ### boxes
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
        
        ### inputs
        ,buttonBackColor = "#151515"
          ,buttonTextColor = "#2E3440"
          ,buttonBorderColor = "#2E3440"
          ,buttonBorderRadius = 5
        
        ,buttonBackColorHover = "#151515"
          ,buttonTextColorHover = "#232323"
          ,buttonBorderColorHover = "#2E3440"
          
        ,textboxBackColor = "#151515" 
          ,textboxBorderColor = kolor_przewodni_java[1]
          ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "#151515"
          ,textboxBorderColorSelect = kolor_przewodni_java[2]
          
        ### tables
        ,tableBackColor = "#151515"
          ,tableBorderColor = "#2E3440"
          ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
      ))
    # Motyw MATLAB ----
    if(input$menu=='MATLAB')
      return(shinyDashboardThemeDIY(
        
        ### general
        appFontFamily = "FuturaMedium"
        ,appFontColor = "white"
          ,primaryFontColor = "#434C5E"
          ,infoFontColor = "#434C5E"
          ,successFontColor = "#434C5E"
          ,warningFontColor = "#434C5E"
          ,dangerFontColor = "#434C5E"
          ,bodyBackColor = "#232323" 
          
        ### header
        ,logoBackColor = "#151515" 
          
        ,headerButtonBackColor = "#151515"
          ,headerButtonIconColor = "#D8DEE9"
          ,headerButtonBackColorHover = kolor_przewodni_matlab[1]
          ,headerButtonIconColorHover = "#151515" 
          
        ,headerBackColor = "#151515"
          ,headerBoxShadowColor = ""
        ,headerBoxShadowSize = "0px 0px 0px"
        
        ### sidebar
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
        
        ,sidebarTabBackColorSelected = kolor_przewodni_matlab[1]
          ,sidebarTabTextColorSelected = "#000000" 
          ,sidebarTabRadiusSelected = "20px" 
        
        ,sidebarTabBackColorHover = kolor_przewodni_matlab[1]
          ,sidebarTabTextColorHover = "#000000"
          ,sidebarTabBorderStyleHover = "none"
        ,sidebarTabBorderColorHover = "none"
        ,sidebarTabBorderWidthHover = 0
        ,sidebarTabRadiusHover = "20px"
        
        ### boxes
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
        
        ### inputs
        ,buttonBackColor = "#151515"
          ,buttonTextColor = "#2E3440"
          ,buttonBorderColor = "#2E3440"
          ,buttonBorderRadius = 5
        
        ,buttonBackColorHover = "#151515"
          ,buttonTextColorHover = "#232323"
          ,buttonBorderColorHover = "#2E3440"
          
        ,textboxBackColor = "#151515" 
          ,textboxBorderColor = kolor_przewodni_matlab[1] 
          ,textboxBorderRadius = 5
        ,textboxBackColorSelect = "#151515"
          ,textboxBorderColorSelect = kolor_przewodni_matlab[2]
          
        ### tables
        ,tableBackColor = "#151515"
          ,tableBorderColor = "#2E3440"
          ,tableBorderTopSize = 1
        ,tableBorderRowSize = 1
      ))
    # Motyw domyślny ----
    return(theme_default)
  })
  
  # styl wybranych komponentów !WORK IN PROGRESS!----
  output$style_css <- renderUI({
    # word page style----
    if(input$menu=='Word')
      return(tags$style(HTML("
      
    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    } 
    .irs--shiny .irs-bar {
    border-top-color: #1B5EBE;
    border-bottom-color: #1B5EBE;
    } 
    .irs--shiny .irs-bar-edge {
    border-color: #1B5EBE;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #41A5EE;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #1B5EBE;
    background-color: #1B5EBE;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #41A5EE;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #1B5EBE;
    }
    
    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #1B5EBE;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #41A5EE;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #1B5EBE;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #1B5EBE;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #41A5EE;
    background: #151515;
    }
    
    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
                           ")))
    # java page style----
    if(input$menu=='Java')
      return(tags$style(HTML("
      
    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    } 
    .irs--shiny .irs-bar {
    border-top-color: #fc0703;
    border-bottom-color: #fc0703;
    } 
    .irs--shiny .irs-bar-edge {
    border-color: #fc0703;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #DD4B39;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #fc0703;
    background-color: #fc0703;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #DD4B39;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #fc0703;
    }
    
    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #fc0703;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #03a1fc;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #DD4B39;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #fc0703;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #03a1fc;
    background: #151515;
    }
    
    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
                           ")))
    # matlab page style----
    if(input$menu=='MATLAB')
      return(tags$style(HTML("
      
    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    } 
    .irs--shiny .irs-bar {
    border-top-color: #ed9242;
    border-bottom-color: #ed9242;
    } 
    .irs--shiny .irs-bar-edge {
    border-color: #ed9242;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #fcf647;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #ed9242;
    background-color: #ed9242;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #fcf647;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #ed9242;
    }
    
    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #ed9242;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #fcf647;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #ed9242;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #ed9242;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #fcf647;
    background: #151515;
    }
    
    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
                           ")))
    # home page style----
    return(tags$style(HTML("
      
    /* sliders */
    .js-irs-0 {
    max-width: 215px;
    } 
    .irs--shiny .irs-bar {
    border-top-color: #48e0ab;
    border-bottom-color: #48e0ab;
    } 
    .irs--shiny .irs-bar-edge {
    border-color: #48e0ab;
    }
    .irs--shiny .irs-single, .irs--shiny .irs-bar-edge, .irs--shiny .irs-bar {
    background: #47fcf6;
    }
    .irs--shiny .irs-handle {
    border: 1px solid #48e0ab;
    background-color: #48e0ab;
    }
    .irs--shiny .irs-handle.state_hover, .irs--shiny .irs-handle:hover {
    background: #47fcf6;
    }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
    color: #000000;
    background-color: #48e0ab;
    }
    
    /* dropdown menus */
    .selectize-dropdown .selected {
    background-color: #48e0ab;
    color: #000000;
    }
    .selectize-dropdown .active:not(.selected) {
    background: #47fcf6;
    color: #000000;
    }
    .selectize-input, .selectize-control.single .selectize-input.input-active {
    background: #000000;
    color: #ffffff
    }
    .selectize-dropdown [data-selectable] .highlight {
    background: rgba(235, 64, 52, 0.4);
    border-radius: 1px;
    }
    .selectize-control.multi .selectize-input>div {
    cursor: pointer;
    background: #48e0ab;
    color: #ffffff;
    }
    selectize-dropdown .create {
    color: #ffffff;
    }
    .form-control, .selectize-input, .selectize-control.single .selectize-input {
    background: #000000;
    color: #ffffff;
    border-color: #48e0ab;
    }
    .nwm-czemu-to-dziala .selectize-input, .selectize-control.single .selectize-input.input-active {
    border-color: #47fcf6;
    background: #151515;
    }
    
    /* fixed sidebar and header */
    .sidebar {
    position: fixed;
    width: 250px;
    white-space: nowrap;
    overflow: visible;
    }
    .main-header {
    position: fixed;
    width:100%;
    }
                           ")))
  })
}

################################################################################
# Tworzenie UI
################################################################################
app_ui <- dashboardPage(
  
  #-----------------------------------------------------------------------------
  # Panel zarządzania
  #-----------------------------------------------------------------------------
  dashboardHeader(
    title = "Prototyp",
    titleWidth = 250
    ),
  dashboardSidebar(
    sidebarMenu(uiOutput('style_ogol'),
                uiOutput('style_css'),
                id = "menu", sidebarMenuOutput("menu"),
                menuItem("Strona domowa", tabName = "Ogólny",
                         icon = icon("home")),
                menuItem("Java", tabName = "Java",
                         icon = icon("java")),
                menuItem("Word", tabName = "Word",
                         icon = icon("file-word")),
                menuItem("MATLAB", tabName = "MATLAB",
                         icon = icon("calculator"))
    ),
    sliderInput(
      inputId = "data",
      label = "Ustaw przedział czasu",
      min = min(as.Date(word$Data.utworzenia.pliku)),
      max = max(as.Date(podsumowanie_wykres2$data)),
      value = c(as.Date(min(as.Date(df$Data_ostatniej_modefikacji))), as.Date(max(as.Date(podsumowanie_wykres2$data))))
    ),
    width = 250
  ),
  #-----------------------------------------------------------------------------
  # Koniec panelu zarządzania
  #-----------------------------------------------------------------------------
  #style = "margin-bottom: 80px; margin-left: 20px; margin-top: 80x;"
  dashboardBody(
    theme_default,
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    tabItems(
      
      #-------------------------------------------------------------------------
      # Panel ogólny
      #-------------------------------------------------------------------------
      tabItem(
        tabName = "Ogólny",
        fluidRow(
          column(width = 8,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   uiOutput("tekst_ogolny")
                 ))
        ),
        fluidRow(
          column(width = 8,
                 selectInput("podsumowanie_11", "Wybierz Imię", zmienne),
                 plotlyOutput("Podsumowanie_wykres1")
                 ),
          column(width = 4,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   uiOutput("tekst_podsumowanie_1")
                 ))
        ),
        fluidRow(
          column(width = 8,
                 selectInput("podsumowanie_2", "Wybierz Imię", zmienne),
                 plotlyOutput("Podsumowanie_wykres2")
                 ),
          column(width = 4,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   uiOutput("tekst_podsumowanie_2")
                 ))
          )
      ),
      #-------------------------------------------------------------------------
      # Koniec panelu ogólnego
      #-------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------
      # Panel Java
      #-------------------------------------------------------------------------
      tabItem(
        tabName = "Java",
        fluidRow(style = "margin-top: 30px;",
                 box(
                   title = tags$p(icon("java"),"Java",
                                  style = "font-size: 200%;")
                 )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 9,
                 plotlyOutput("JavaWykres1")
          ),
          column(width = 3,
                 wellPanel(
                   style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
                   uiOutput("JavaWykres10")
                   )
                 )
        # ),
        # fluidRow(
          # column(width = 4,
          #        box(style = "margin-bottom: 20px; margin-left: 10px; margin-top: 150 px",
          #            "    Wykres przedstawia liczbę utworzonych plików z rozszerzeniem
          #            .java w czasie. Klikając na legendę można wybać osoby, do których
          #             dane będą się odnosić. Po najechaniu na słupek w danym kolorze można
          #            zobaczyć dokładną liczbę utworzonych plików w wybranym miesiącu. Dodatkowo
          #            jeżdżąc kursorem wzdłuż kolumny wyświetla się w dolnej części opisu informacja
          #            o ilości utworzonych plików z podziałem na dni.")
          #        )
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
                   title = tags$p("Najdłuższy wyraz:",
                                  style = "font-size: 125%; text-align: left;color: #FFFFFF;"),
                   htmlOutput("JavaWykres6"),
                   width = 12
                 )
          ),
          column(width = 6,
                 box(
                   title = tags$p("Ile wyrazów charakterystycznych dla Javy średnio zostało napisanych na plik:",
                                  style = "font-size: 125%; text-align: left;color: #FFFFFF;"),
                   selectizeInput("txtIn", "Wpisz wyraz charakterystyczny dla Javy",
                                  choices = java_keyword_list,
                                  selected = "protected",
                                  multiple = F, 
                                  options = list(create=F,
                                                 placeholder = 'Wpisz wyraz',
                                                 plugins = list('restore_on_backspace'))
                   ),
                   htmlOutput("JavaWykres7"),
                   width = 12
                 )
          )
        ),
        fluidRow(
          style = "margin-bottom: 80px;",
          column(width = 3,
                 selectizeInput(
                   inputId = "JavaSelectInput1",
                   label = "Wybierz osoby, do których dane będą się odnosiły",
                   choices = c("Mikołaj", "Małgosia", "Sebastian"),
                   selected = c("Mikołaj", "Małgosia", "Sebastian"),
                   multiple = TRUE,
                   options = list(create=F,
                                  placeholder = 'Wybierz imiona',
                                  plugins= list('remove_button')
                   )
                 )
          ),
          column(width = 4.5,
                 valueBoxOutput('JavaWykres8')
          ),
          column(width = 4.5,
                 valueBoxOutput('JavaWykres9')
          )
        )
      ),
      #-------------------------------------------------------------------------
      # Koniec panelu Java
      #-------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------
      # Panel Word
      #-------------------------------------------------------------------------
      tabItem(
        tabName = "Word",
        fluidRow(style = "margin-bottom: 20px; margin-left: 20px;",
                 box(title = "Word")),
        fluidRow(
          style = "margin-bottom: 80px; margin-top: 40px;",
          column(
            width = 8,
            plotlyOutput("wykres1_1")
          ),
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              uiOutput("tekst_word_1")
            )
          )
          
        ),
        fluidRow(
          style = "margin-bottom: 40px; margin-top: 40px;",
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              uiOutput("tekst_word_2")
            )
          ),
          column(
            width = 8,
            selectInput("zmienna",
                        "Wybierz Imię",
                        zmienne),
            plotlyOutput("wykres2", height = "500px")
          )
          
        ),
        fluidRow(
          style = "margin-bottom: 40px; margin-top: 40px;",
          column(
            width = 8,
            plotlyOutput("wykres3")
          ),
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              uiOutput("tekst_word_3")
            )
          )
          
        )
      ),
      #-------------------------------------------------------------------------
      # Koniec panelu Word
      #-------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------
      # Panel MATLAB
      #-------------------------------------------------------------------------
      tabItem(
      tabName = "MATLAB",
      fluidRow(style = "margin-left: 500px;",
               box(title = h4("Matlab", style = "font-size: 45px; margin-top: 60px"),
                   width = 12,  # Dostosuj szerokość pudełka według potrzeb
                   solidHeader = TRUE,
                   style = "font-size: 200px; margin: auto;"
               )),
      fluidRow(
            column( width = 8, style = "margin-bottom: 80px; margin-left: 20px; margin-top: 60 px",
            selectInput(
              inputId = "matlab5",
              label = "Wybierz imię",
              choices = c("Mikołaj", "Małgosia", "Sebastian"),
              selected = c("Mikołaj", "Małgosia", "Sebastian"),
              multiple = TRUE
            ),
            plotlyOutput("MATLABWykres5")
        ))
      ,
      fluidRow(
        style = "margin-bottom: 80px;",
        column(
          width = 8,
          selectInput(
            inputId = "matlab4",
            label = "Wybierz imię",
            choices = c("Mikołaj", "Małgosia", "Sebastian"),
            selected = c("Mikołaj", "Małgosia", "Sebastian"),
            multiple = TRUE
          ),
          plotlyOutput("MATLABWykres4")
        ),
        column(
          width = 4,
          wellPanel(
            style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
            uiOutput("tekst_matlab_1")
          )
        )),
        fluidRow( 
          style = "margin-bottom: 80px; margin-left: 20px;",
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              uiOutput("tekst_matlab_2")
            )
          ),
          column(
              width = 8,
                 selectInput(
                   inputId = "matlab1",
                   label = "Wybierz imię",
                   choices = c("Mikołaj", "Małgosia", "Sebastian"),
                   selected = c("Mikołaj", "Małgosia", "Sebastian"),
                   multiple = TRUE
                 ),
                 plotlyOutput("MATLABWykres1")
          )),
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
          ),
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              uiOutput("tekst_matlab_3")
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            wellPanel(
              style = "background-color: rgba(255, 255, 255, 0.0); border: 2px solid rgba(0, 0, 0, 0);",
              uiOutput("tekst_matlab_4")
            )
          ),
          column(width = 6,
                 plotlyOutput("MATLABWykres3")
          )
        )

      )
      #-------------------------------------------------------------------------
      # Koniec panelu MATLAB
      #-------------------------------------------------------------------------
      
    ),
  )
)

shinyApp(app_ui, server)

