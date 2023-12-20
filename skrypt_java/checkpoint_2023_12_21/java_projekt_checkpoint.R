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
        y = "Średnia liczba komentarzów w linii na plik"
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
        y = "Średnia liczba komentarzów w bloku na plik"
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
}

# Tworzenie UI -----------------------------------------------------------------
app_ui <- dashboardPage(
  dashboardHeader(title = "Prototyp"),
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
    width = 250
  ),
  dashboardBody(
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
      ),
      tabItem(
        tabName = "Word"
      ),
      tabItem(
        tabName = "MATLAB"
      )
    )
  )
)


shinyApp(app_ui, server)