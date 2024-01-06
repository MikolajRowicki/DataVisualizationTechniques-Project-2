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
        tabName = "Word"
        # ,tags$h2('')
      ),
      tabItem(
        tabName = "MATLAB"
        # ,tags$h3('')
      )
    ),
    
  )
)




shinyApp(app_ui, server)