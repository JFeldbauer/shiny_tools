library(leaflet)
library(dygraphs)
library(plotly)

ui <- fluidPage(
  HTML('<span lang = "de">'), # worakround; improve this when shiny supports global lang tag
  includeHTML("www/header_ihb_en.html"),
  titlePanel("DWD Wetterstationen"),
  sidebarLayout(
    sidebarPanel(
      htmlOutput("station"),
      h2('Weitere Einstellungen:'),
      conditionalPanel(condition="input.tabselected!=4",htmlOutput("plot_seclect")),
      conditionalPanel(condition="input.tabselected==3",
                       checkboxInput("cumsum", "Kummulative Jahrestageswerte", FALSE)),
      conditionalPanel(condition="input.tabselected==3", htmlOutput("sel_year")),
      conditionalPanel(condition="input.tabselected==2",
                       checkboxInput("lm", "Lineare Regression fitten", FALSE)),
      conditionalPanel(condition="input.tabselected==2",
                       checkboxInput("monthly", "Monatliche Mittelwerte", FALSE)),
      conditionalPanel(condition="input.monthly==true && input.tabselected==2",
                       selectInput("month", "Monat",
                                   choices = list(Januar = 1,
                                                  Februar = 2,
                                                  März = 3,
                                                  April = 4,
                                                  Mai = 5,
                                                  Juni = 6,
                                                  Juli = 7,
                                                  August = 8,
                                                  September = 9,
                                                  Oktober = 10,
                                                  November = 11,
                                                  Dezember = 12),
                                   multiple = FALSE)),
      style = "font-size:75%"),
    mainPanel(
      tabsetPanel(id = "tabselected",
        tabPanel("Karte und Rohwerte", value = 1,
                 leafletOutput("map", height="600px"),
                 dygraphOutput("plot", height="300px")),
        tabPanel("Mittelwerte", value = 2,
                 plotlyOutput("mean_year"),
                 conditionalPanel("input.lm==true", tableOutput("sumTable"))),
        tabPanel("Jahre vergleichen", value = 3,
                 plotlyOutput("mean_doy")),
        tabPanel("Warming Stripes", value = 4,
                 plotlyOutput("warming")),
        tabPanel("Info", value = 5,
                 withMathJax(includeMarkdown("info.md")))
      ))
  ),
  includeHTML("www/footer_en.html"), # <---
  HTML("</span>")
)