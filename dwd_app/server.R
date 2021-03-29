# libraries
library(shiny)
library(leaflet)
library(dygraphs)
library(htmlwidgets)
library(sf)
library(deldir)
library(xts)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(plotly)
library(RColorBrewer)
library(reshape2)
library(Kendall)
library(climaemet)
##----------------- load functions -----------------------------------------

source("functions.R")

options(shiny.reactlog = TRUE) # ctrl + F3 to show graph(in Browser)
##-------------- server -----------------------------------------------


server <- function(input, output) {

  #dwd_stations <- get_DWD_stat_info()
  load("dwd_stations.Rdata")

  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)

  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })

  my_place <- reactive({

    if(!is.numeric(data_of_click$clickedMarker$lat)) {
      my_place <- "Dresden-Klotzsche"
      } else {
        my_place <- dwd_stations$Stationsname[dwd_stations$geoLaenge==data_of_click$clickedMarker$lng&
                                                     dwd_stations$geoBreite==data_of_click$clickedMarker$lat]
      }
  })

  output$station <- renderUI(h1(my_place()))

  # reactive function to get the data for the selected station
  get_dat <- reactive({
    get_DWD_data_hist(subset(dwd_stations, dwd_stations$Stationsname == my_place()))
    #load("test_data.Rdata")
    #return(test_d)
  })

  # data frame for calculations
  dat_calc <- reactive({
    data.frame(t = get_dat()$data$MESS_DATUM,
               var = get_dat()$data[, plt_var()])})

  # variable which to use
  plt_var <- reactive({
  tag_plt <- unique(get_dat()$meta_per[,c('Parameter','Parameterbeschreibung')])
  tag_plt <- tag_plt[tag_plt$Parameter %in% c("RSK", "SHK_TAG", "TMK", "TXK",
                                              "TNK"), ]
  return(tag_plt$Parameter[input$plotvar==tag_plt$Parameterbeschreibung])
  })

  # available variables
  av_var <- reactive({
    tag_plt <- unique(get_dat()$meta_per[,c('Parameter','Parameterbeschreibung')])
    tag_plt <- tag_plt[tag_plt$Parameter %in% c("RSK", "SHK_TAG", "TMK", "TXK",
                                                "TNK"), ]
    return(tag_plt$Parameterbeschreibung)
  })

  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
          leaflet() %>%
          setView(lat = 50.71158, lng = 12.99802, zoom = 10) %>%
          addProviderTiles("OpenStreetMap.HOT", layerId = 1,group = 'Landscape',
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addProviderTiles("OpenTopoMap", layerId = 1, group ='Topography',
                           options = providerTileOptions(noWrap = TRUE))%>%
      addMarkers(lat = dwd_stations$geoBreite,
                 lng = dwd_stations$geoLaenge,
                 label = dwd_stations$Stationsname,
                 layerId = 3,
                 labelOptions(layerId=6, noHide = FALSE, direction = 'auto'),
                 options=markerOptions(riseOnHover=TRUE),
                 group='Marker')  %>%

      addCircles(lat = dwd_stations$geoBreite,
                 lng = dwd_stations$geoLaenge,
                 popup = paste(dwd_stations$Stationsname,
                               "</br>ID:",
                               dwd_stations$Stations_id,
                               "\n </br>Höhe:",
                               dwd_stations$Stationshoehe, "m über NHN",
                               "\n </br>Daten von:",
                               dwd_stations$von_datum,
                               "\n </br>bis:",
                               dwd_stations$bis_datum),
                 layerId = 2, weight = 7,color='red',
                 opacity = 0.7, group='Stationen')  %>%

          addLayersControl(
            baseGroups = c('Landscape','Topography'),position = 'bottomright',
            options = layersControlOptions(collapsed = TRUE),
            overlayGroups = c('Stationen', 'Marker'))
  })

  output$plot <- renderDygraph({

    tsx <- xts(get_dat()$data[, plt_var()], get_dat()$data$MESS_DATUM)
    dygraph(tsx, xlab = 'Datum', main = my_place(),
            ylab = input$plotvar) %>%
      dySeries("V1", label = input$plotvar) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE)

  })

  # plot warming stripes
  output$warming <- renderPlotly({
    an <- an_mean(data.frame(t = get_dat()$data$MESS_DATUM,
                             var = get_dat()$data$TMK))
    pl <- ggstripes(data.frame(year = year(an$data$t),
                               temp = an$data$var), plot_title = my_place(),
                    plot_type = "stripes")
    ggplotly(p = pl)
  })

  # plot of mean values
  output$mean_year <- renderPlotly({

    if (input$monthly == FALSE) {
      pl <- an_mean(dat_calc())$plot + ylab(input$plotvar) + xlab("Jahr")
    } else {
      pl <- mon_mean(dat_calc())$plot[[as.numeric(input$month)]] +
        ylab(paste0(input$plotvar, " im ", month.abb[as.numeric(input$month)])) +
        xlab("Jahr")
    }
    if(input$lm) {
      pl <- pl + geom_smooth(aes(x = t, y = var, col = "Lineare Regression"), method = "lm")
    }
    if(input$loess) {
      pl <- pl + geom_smooth(aes(x = t, y = var, col = "loess Filter"), method = "loess")
    }
    ggplotly(p = pl)
  })

  sumTable <-  reactive({

    if(input$monthly == FALSE) {
      DF <- an_mean(dat_calc())$data
    } else {
      DF <- subset(mon_mean(dat_calc())$data, mon_mean(dat_calc())$data$month == input$month)
    }
    ts <- xts(DF$var, DF$t)

    ken <- MannKendall(ts)
    lm <- lm(var ~ t, DF)

    setNames(data.frame(c("Linearer Trend", "Mann Kendall Test"),
                        c("Slope", "Tau"),
                        c(lm$coefficients[2] * 365.25,
                          ken$tau[1]),
                        c(summary(lm)$coefficients[2, 2] * 365.25, NA),
                        c(summary(lm)$coefficients[2, 4], ken$sl[1])),
             c("Method", "Measure","Value", "Std. err", "p-value"))
  })
  # summary table of the linear fit
  output$sumTable <- renderTable({
        sumTable()
    }, digits = 4)

# plot of mean doy
  output$mean_doy <- renderPlotly({

    if(input$cumsum) {
      p <- doy_cum(dat_calc(), use_year = unlist(input$year_plt))
    } else {
      p <- doy_av(dat_calc(), use_year = unlist(input$year_plt))
    }
    ggplotly(p = p$plot + ylab(input$plotvar) + xlab("Jahr"))
  })

  # years to select
  output$sel_year <- renderUI({
    selectizeInput("year_plt", "Welche Jahre vergleichen",
                       choices = setNames(as.list(unique(year(dat_calc()$t))),
                                          unique(year(dat_calc()$t))),
                   multiple = TRUE)
  })

  # which variables are available
  output$plot_seclect <- renderUI({

    selectInput(inputId = "plotvar", #name of input
                label = "Welche Variable plotten?", #label displayed in ui
                choices = av_var())
  })
}

