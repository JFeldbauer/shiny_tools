# packages
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
    dat <- get_dat()
    dat$data <- dat$data[year(dat$data$MESS_DATUM) >= input$time_ana[1] &
                           year(dat$data$MESS_DATUM) <= input$time_ana[2], ]
    data.frame(t = dat$data$MESS_DATUM,
               var = dat$data[, plt_var()])})

  # variable which to use
  plt_var <- reactive({
    tag_plt <- unique(get_dat()$meta_per[,c('Parameter','Parameterbeschreibung')])
    tag_plt <- tag_plt[tag_plt$Parameter %in% c("RSK", "SHK_TAG", "TMK", "TXK",
                                              "TNK"), ]
    return(tag_plt$Parameter[input$plotvar==tag_plt$Parameterbeschreibung])
  })

  # check quality of the data
  chk_qual <- reactive({
    # get the averaged years
    dc <- dat_calc()
    # emergency exit during installation (no data yet)
    if (nrow(dc) < 2) {
      return(list(av = 0, min30y = 0, zeitr = "empty"))
    }

    if (input$monthly == FALSE) {
      dat <- an_mean(dc)
    } else {
      dat <- mon_mean(dc)
      dat$data <- subset(dat$data, dat$data$month == as.numeric(input$month))
    }

    # a continuous vector with all years
    max_yrs <- seq(min(dat$data$year), max(dat$data$year), by = 1)

    # for how many years is data available
    av <- round(sum(dat$data$year %in% max_yrs)/length(max_yrs), 2) * 100

    # longest sequence of years where data is available
    lngths <- sapply(split(diff(dat$data$year),
          cumsum(seq_along(diff(dat$data$year)) %in% (which(diff(dat$data$year)!=1)+1)),
          drop = TRUE),
          length)
    # is there at least 30 years of undisturbed data
    min30y <- sum(dat$data$year %in% max_yrs) > 30 & any(lngths > 30)

    return(list(av = av,
                min30y = min30y,
                zeitr = paste0(min(dat$data$year), " - " , max(dat$data$year))))
  })

  # check if time span of selection is ok
  check_timespn <- reactive({
    # check empty argument, then time span is full
    if (is.null(input$time_ana)) return(TRUE)

    meta <- dwd_stations[dwd_stations$Stationsname == my_place(), ]

    # no if needed, comparison is boolean anyway
    return(
      input$time_ana[1] >= year(meta$von_datum) &
      input$time_ana[2] <= year(meta$bis_datum)
    )
  })

  # available variables
  av_var <- reactive({
    tag_plt <- unique(get_dat()$meta_per[,c('Parameter', 'Parameterbeschreibung')])
    tag_plt <- tag_plt[tag_plt$Parameter %in% c("RSK", "SHK_TAG", "TMK", "TXK",
                                                "TNK"), ]
    return(tag_plt$Parameterbeschreibung)
  })

  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
          leaflet() %>%
          setView(lat = 51.05, lng = 13.75, zoom = 10) %>%
          addProviderTiles("OpenStreetMap.HOT", layerId = 1, group = 'Landscape',
                           options = providerTileOptions(noWrap = TRUE)) %>%
          addProviderTiles("OpenTopoMap", layerId = 1, group ='Topography',
                           options = providerTileOptions(noWrap = TRUE))%>%
      addMarkers(lat = dwd_stations$geoBreite,
                 lng = dwd_stations$geoLaenge,
                 #label = dwd_stations$Stationsname,
                 layerId = 3,
                 label = lapply(paste(dwd_stations$Stationsname,
                               "</br>Daten von:", dwd_stations$von_datum,
                               "</br>bis:", dwd_stations$bis_datum), HTML),
                 labelOptions(layerId=6, noHide = FALSE, direction = 'auto'),
                 options=markerOptions(riseOnHover=TRUE),
                 group='Marker')  %>%

      # addCircles(lat = dwd_stations$geoBreite,
      #            lng = dwd_stations$geoLaenge,
      #            popup = paste(dwd_stations$Stationsname,
      #                          "</br>ID:",
      #                          dwd_stations$Stations_id,
      #                          "\n </br>Höhe:",
      #                          dwd_stations$Stationshoehe, "m über NHN",
      #                          "\n </br>Daten von:",
      #                          dwd_stations$von_datum,
      #                          "\n </br>bis:",
      #                          dwd_stations$bis_datum),
      #            layerId = 2, weight = 7,color='red',
      #            opacity = 0.7, group='Stationen')  %>%

          addLayersControl(
            baseGroups = c('Landscape', 'Topography'), position = 'bottomright',
            options = layersControlOptions(collapsed = TRUE),
            #overlayGroups = c('Stationen', 'Marker'))
            overlayGroups = c('Stationen'))
  })

  output$plot <- renderDygraph({
    if(check_timespn()) {
      tsx <- xts(dat_calc()$var, dat_calc()$t)
      # avoid error message during initialization if tsx is still empty
      if (!is.null(nrow(tsx))) {
        dygraph(tsx, xlab = 'Datum', main = my_place(),
                ylab = input$plotvar) %>%
          dySeries("V1", label = input$plotvar) %>%
          dyLegend(show = "always", hideOnMouseOut = FALSE)
      }
    } else {}
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
      pl <- pl + geom_smooth(aes(x = t, y = var, col = "Linearer Trend"), method = "lm")
    }
    if(input$loess) {
      pl <- pl + geom_smooth(aes(x = t, y = var, col = "Gleitendes Mittel (Loess-Filter)"), method = "loess")
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
                        c("Steigung", "Tau"),
                        c(lm$coefficients[2] * 365.25,
                          ken$tau[1]),
                        c(summary(lm)$coefficients[2, 2] * 365.25, NA),
                        c(summary(lm)$coefficients[2, 4], ken$sl[1])),
             c("Methode", "Parameter","Wert", "Standardfehler", "p-Wert"))
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

  # Quality of the data
  output$qual <- renderUI({
    p(paste0("Für ", chk_qual()$av, "% des gesammten Zeitraumes (",
             chk_qual()$zeitr,") sind ausreichend Daten vorhanden.\n",
             ifelse(chk_qual()$min30y, " Mindestens 30 Jahre Daten am Stück vorhanden.",
                    paste0(" Keine 30 Jahre Daten am Stück vorhanden.",
                           " Aussagen über Klimatrends sind nicht möglich."))))
  })

  # Zeitraum zum analysieren
  output$timespn <- renderUI({
    meta <- dwd_stations[dwd_stations$Stationsname == my_place(), ]
    sliderInput("time_ana", "Zeitraum wählen", year(meta$von_datum),
                year(meta$bis_datum), step = 1,
                value = c(year(meta$von_datum), year(meta$bis_datum)), sep = "")
  })
outputOptions(output, "timespn", priority = 10)
  # Station meta informations
  output$meta_stat <- renderUI({
    id <- dwd_stations$Stationsname == my_place()
    p1 <- paste(strong("ID:"), dwd_stations$Stations_id[id])
    p2 <- paste(strong("Höhe:"), dwd_stations$Stationshoehe[id], "m über NHN")
    p3 <- paste(strong("Daten von:"), dwd_stations$von_datum[id])
    p4 <- paste(strong("bis:"), dwd_stations$bis_datum[id])

    HTML(paste(p1, p2, p3, p4, sep = '<br/>'))
  })
}

