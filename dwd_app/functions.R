get_DWD_stat_info <- function(){
  # get data from the DWD server
  url_stations <- paste0("https://opendata.dwd.de/climate_environment/",
                         "CDC/observations_germany/climate/daily/kl/historical/",
                         "KL_Tageswerte_Beschreibung_Stationen.txt")
  widths <- nchar(read.table(url_stations, header = FALSE, nrows = 1, skip = 1))
  widths <- widths + c(-5, -1, 0, 5, 0 , 2, 0, 14)
  stations <- read.fwf(url_stations, header = FALSE, skip = 2, widths = widths,
                       fileEncoding = "iso-8859-1")
  colnames(stations) <- read.table(url_stations, header = FALSE, nrows = 1)
  stations$von_datum <- ymd(stations$von_datum)
  stations$bis_datum <- ymd(stations$bis_datum)
  stations$Stationsname <- gsub("\\s*$", "", stations$Stationsname, perl = TRUE)
  stations$Bundesland <- gsub("\\s*$", "", stations$Bundesland, perl = TRUE)
  stations$Stations_id <- gsub("^0", "",
                               sapply(stations$Stations_id,
                                      function(s) paste0(paste0(rep("0", 6 - ceiling(log10(s + 0.5))),
                                                                collapse = ""), s)), perl = TRUE)
  # historical data only goes up to the end of 2019
  stations$bis_datum[stations$bis_datum > ymd(20191231)] <- ymd(20191231)

  return(stations)
}

## function to get DWD data (historic) for a certain station
get_DWD_data_hist <- function(stat_meta) {
  # url to the meteo file
  url_stat_dat <- paste0("https://opendata.dwd.de/climate_environment/CDC/",
                         "observations_germany/climate/daily/kl/historical/",
                         "tageswerte_KL_", stat_meta$Stations_id, "_",
                         format(stat_meta$von_datum, "%Y%m%d"), "_",
                         format(stat_meta$bis_datum, "%Y%m%d"), "_hist.zip")

  # download and read in the data from the zip file
  temp <- tempfile()
  tryCatch(download.file(url_stat_dat, temp),
           error = function(e){warning(paste0("No daily data found for station "),
                                       stat_meta$Stationsname)})
  dat_met <- read.table(unz(temp, paste0("produkt_klima_tag_",
                                         format(stat_meta$von_datum, "%Y%m%d"), "_",
                                         format(stat_meta$bis_datum, "%Y%m%d"),
                                         "_", stat_meta$Stations_id, ".txt")),
                        sep = ";", header = TRUE, fileEncoding = "iso-8859-1", na.strings = -999)
  # change format of date
  dat_met$MESS_DATUM <- ymd(dat_met$MESS_DATUM)

  # meta data for the parameters
  meta_name <- paste0("Metadaten_Parameter_klima_tag_", stat_meta$Stations_id, ".txt")

  meta_dat <- read.table(unz(temp, meta_name), sep = ";", header = TRUE,
                         nrow = length(readLines(unz(temp, meta_name))) - 3,
                         fileEncoding = "iso-8859-1")
  # change format of date
  meta_dat$Von_Datum <- ymd(meta_dat$Von_Datum)
  meta_dat$Bis_Datum <- ymd(meta_dat$Bis_Datum)

  # meta data for the location
  geo_name <- paste0("Metadaten_Geographie_", stat_meta$Stations_id, ".txt")

  geo_meta <- read.table(unz(temp, geo_name), sep = ";", header = TRUE,
                         fileEncoding = "iso-8859-1")
  # change format of date
  geo_meta$von_datum <- ymd(geo_meta$von_datum)
  geo_meta$bis_datum <- ymd(geo_meta$bis_datum)

  unlink(temp)

  # remove NAs
  for(c in colnames(dat_met)) {
    if(!c %in% c("STATIONS_ID", "MESS_DATUM", "eor")) {
      id <- which(dat_met[, c] == -999)
      dat_met[id, c] <- NA
    }
  }

  # generic data definition
  return(list(data = dat_met,
              meta_per = meta_dat,
              meta_geo = geo_meta))
}

## funciton to calculate annual average values for a data.frame
an_mean <- function(data, min_c_year = 333){

  an_dat <- aggregate(data, by = list(year = year(data$t)), mean, na.rm = TRUE)
  # count the number of obervations in the year
  an_dat$count <- aggregate(data$var, by = list(year = year(data$t)), function(x)sum(!is.na(x)))$x

  # remove years with less than a given number of data
  an_dat$var[an_dat$count < min_c_year] <- NA
  an_ts <- xts(x = an_dat$var, order.by = an_dat$t)

  # plot data
  p <- ggplot(an_dat) + geom_point(aes(x = t, y = var))

  return(list(data = an_dat,
              ts = an_ts,
              plot = p))
}


## funciton to calculate monthly average values for a data.frame
mon_mean <- function(data, min_c_mon = 15) {

  mon_dat <- aggregate(data, by = list(year = year(data$t),
                                       month = month(data$t)), mean, na.rm = TRUE)
  mon_dat$count <- aggregate(data$var, by = list(year = year(data$t),
                                                 month = month(data$t)), function(x)sum(!is.na(x)))$x

  # remove years with less than a given number of data
  mon_dat$var[mon_dat$count < min_c_mon] <- NA

  mon_ts <- xts(x = mon_dat$var, order.by = mon_dat$t)

  p <- list()
  for (m in 1:12) {
    mon_sub <- subset(mon_dat, mon_dat$month == m)
    p[[m]] <- ggplot(mon_sub) + geom_point(aes(x = t, y = var))

  }

  return(list(data = mon_dat,
              ts = mon_ts,
              plot = p))
}


## funciton to calculate average day of the year values for a data.frame
doy_av <- function(data, use_year = c(1960, 1970, 1980, 1990, 2000, 2010)){

  dly_dat <- data
  dly_dat$year <- year(data$t)
  dly_dat$doy <- yday(data$t)

  # calculate average year
  dly_mean <- aggregate(data.frame(var = dly_dat$var), list(doy = dly_dat$doy), mean, na.rm = TRUE)
  dly_mean$year <- "average"
  dly_mean$q5 <- aggregate(data.frame(var = dly_dat$var), list(doy = dly_dat$doy),
                           quantile, 0.05, na.rm = TRUE)$var
  dly_mean$q95 <- aggregate(data.frame(var = dly_dat$var), list(doy = dly_dat$doy),
                            quantile, 0.95, na.rm = TRUE)$var


  # select which years to plot
  dly_dat_s <- subset(dly_dat, dly_dat$year %in% use_year)
  dly_dat_s <- rbind(dly_dat_s[, 2:4], dly_mean[, 1:3])
  p <- ggplot(dly_dat_s) + geom_ribbon(data = dly_mean, aes(x = doy, ymin = q5, ymax = q95),
                                       col = "grey42", fill = "grey42") +
    geom_line(aes(x = doy, y = var, col = year)) +
    scale_color_manual(values = c(brewer.pal(length(use_year), "Dark2"), "black"))

  return(list(data = dly_dat_s,
              plot = p))
}

## funciton to calculate cumulative day of the year values for a data.frame
doy_cum <- function(data,   use_year = c(1970, 1980, 1990, 2000, 2010, 2018, 2019)){
  dly_dat <- data
  dly_dat$year <- year(data$t)
  dly_dat$doy <- yday(data$t)
  # cumulative plot
  dly_dat_c <- lapply(unique(dly_dat$year), function(y){
    tmp_dat <- subset(dly_dat, dly_dat$year == y)
    tmp_dat$var[is.na(tmp_dat$var)] <- 0
    cms <- cumsum(tmp_dat$var)
    return(data.frame(doy = tmp_dat$doy,
                      var = cms,
                      year = tmp_dat$year))})
  # melt list to long data frame
  dly_dat_c <- melt(dly_dat_c, id.vars = c("doy", "year", "var"))

  # mean cumulative variable
  dly_c_mean <- aggregate(list(var = dly_dat_c$var), by = list(doy = dly_dat_c$doy),
                          mean, na.rm = TRUE)
  dly_c_mean$year <- "average"
  dly_c_mean$q5 <- aggregate(list(var = dly_dat_c$var), by = list(doy = dly_dat_c$doy),
                             quantile, 0.05, na.rm = TRUE)$var
  dly_c_mean$q95 <- aggregate(list(var = dly_dat_c$var), by = list(doy = dly_dat_c$doy),
                              quantile, 0.95, na.rm = TRUE)$var



  dly_dat_s_c <- subset(dly_dat_c, dly_dat_c$year %in% use_year)
  dly_dat_s_c <- rbind(dly_dat_s_c[, 1:3], dly_c_mean[, 1:3])
  dly_dat_s_c <- dly_dat_s_c[dly_dat_s_c$doy < 366, ]
  p <- ggplot(dly_dat_s_c) + geom_ribbon(data = dly_c_mean, aes(x = doy, ymin = q5, ymax = q95),
                                         col = "grey42", fill = "grey42") +
    geom_line(aes(x = doy, y = var, col = year)) +
    scale_color_manual(values = c(brewer.pal(length(use_year), "Dark2"), "black"))

  return(list(data = dly_dat_s_c,
              plot = p))
}
