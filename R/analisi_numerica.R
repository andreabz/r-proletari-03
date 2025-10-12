# R/analisi_numerica.R
# Funzioni per il valutare il rispetto dei limiti imposti dal D.Lgs. 155/2010.
library(data.table)

#' Controlla i valori giornalieri di PM10 rispetto al limite
#'
#' @param dati data.table con colonne: station_id, stazione, comune, reftime, parametro, value
#' @details Restituisce per ciascuna stazione e giorno il valore di PM10 e un indicatore logico se supera il limite di 50 µg/m³
check_pm10 <- function(dati) {
  dati[parametro == "PM10",
       .(station_id, stazione, comune,
         date = reftime,
         val_pm10 = value,
         supera_50 = value > 50)]
}

#' Calcola il rapporto PM2.5/PM10
#'
#' @param dati data.table con colonne: station_id, stazione, comune, reftime, parametro, value
#' @details Restituisce per ciascuna stazione e giorno i valori di PM2.5, PM10 e il loro rapporto
calc_pm25 <- function(dati) {
  pm10 <- dati[parametro == "PM10",
               .(station_id, stazione, comune, date = reftime, val_pm10 = value)]
  pm25 <- dati[parametro == "PM2.5",
               .(station_id, stazione, comune, date = reftime, val_pm25 = value)]
  merged <- merge(pm25, pm10,
                  by = c("station_id", "stazione", "comune", "date"),
                  all = FALSE)
  merged[, ratio_pm25_pm10 := val_pm25 / val_pm10]
  merged
}

#' Controlla i valori orari di NO2 rispetto ai limiti normativi
#'
#' @param dati data.table con colonne: station_id, stazione, comune, reftime, parametro, value
#' @details Restituisce per ciascuna stazione e giorno:
#' - valore minimo e massimo orario  
#' - numero di ore con valore > 200 µg/m³  
#' - numero di sequenze di 3 ore consecutive con valore > 400 µg/m³
check_no2 <- function(dati) {
  dati[parametro == "NO2 (Biossido di azoto)"][
    , .(
      min_val = min(value, na.rm = TRUE),
      max_val = max(value, na.rm = TRUE),
      n_supera_200 = sum(value > 200, na.rm = TRUE),
      n_supera_400_3h = {
        r <- rle(value > 400)
        sum(r$lengths[r$values] >= 3)
      }
    ), by = .(station_id, stazione, comune, date = as.Date(reftime))
  ]
}

#' Controlla i valori orari di SO2 rispetto al limite orario
#'
#' @param dati data.table
#' @details Restituisce per ciascuna stazione e giorno il valore minimo e massimo orario e il numero di superamenti del limite di 350 µg/m³
check_so2 <- function(dati) {
  dati[parametro == "SO2 (Biossido di zolfo)",
       .(min_val = min(value, na.rm = TRUE),
         max_val = max(value, na.rm = TRUE),
         n_supera_350 = sum(value > 350, na.rm = TRUE)),
       by = .(station_id, stazione, comune, date = as.Date(reftime))]
}

#' Controlla i valori orari di CO usando media mobile 8 ore
#'
#' @param dati data.table con dati orari di CO
#' @details Restituisce per ciascuna stazione e giorno il valore minimo e massimo della media mobile 8 ore e il numero di superamenti del limite di 10 mg/m³ (10000 µg/m³)
check_co <- function(dati) {
  x <- dati[parametro == "CO (Monossido di carbonio)"]
  setorder(x, station_id, reftime)
  x[, rollmean_8h := frollmean(value, n = 8, align = "right"), by = station_id]
  x[, .(min_8h = min(rollmean_8h, na.rm = TRUE),
        max_8h = max(rollmean_8h, na.rm = TRUE),
        n_supera_10 = sum(rollmean_8h > 10000, na.rm = TRUE)),
    by = .(station_id, stazione, comune, date = as.Date(reftime))]
}

#' Controlla i valori orari di O3 rispetto ai limiti normativi
#'
#' @param dati data.table con dati orari di O3
#' @details Restituisce per ciascuna stazione e giorno:
#' - minimo e massimo della media mobile 8 ore  
#' - numero di superamenti della media mobile 8 ore > 120 µg/m³  
#' - numero di ore singole > 180 µg/m³  
#' - numero di sequenze di 3 ore consecutive > 240 µg/m³  
#' - minimo e massimo della media oraria
check_o3 <- function(dati) {
  x <- dati[parametro == "O3 (Ozono)"]
  setorder(x, station_id, reftime)
  x[, rollmean_8h := frollmean(value, n = 8, align = "right"), by = station_id]
  x[
    , .(
      min_oraria = min(value, na.rm = TRUE),
      max_oraria = max(value, na.rm = TRUE),
      min_8h = min(rollmean_8h, na.rm = TRUE),
      max_8h = max(rollmean_8h, na.rm = TRUE),
      n_supera_120 = sum(rollmean_8h > 120, na.rm = TRUE),
      n_supera_180_orario = sum(value > 180, na.rm = TRUE),
      n_supera_240_3h = {
        r <- rle(value > 240)
        sum(r$lengths[r$values] >= 3)
      }
    ), by = .(station_id, stazione, comune, date = as.Date(reftime))]
}

#' Genera il report per un singolo parametro
#'
#' @param dati data.table con dati ambientali
#' @param parametro stringa che indica il parametro da analizzare
#'        ("PM10", "PM2.5", "NO2", "SO2", "CO", "O3")
#' @details Applica la funzione di controllo appropriata al parametro scelto
#'          e restituisce un data.table con tutti gli indici calcolati.
report_parametro <- function(dati, parametro) {
  if (parametro == "PM10") {
    check_pm10(dati)
  } else if (parametro == "PM2.5") {
    calc_pm25(dati)
  } else if (parametro == "NO2") {
    check_no2(dati)
  } else if (parametro == "SO2") {
    check_so2(dati)
  } else if (parametro == "CO") {
    check_co(dati)
  } else if (parametro == "O3") {
    check_o3(dati)
  } else {
    stop("Parametro non supportato")
  }
}

#' Genera il report giornaliero per tutti i parametri principali
#'
#' @param dati data.table con dati ambientali
#' @details Applica tutte le funzioni di controllo sui parametri principali
#'          e restituisce una lista di data.table, uno per ciascun parametro.
#'          La tabella di O3 ora include anche minimo e massimo della media oraria,
#'          oltre ai superamenti orari e a 3 ore consecutive.
report_giornaliero <- function(dati) {
  parametri <- c("PM10", "PM2.5", "NO2", "SO2", "CO", "O3")
  out <- lapply(parametri, function(p) report_parametro(dati, p))
  names(out) <- parametri
  out
}
