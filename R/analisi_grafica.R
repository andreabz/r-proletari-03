# R/analisi_grafica.R
# Funzioni per la creazione di grafici della qualità dell'aria
library(ggplot2)

#' Grafico orario per O3 con facet per stazione
#'
#' @param dati data.table con dati orari
#' @details Grafico con ore del giorno sull'asse X, concentrazione O3 (µg/m³) sull'asse Y,
#'          punti e linea per le concentrazioni orarie, linea orizzontale per limite orario 180 µg/m³,
#'          facet per stazione (una colonna, tante righe quante le stazioni).
grafico_o3 <- function(dati) {
  okabe_ito <- c("O3" = "#BA1212")
  limite_color <- "#000000"
  
  x <- dati[parametro == "O3 (Ozono)"]
  nome_stazione <- unique(x$stazione)
  lim_orario <- 180
  
  ggplot(x, aes(x = reftime, y = value)) +
    geom_point(color = okabe_ito["O3"], size = 2) +
    geom_line(aes(group = 1), color = okabe_ito["O3"], alpha = 0.4, linewidth = 1.2) +
    geom_hline(yintercept = lim_orario, linetype = "dashed", color = limite_color, linewidth = 1) +
    scale_x_datetime(date_labels = "%H:%M", name = "Ora del giorno") +
    labs(
      y = expression("Concentrazione O"[3]*" (µg/m³)"),
      title = expression("Concentrazioni orarie per il parametro ozono (O"[3]*")")
    ) +
    facet_wrap(~ stazione, ncol = 1, scales = "free_y") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "#BA1212"),
      strip.text = element_text(color = "white", face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)
    )
}

#' Grafico orario per NO2 con facet per stazione
#'
#' @param dati data.table con dati orari
#' @details Grafico con ore del giorno sull'asse X, concentrazione NO2 (µg/m³) sull'asse Y,
#'          punti e linea per le concentrazioni orarie, linea orizzontale per limite orario 200 µg/m³,
#'          facet per stazione (una colonna, tante righe quante le stazioni).
grafico_no2 <- function(dati) {
  okabe_ito <- c("NO2" = "#BA1212")
  limite_color <- "#000000"
  
  x <- dati[parametro == "NO2 (Biossido di azoto)"]
  nome_stazione <- unique(x$stazione)
  lim_orario <- 200
  
  ggplot(x, aes(x = reftime, y = value)) +
    geom_point(color = okabe_ito["NO2"], size = 2) +
    geom_line(aes(group = 1), color = okabe_ito["NO2"], alpha = 0.4, linewidth = 1.2) +
    geom_hline(yintercept = lim_orario, linetype = "dashed", color = limite_color, linewidth = 1) +
    scale_x_datetime(date_labels = "%H:%M", name = "Ora del giorno") +
    labs(
      y = expression("Concentrazione NO"[2]*" (µg/m³)"),
      title = expression("Concentrazioni orarie per il parametro diossido di azoto (NO"[2]*")")
    ) +
    facet_wrap(~ stazione, ncol = 1, scales = "free_y") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "#BA1212"),
      strip.text = element_text(color = "white", face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)
    )
}

#' Grafico orario per SO2 con facet per stazione
#'
#' @param dati data.table con dati orari
#' @details Grafico con ore del giorno sull'asse X, concentrazione SO2 (µg/m³) sull'asse Y,
#'          punti e linea per le concentrazioni orarie, linea orizzontale per limite orario 350 µg/m³,
#'          facet per stazione (una colonna, tante righe quante le stazioni).
grafico_so2 <- function(dati) {
  okabe_ito <- c("SO2" = "#BA1212")
  limite_color <- "#000000"
  
  x <- dati[parametro == "SO2 (Biossido di zolfo)"]
  nome_stazione <- unique(x$stazione)
  lim_orario <- 350
  
  ggplot(x, aes(x = reftime, y = value)) +
    geom_point(color = okabe_ito["SO2"], size = 2) +
    geom_line(aes(group = 1), color = okabe_ito["SO2"], alpha = 0.4, linewidth = 1.2) +
    geom_hline(yintercept = lim_orario, linetype = "dashed", color = limite_color, linewidth = 1) +
    scale_x_datetime(date_labels = "%H:%M", name = "Ora del giorno") +
    labs(
      y = expression("Concentrazione SO"[2]*" (µg/m³)"),
      title = expression("Concentrazioni orarie per il parametro diossido di zolfo (SO"[2]*")")
    ) +
    facet_wrap(~ stazione, ncol = 1, scales = "free_y") +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "#BA1212"),
      strip.text = element_text(color = "white", face = "bold", size = 10),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(face = "italic", size = 12, hjust = 0.5)
    )
}

#' Wrapper grafico per i principali parametri con facet per stazione
#'
#' @param dati data.table con dati orari
#' @param parametro stringa: "O3", "NO2", "SO2"
#' @details Restituisce il grafico ggplot del parametro scelto per tutte le stazioni presenti nel dataset,
#'          usando punti e linea, limite orario con annotazione e facet per stazione (una colonna).
grafico_parametro <- function(dati, parametro) {
  if (parametro == "O3") {
    grafico_o3(dati)
  } else if (parametro == "NO2") {
    grafico_no2(dati)
  } else if (parametro == "SO2") {
    grafico_so2(dati)
  } else {
    stop("Parametro non supportato per la visualizzazione grafica")
  }
}
