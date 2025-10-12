# R/utils.R
# Funzioni generali utilizzate in varie parti del progetto
library(data.table)

#' Pulizia e normalizzazione dati ARPAE
#'
#' Pulisce e converte i dati scaricati dall'API ARPAE:
#' - rimuove colonne inutili,
#' - normalizza i nomi delle colonne,
#' - converte `reftime` in `POSIXct`,
#' - trasforma le colonne numeriche in numeric,
#' - lascia `v_flag` come carattere.
#'
#' @param dt Oggetto `data.table` con i dati scaricati da ARPAE.
#'
#' @return Un `data.table` con colonne normalizzate e convertite.
#' @examples
#' \dontrun{
#' raw <- data.table::data.table(
#'   reftime = "08/21/2025 02:00", value = "15", v_flag = "G"
#' )
#' pulisci_dati(raw)
#' }
pulisci_dati <- function(dt) {
  
  # rimuove la colonna _full_text se presente
  if ("_full_text" %in% names(dt)) dt[, `_full_text` := NULL]
  
  # normalizza nomi colonne
  clean_names <- tolower(gsub("[^a-zA-Z0-9]", "_", names(dt)))
  
  # evita che inizino con underscore
  clean_names <- ifelse(substr(clean_names, 1, 1) == "_", gsub("_", "", clean_names), clean_names)
  
  setnames(dt, clean_names)
  
  # converte reftime in POSIXct
  if ("reftime" %in% names(dt)) {
    dt[, reftime := as.POSIXct(reftime, format = "%m/%d/%Y %H:%M", tz = "UTC")]
  }
  
  # colonne da convertire in numerico
  num_cols <- intersect(c("id", "station_id", "variable_id", "value"), names(dt))
  for (col in num_cols) {
    dt[[col]] <- as.numeric(dt[[col]])
  }
  
  # v_flag rimane character
  if ("v_flag" %in% names(dt)) dt[, v_flag := as.character(v_flag)]
  
  dt
}

#' Crea una versione pulita di una stringa
#'
#' Questa funzione converte tutte le lettere di una stringa in minuscolo,
#' sostituendo spazi e trattini con underscore (`_`) e normalizzando le 
#' lettere `ì` in `i`. È utile per creare nomi di file o identificatori
#' coerenti e privi di caratteri speciali.
#'
#' @param x Una stringa (o un vettore di stringhe) da normalizzare.
#'
#' @return Una stringa con spazi e trattini sostituiti da underscore,
#'         e le lettere `ì` sostituite da `i`.
#'
#' @examples
#' slugify("Reggio Emilia")
#' #> "Reggio_Emilia"
#'
#' slugify("Forlì-Cesena")
#' #> "Forli_Cesena"
#'
#' @export
slugify <- function(x) {
  x <- tolower(x)
  x <- gsub(" ", "_", x)
  x <- gsub("-", "_", x)
  x <- gsub("ì", "i", x)
  x
}

#' @title Mostra la data e l'ora dell'ultimo aggiornamento automatico
#' @description
#' Legge la data/ora di build dal sistema o da una variabile d'ambiente impostata
#' (ad esempio da una GitHub Action) e restituisce una riga HTML che indica
#' l'ultimo aggiornamento automatico del sito o dei report.
#'
#' Se la variabile d'ambiente `GITHUB_RUN_TIMESTAMP` non è impostata, mostra un
#' messaggio di fallback con "dato non disponibile".
#'
#' @details
#' La funzione è pensata per essere inclusa all’interno di documenti Quarto (`.qmd`)
#' o report HTML. Il testo è formattato in modo discreto, con caratteri piccoli e
#' colorazione neutra.
#'
#' @return
#' Nessun valore restituito: la funzione produce direttamente un output HTML
#' mediante `cat()`.
#'
#' @examples
#' \dontrun{
#'   mostra_data_build()
#' }
#'
#' @export
mostra_data_build <- function() {
  # Legge la data/ora di build dalla variabile d'ambiente impostata nella GitHub Action
  build_time <- Sys.getenv("GITHUB_RUN_TIMESTAMP")
  
  if (nzchar(build_time)) {
    # Converte la stringa ISO in oggetto POSIXct
    build_time <- as.POSIXct(build_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    
    # Stampa HTML formattato con data leggibile
    cat(
      sprintf(
        '<p style="text-align:center; font-size: 0.9em; color: #555;">
        Ultimo aggiornamento automatico: <strong>%s UTC</strong>
        </p>',
        format(build_time, "%d %B %Y, ore %H:%M")
      )
    )
  } else {
    # Caso in cui la variabile non sia disponibile
    cat('<p style="text-align:center; font-size: 0.9em; color: #555;">
         Ultimo aggiornamento automatico: <strong>dato non disponibile</strong>
         </p>')
  }
}