# R/report_helper.R
# Funzioni di supporto per generazione report giornaliero qualità dell'aria
#' Questo file contiene funzioni per:
#' - generare sezioni del report per ciascun parametro
#' - commentare automaticamente i superamenti dei valori limite
#'   secondo quanto previsto dal D.Lgs. 155/2010
library(data.table)
library(ggplot2)
library(gt)
library(knitr)
library(kableExtra)

#' Etichette delle colonne adattive per HTML e PDF
#'
#' Restituisce le etichette delle colonne con i giusti a capo a seconda del formato di output
#' (HTML o LaTeX PDF).
#'
#' @param parametro Nome del parametro (es. "PM10", "NO2", "O3").
#' @return Vettore nominato con le etichette delle colonne.
#' @export
etichette_colonne <- function(parametro) {
  is_latex <- knitr::is_latex_output()
  
  # funzione interna per gestire il formato degli a capo
  myfmt <- function(...) {
    if (is_latex) {
      paste0("\\makecell[r]{", paste(c(...), collapse = " \\\\ "), "}")
    } else {
      paste(c(...), collapse = "<br>")
    }
  }
  
  
  switch(parametro,
         "PM10" = c(
           stazione   = "Stazione",
           comune     = "Comune",
           val_pm10   = myfmt("Concentrazione", "(µg/m³)")
         ),
         "PM2.5" = c(
           stazione        = "Stazione",
           comune          = "Comune",
           val_pm25        = myfmt("Concentrazione", "(µg/m³)"),
           ratio_pm25_pm10 = "PM2.5/PM10"
         ),
         "NO2" = c(
           stazione        = "Stazione",
           comune          = "Comune",
           min_val         = myfmt("Min", "(µg/m³)"),
           max_val         = myfmt("Max", "(µg/m³)"),
           n_supera_200    = "≥ 200 µg/m³",
           n_supera_400_3h = myfmt("≥ 400 µg/m³", "per 3h")
         ),
         "SO2" = c(
           stazione  = "Stazione",
           comune    = "Comune",
           min_val   = myfmt("Min", "(µg/m³)"),
           max_val   = myfmt("Max", "(µg/m³)")
         ),
         "CO" = c(
           stazione    = "Stazione",
           comune      = "Comune",
           min_8h      = myfmt("Min 8h MA", "(mg/m³)"),
           max_8h      = myfmt("Max 8h MA", "(mg/m³)"),
           n_supera_10 = "≥ 10 mg/m³"
         ),
         "O3" = c(
           stazione            = "Stazione",
           comune              = "Comune",
           min_oraria          = myfmt("Min", "(µg/m³)"),
           max_oraria          = myfmt("Max", "(µg/m³)"),
           min_8h              = myfmt("Min 8h MA", "(µg/m³)"),
           max_8h              = myfmt("Max 8h MA", "(µg/m³)"),
           n_supera_120        = "≥ 120 µg/m³",
           n_supera_180_orario = "≥ 180 µg/m³",
           n_supera_240_3h     = myfmt("≥ 240 µg/m³", "per 3h")
         ),
         NULL
  )
}

#' Etichette dei parametri in forma testuale e simbolica
#'
#' Restituisce sia il nome esteso del parametro (es. "Ozono") sia
#' la sua rappresentazione simbolica (es. O pedice 3).
#'
#' @param parametro Stringa del parametro (es. "O3").
#' @return Una lista con elementi `nome` (carattere) e `simbolo` (espressione).
#' @export
etichette_parametro <- function(parametro) {
  switch(parametro,
         "PM10"  = list(nome = "Polveri sottili PM10", simbolo = "PM₁₀"),
         "PM2.5" = list(nome = "Polveri sottili PM2.5", simbolo = "PM₂.₅"),
         "NO2"   = list(nome = "Diossido di azoto", simbolo = "NO₂"),
         "SO2"   = list(nome = "Diossido di zolfo", simbolo = "SO₂"),
         "CO"    = list(nome = "Monossido di carbonio", simbolo = "CO"),
         "O3"    = list(nome = "Ozono", simbolo = "O₃"),
         list(nome = parametro, simbolo = parametro)
  )
}

#' Dizionario dei tipi di superamento normativo
#'
#' Restituisce la descrizione normativa di un superamento e l'articolo corretto
#' (maschile/femminile) per garantire concordanza grammaticale.
#'
#' @param tipo Codice interno che identifica il tipo di limite/soglia.
#' @return Una lista con `label` (stringa) e `articolo` ("il" o "la").
#' @export
descrizione_superamento <- function(tipo) {
  switch(tipo,
         "limite_200"        = list(label = "valore limite orario (200 µg/m³)", articolo = "il"),
         "limite_350"        = list(label = "valore limite orario (350 µg/m³)", articolo = "il"),
         "soglia_info"       = list(label = "soglia di informazione (180 µg/m³)", articolo = "la"),
         "soglia_allarme_NO2"= list(label = "soglia di allarme (400 µg/m³, 3h)", articolo = "la"),
         "valore_obiettivo"  = list(label = "valore obiettivo (120 µg/m³, media mobile 8h)", articolo = "il"),
         "limite_CO"         = list(label = "valore limite media mobile 8h (10 mg/m³)", articolo = "il"),
         "soglia_allarme_O3" = list(label = "soglia di allarme (240 µg/m³, 3h)", articolo = "la"),
         NULL
  )
}

#' Genera la tabella di riepilogo per un parametro
#'
#' @param parametro Nome del parametro (es. "NO2")
#' @param res Data.table con i risultati per il parametro
#' @return Una tabella formattata (gt per HTML, kable per PDF)
#' @export
tabella_parametro <- function(parametro, res) {
  data <- res$date[1]
  lab <- etichette_parametro(parametro)
  # seleziono colonne di interesse
  coltbl <- colnames(res)
  
  # colonne da escludere
  exclude <- c("station_id", "date", "reftime")
  if (parametro == "PM10") exclude <- c(exclude, "supera_50")
  if (parametro == "PM2.5") exclude <- c(exclude, "val_pm10")
  
  coltbl <- setdiff(colnames(res), exclude)
  tbl <- res[, ..coltbl]
  
  # seleziono le colonne numeriche
  nums <- names(tbl)[vapply(tbl, is.numeric, logical(1))]
  
  # etichette delle colonne (senza toccare i nomi originali)
  labels <- etichette_colonne(parametro)
  label_map <- labels[names(labels) %in% coltbl]
  
  if (knitr::is_latex_output()) {
    # === LATEX ===
    tbl[, (nums) := lapply(.SD, function(x) signif(x, 2)), .SDcols = nums]
    
    # rinomina solo nella copia per la stampa
    col_names <- colnames(tbl)
    for (i in seq_along(col_names)) {
      if (col_names[i] %in% names(label_map)) {
        col_names[i] <- label_map[[col_names[i]]]
      }
    }
    setnames(tbl, old = colnames(tbl), new = col_names)
    
    knitr::kable(tbl, format = "latex", booktabs = TRUE,
                 caption = paste("Tabella dei risultati per", lab$nome,
                                 "misurati il", data),
                 escape = FALSE) |>
      kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
      print()
    
  } else {
    # === HTML ===
    tabella <- gt::gt(tbl) |>
      gt::fmt_number(columns = nums, n_sigfig = 2) |>
      gt::cols_label(.list = lapply(label_map, function(x) gt::html(x))) |>
      gt::tab_caption(paste("Tabella dei risultati per", lab$nome,
                            "misurati il", data))
    print(tabella)
  }
}

#' Crea una sezione del report per un parametro specifico
#'
#' Questa funzione genera una sezione completa del report per un parametro:
#' - titolo e sottotitolo in stile militante-satirico
#' - tabella riassuntiva dei valori con intestazioni leggibili e caption
#' - grafico temporale (se disponibile) con caption
#' - commento testuale automatizzato in base ai superamenti normativi
#'
#' @param parametro Nome del parametro (es. "NO2").
#' @param report Oggetto restituito da `report_giornaliero()`.
#' @param dati Dataset con i risultati.
#' @param width Larghezza del grafico (in pollici, default: 7)
#' @param height Altezza minima del grafico (in pollici, default: 4)
#' @return Codice markdown da includere nel report Quarto.
#' @export
crea_sezione_parametro <- function(parametro, report, dati, width = 7, height = 7) {
  data <- report[[parametro]]$date[1] |> as.character()
  prov <- dati[1, provincia]
  lab <- etichette_parametro(parametro)
  
  # estraggo i dati del parametro
  res <- report[[parametro]]
  
  # intestazione della sezione
  cat("\n\n###", lab$nome, "\n\n")
  
  if (is.null(res) || nrow(res) == 0) {
    cat("Non ci sono dati disponibili per il parametro",
        tolower(lab$nome),
        "(", gsub('\"', '', deparse(lab$simbolo)), 
        ") forse le stazioni di misura sono state sabotate da elementi borghesi.\n\n")
    return(invisible(NULL))
  }
  
  # === Tabella dei risultati ===
  suppressWarnings({
    tabella_parametro(parametro, res)
  })
  
  # === Grafico (solo parametri supportati) ===
  if (parametro %in% c("NO2", "SO2", "O3")) {
    plot <- grafico_parametro(dati, parametro)
    
    outdir <- "docs/img"
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    is_latex <- knitr::is_latex_output()
    ext <- if (is_latex) ".pdf" else ".png"
    
    tmpfile <- file.path(outdir, paste0(data, "_", prov, "_", parametro, ext))
    
    # Salva grafico
    ggplot2::ggsave(
      filename = tmpfile,
      plot = plot,
      width = width,
      height = height,
      units = "in",
      dpi = 150
    )
    
    # Inserisci grafico nel report
    if (is_latex) {
      cat("\\begin{center}\\includegraphics[width=0.8\\textwidth]{", tmpfile, "}\\end{center}\n\n")
    } else {
      relpath <- file.path("docs", "img", basename(tmpfile))
      cat("![](", relpath, ")\n\n", sep = "")
    }
  }
  
  
  # Commento sui superamenti
  comm <- commento_superamenti(parametro, res)
  cat("\n\n", comm, "\n\n")
}

#' Genera un commento sui superamenti dei valori limite
#'
#' @param parametro Nome del parametro ("PM10", "PM2.5", "NO2", "SO2", "CO", "O3")
#' @param res Tabella di risultati specifica per il parametro
#'
#' @return Una stringa con il commento in tono sovietico-satirico.
#'
#' @details
#' La funzione controlla i superamenti dei limiti secondo D.Lgs. 155/2010:
#' - PM10: 50 µg/m³ come media giornaliera
#' - PM2.5: nessun limite orario, si commenta il valore medio
#' - NO2: 200 µg/m³ orario, 400 µg/m³ per 3 ore consecutive
#' - SO2: 350 µg/m³ orario
#' - CO: 10 mg/m³ come massima media mobile 8h
#' - O3: 120 µg/m³ (media mobile 8h, target), 180 µg/m³ orario (informazione),
#'       240 µg/m³ per 3 ore consecutive (allarme)
#'
#' Gestisce in automatico singolare e plurale.
commento_superamenti <- function(parametro, res) {
  commenti <- c()
  
  # helper interno per il singolare/plurale
  sp_phrase <- function(n, descr, stazioni) {
    if (n == 0) {
      return(paste0("- Non si registrano superamenti ", descr, 
                    " presso le stazioni di misura considerate."))
    }
    verbo <- ifelse(n == 1, "si registra", "si registrano")
    sost  <- ifelse(n == 1, "superamento", "superamenti")
    dove <-  ifelse(n == 1, "presso la stazione", "presso le stazioni")
    return(paste0("Per il parametro ", parametro, " ", verbo, " ", n, " ", sost, 
                  " del ", descr, " ", dove, " ", 
                  paste(stazioni, collapse = ", "), "."))
  }
  
  if (parametro == "PM10") {
    sup <- res[supera_50 == TRUE]
    n <- nrow(sup)
    commenti <- c(commenti, sp_phrase(n, "limite giornaliero di 50 µg/m³", sup$stazione))
    
  } else if (parametro == "PM2.5") {
    commenti <- c(commenti, "Il PM2.5 non prevede limiti orari giornalieri secondo normativa: "
                  ,"si riporta quindi il valore medio per le stazioni considerate.")
    
  } else if (parametro == "NO2") {
    sup200 <- res[n_supera_200 > 0]
    sup400 <- res[n_supera_400_3h > 0]
    commenti <- c(commenti,
                  sp_phrase(sum(res$n_supera_200), "del limite orario di 200 µg/m³", sup200$stazione),
                  sp_phrase(sum(res$n_supera_400_3h), "del limite di 400 µg/m³ per tre ore consecutive", sup400$stazione))
    
  } else if (parametro == "SO2") {
    sup350 <- res[n_supera_350 > 0]
    commenti <- c(commenti,
                  sp_phrase(sum(res$n_supera_350), "del limite orario di 350 µg/m³", sup350$stazione))
    
  } else if (parametro == "CO") {
    sup <- res[n_supera_10 > 0]
    commenti <- c(commenti,
                  sp_phrase(sum(res$n_supera_10),
                            "del valore limite di 10 mg/m³ sulla media mobile (MA) di 8 ore", sup$stazione))
    
  } else if (parametro == "O3") {
    sup120 <- res[n_supera_120 > 0]
    sup180 <- res[n_supera_180_orario > 0]
    sup240 <- res[n_supera_240_3h > 0]
    commenti <- c(commenti,
                  sp_phrase(sum(res$n_supera_120), "del valore obiettivo di 120 µg/m³ (massima media mobile - MA - sulle 8 ore)", sup120$stazione),
                  sp_phrase(sum(res$n_supera_180_orario), "della soglia di informazione di 180 µg/m³ oraria", sup180$stazione),
                  sp_phrase(sum(res$n_supera_240_3h), "della soglia di allarme di 240 µg/m³ per tre ore consecutive", sup240$stazione))
  }
  
  paste(paste(commenti, collapse = "\n"), "\n")
}

#' Genera tutte le sezioni del report per i parametri presenti
#'
#' @param report lista di data.table restituita da report_giornaliero()
#' @param dati data.table contenente i dati orari
#' @return NULL, stampa direttamente tutte le sezioni
report_giornaliero_sezioni <- function(report, dati) {
  parametri <- names(report)
  for(param in parametri) {
    crea_sezione_parametro(param, report, dati)
  }
}
