library(quarto)
library(here)

source(here("R", "utils.R"))
source(here("R", "data_download.R"))
source(here("R", "data_anagrafiche.R"))
source(here("R", "analisi_numerica.R"))
source(here("R", "analisi_grafica.R"))
source(here("R", "report_helper.R"))

province <- c("Bologna", "Ferrara", "Modena", "Parma", "Piacenza",
              "Ravenna", "Reggio Emilia", "Rimini", "Forlì-Cesena")

giorno <- (Sys.Date() - 2) |> format("%d/%m/%Y")

# Cartella di output
out_dir <- here("docs", "province")

# Creo la cartella solo se non esiste
if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
  message("Cartella creata: ", out_dir)
} else {
  message("Cartella già esistente: ", out_dir)
}

# Render dei report provinciali
for (p in province) {
  sigla <- carica_sigla(nome = p)
  
  quarto::quarto_render(
    input = here("province", "template.qmd"),
    output_file = paste0(slugify(p), ".html"),
    execute_dir = out_dir,  # eseguo in questa cartella
    execute_params = list(provincia = sigla,
                          data = giorno)
  )
}