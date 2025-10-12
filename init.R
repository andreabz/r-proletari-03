# init.R
# Script di inizializzazione del progetto

# Controlla se renv è installato
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Ripristina l'ambiente dal lockfile
renv::restore(prompt = FALSE)

message("✅ Ambiente ripristinato con renv. Puoi ora generare il report con Quarto.")