# R per proletari - Episodio 03

**R per proletari** è una serie di post su LinkedIn dedicata a chi vuole
liberarsi dal giogo del lavoro manuale e automatizzare la produzione di report
ripetitivi con la potenza collettiva di **R** e **Quarto**.

In questo *episodio 03*, automatizziamo la creazione di di bollettini provinciali
sulla qualità dell'aria per le province dell'Emilia-Romagna, utilizzando i dati dei compagni di 
**ARPA Emilia-Romagna** sui principali inquinanti atmosferici.

## Obiettivi

- Scaricare i dati dalle [API di ARPA Emilia-Romagna](https://dati.arpae.it/datastore/dump/4dc855a1-6298-4b71-a1ae-d80693d43dcb).  
- Analizzare i dati secondo i criteri del  
  [Decreto Legislativo 155/2010](https://www.normattiva.it/uri-res/N2Ls?urn:nir:stato:decreto.legislativo:2010-08-13;155).  
- Generare automaticamente i report Quarto analoghi a un bollettino ufficiale.
- Aggiornare i bollettini ogni notte con i dati di due giorni precedenti e pubblicarli su GitHub Pages:  
  👉 [andreabz.github.io/r-proletari-03](https://andreabz.github.io/r-proletari-03/).

## Struttura del progetto

- `R/` – funzioni per scaricare, pulire e analizzare i dati.  
- `data/` – archivio dei dati grezzi scaricati dalle API.  
- `index.qmd` – documento Quarto principale per la selezione della provincia.
- `province/` – template dei report provinciali.
- `init.R` – script per inizializzare l'ambiente con **renv**.  
- `renv/` + `renv.lock` – ambiente R riproducibile e condivisibile.

## Requisiti

- **R >= 4.2**
- **Quarto** installato (https://quarto.org)
- Tutti i pacchetti R sono gestiti tramite **renv**.

## Setup

1. Clonare il repository:

   ```bash
   git clone https://github.com/andreabz/r-proletari-03.git
   cd r-proletari-03
   ```

2. Inizializzare l'ambiente R

   ```r
   source("init.R")
   ```
   
## Criteri di valutazione dei dati secondo il Decreto Legislativo 155/2010

Il report confronta i dati osservati con i valori limite di legge, ad esempio:

- PM10 → media giornaliera ≤ 50 µg/m³ (max 35 superamenti/anno).
- NO₂ → max oraria ≤ 200 µg/m³ (max 18 superamenti/anno).
- O₃ → max media mobile 8h ≤ 120 µg/m³ (max 25 superamenti/anno).
- CO → max media mobile 8h ≤ 10 mg/m³.
- SO₂ → max giornaliera ≤ 125 µg/m³ (max 3 superamenti/anno).
- SO₂ → max oraria ≤ 350 µg/m³ (max 24 superamenti/anno).

## Output

Il report finale include:

- Documenti [html](https://andreabz.github.io/r-proletari-03/).
- Tabelle comparative tra osservazioni e limiti normativi.
- Grafici temporali per ogni inquinante e stazione.

## Prossimi sviluppi

- Creazione di una web-app che permetta di generare on-demand un report 
  per una provincia e un giorno specifico.
