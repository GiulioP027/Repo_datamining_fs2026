library(httr)
library(jsonlite)
library(tidyverse)

CHski_areas <- ski_areas %>% 
  filter(ski_areas$countries == "Switzerland")



# 1. Seleziona solo le colonne necessarie e rinominale se serve
# Supponiamo che il tuo df si chiami 'mio_df'
df_per_api <- CHski_areas[, c("lat", "lng")]
colnames(df_per_api) <- c("latitude", "longitude")

# 2. Converti in formato JSON nidificato sotto la chiave "results"
json_output <- list(results = df_per_api)
json_finale <- toJSON(json_output, dataframe = "rows", pretty = TRUE)

# Visualizza il risultato
cat(json_finale)


