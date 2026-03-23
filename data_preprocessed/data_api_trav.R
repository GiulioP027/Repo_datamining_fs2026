library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(tidygeocoder)
library(stringr)
library(janitor)

# dati----
# 
# raw <- fromJSON("data_raw/hotel_df.json")
# 
# ds <- raw$dataset
# 
# years <- names(ds$dimension$Jahr$category$index)
# months <- names(ds$dimension$Monat$category$index)
# gemeinden <- names(ds$dimension$Gemeinde$category$index)
# origin <- names(ds$dimension$Herkunftsland$category$index)
# indikator <- names(ds$dimension$Indikator$category$index)
# 
# df_hotellerie <- expand.grid(
#   Jahr = years,
#   Monat = months,
#   Gemeinde = gemeinden,
#   Herkunftsland = origin,
#   Indikator = indikator,
#   KEEP.OUT.ATTRS = FALSE,
#   stringsAsFactors = FALSE
# )
# 
# 
# df_hotellerie$value <- ds$value
# 
# 
# df_hotellerie <- df_hotellerie %>%
#   mutate(
#     year = as.integer(Jahr),
#     month = Monat,
#     month_label = ds$dimension$Monat$category$label[Monat],
#     municipality_id = as.integer(Gemeinde),
#     municipality = ds$dimension$Gemeinde$category$label[Gemeinde],
#     indicator = ds$dimension$Indikator$category$label[Indikator]
#   )


# tentativo df completo ----


load("data_preprocessed/df_analysis.rda")  

# df_analysis ----

df_analysis <- df_analisi


# 2) EXPLODE LOCALITIES

df_localities <- df_analysis %>%
  select(name, localities) %>%
  mutate(localities = str_split(localities, ";")) %>%
  unnest(localities) %>%
  mutate(localities = str_trim(localities))

# funzione pulizia nomi
clean_name <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\\s*\\([^\\)]+\\)", "") %>%  # toglie (BE), (VS), ecc.
    str_replace_all("[-/]", " ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_squish()
}

df_localities <- df_localities %>%
  mutate(locality_key = clean_name(localities))

# hotel_df----

raw_hotel <- fromJSON("data_raw/hotel_df.json")
ds1 <- raw_hotel$dataset

df_hotel <- expand.grid(
  Jahr = names(ds1$dimension$Jahr$category$index),
  Monat = names(ds1$dimension$Monat$category$index),
  Gemeinde = names(ds1$dimension$Gemeinde$category$index),
  Herkunftsland = names(ds1$dimension$Herkunftsland$category$index),
  Indikator = names(ds1$dimension$Indikator$category$index),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  mutate(
    value = ds1$value,
    year = as.integer(Jahr),
    month = Monat,
    municipality = unname(ds1$dimension$Gemeinde$category$label[Gemeinde]),
    indicator = unname(ds1$dimension$Indikator$category$label[Indikator]),
    municipality_key = clean_name(municipality)
  ) %>%
  select(year, month, municipality, municipality_key, indicator, value)

# tieni solo le località che ti servono
df_hotel_needed <- df_hotel %>%
  semi_join(
    df_localities %>% distinct(locality_key),
    by = c("municipality_key" = "locality_key")
  )

# turism_df----

raw_turism <- fromJSON("data_raw/turism_df.json")
ds2 <- raw_turism$dataset

df_turism <- expand.grid(
  Jahr = names(ds2$dimension$Jahr$category$index),
  Monat = names(ds2$dimension$Monat$category$index),
  Gemeinde = names(ds2$dimension$Gemeinde$category$index),
  Indikator = names(ds2$dimension$Indikator$category$index),
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
) %>%
  mutate(
    value = ds2$value,
    year = as.integer(Jahr),
    month = Monat,
    municipality = unname(ds2$dimension$Gemeinde$category$label[Gemeinde]),
    indicator = unname(ds2$dimension$Indikator$category$label[Indikator]),
    municipality_key = clean_name(municipality)
  ) %>%
  select(year, month, municipality, municipality_key, indicator, value)

# tieni solo le località che ti servono
df_turism_needed <- df_turism %>%
  semi_join(
    df_localities %>% distinct(locality_key),
    by = c("municipality_key" = "locality_key")
  )
