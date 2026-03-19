library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(tidygeocoder)


save(df_analisi, file = "data_preprocessed/df_analysis.rda")

print(df_analisi$localities)



df_Chski <- df_Chski %>%
  reverse_geocode(
    lat = lat,
    long = lng,
    method = "osm",
    address = municipality
  )

df_Chski <- df_Chski %>%
  mutate(
    municipality = sapply(strsplit(municipality, ","), `[`, 1)
  )



# URL API
url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-1003020000_101/px-x-1003020000_101.px"

# JSON query corretto (rimuovendo "YYYY")
body <- '{
  "query": [
    {
      "code": "Jahr",
      "selection": {
        "filter": "item",
        "values": ["2015","2016","2017","2018","2019","2020","2021","2022","2023","2024","2025"]
      }
    },
    {
      "code": "Monat",
      "selection": {
        "filter": "item",
        "values": ["1","2","3","4","5","6","7","8","9","10","11","12"]
      }
    }
  ],
  "response": {
    "format": "json-stat"
  }
}'


