library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(ggplot2)
library(tidygeocoder)

# dati----

df_overnight <- read_delim("data_raw/px-x-1003020000_101_20260319-153528.csv", 
                           delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df_overnight) |> print()

df_hotellerie <- read_delim("data_raw/px-x-1003020000_201_20260319-154720_hotellerie.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(df_hotellerie)

# pulizia dati----







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



print(df_analisi)
# URL API
url <- "https://www.pxweb.bfs.admin.ch/api/v1/en/px-x-1003020000_101/px-x-1003020000_101.px"

