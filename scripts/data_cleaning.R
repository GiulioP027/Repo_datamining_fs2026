library(httr)
library(jsonlite)
library(tidyverse)
library(readr)

ski_areas <- read_csv("data_raw/ski_areas.csv")

CHski_areas <- ski_areas %>% 
  filter(countries == "Switzerland")

df_per_api <- CHski_areas %>%
  select(lat, lng) %>%
  rename(latitude = lat, longitude = lng) %>%
  na.omit()

# richiesta API ----

body <- list(locations = df_per_api)

response <- POST(
  "https://api.opentopodata.org/v1/eudem25m",
  body = body,
  encode = "json"
)

data <- fromJSON(content(response, "text", encoding = "UTF-8"))

df_per_api$elevation <- data$results$elevation

# Merge dei dati API con il dataset originale ----

CHski_areas <- CHski_areas %>%
  left_join(df_per_api,
            by = c("lat" = "latitude", "lng" = "longitude"))

View(CHski_areas)
