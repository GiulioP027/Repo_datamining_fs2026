library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)

# coordinates----
ski_areas <- read_csv("data_raw/ski_areas.csv")

CHski_areas <- ski_areas %>% 
  filter(countries == "Switzerland")
save(CHski_areas, file = "data_raw/CHski_areas.rda")

str(CHski_areas)

lat <- CHski_areas$lat
lng <- CHski_areas$lng

coords <- CHski_areas[!is.na(CHski_areas$lat) & !is.na(CHski_areas$lng), ]


