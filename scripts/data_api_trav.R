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

# località

df_localities_all <- df_analysis %>%
  select(name, localities) %>%
  distinct() %>%
  mutate(localities = str_split(localities, ";")) %>%
  unnest(localities) %>%
  mutate(
    localities = str_trim(localities),
    locality_key = clean_name(localities)
  ) %>%
  distinct(name, localities, locality_key)

# lista comuni bfs jason

bfs_municipalities <- bind_rows(
  df_hotel %>% distinct(municipality),
  df_turism %>% distinct(municipality)
) %>%
  distinct() %>%
  mutate(municipality_key = clean_name(municipality)) %>%
  arrange(municipality)

# mappatura

manual_lookup <- tibble::tribble(
  ~localities,            ~municipality_bfs,
  "Verbier",              "Val de Bagnes",
  "Le Châble",            "Val de Bagnes",
  "Bruson",               "Val de Bagnes",
  "Champsec",             "Val de Bagnes",
  "Sarreyer",             "Val de Bagnes",
  "Lourtier",             "Val de Bagnes",
  
  "Haute-Nendaz",         "Nendaz",
  "Siviez",               "Nendaz",
  "Clèbes",               "Nendaz",
  
  "Les Mayens-de-Sion",   "Sion",
  
  "Davos Dorf",           "Davos",
  "Davos Platz",          "Davos",
  "Davos Wolfgang",       "Davos",
  "Clavadel",             "Davos",
  "Frauenkirch",          "Davos",
  "Glaris",               "Davos",
  
  "Serneus Dorf",         "Klosters",
  "Saas im Prättigau",    "Klosters",
  
  "Champex",              "Orsières",
  "Les Diablerets",       "Ormont-Dessus",
  "Gsteig bei Gstaad",    "Saanen",
  
  "Albinen",              "Leukerbad",
  
  "Rothwald",             "Goms",
  "Binn",                 "Goms",
  
  "Silvaplana-Surlej",    "Silvaplana",
  "Plaun da Lej",         "Silvaplana",
  
  "Tarasp",               "Scuol",
  "Sent",                 "Scuol",
  "Ftan",                 "Scuol",
  
  "Rosswald",             "Ried-Brig"
)

# tab mappata

mapping_complete <- df_localities_all %>%
  left_join(manual_lookup, by = "localities") %>%
  mutate(
    manual_key = if_else(!is.na(municipality_bfs), clean_name(municipality_bfs), NA_character_)
  ) %>%
  left_join(
    bfs_municipalities %>%
      transmute(
        municipality_exact = as.character(municipality),
        locality_key = as.character(municipality_key)
      ),
    by = "locality_key"
  ) %>%
  left_join(
    bfs_municipalities %>%
      transmute(
        municipality_manual_valid = as.character(municipality),
        manual_key = as.character(municipality_key)
      ),
    by = "manual_key"
  ) %>%
  mutate(
    municipality_exact = as.character(municipality_exact),
    municipality_manual_valid = as.character(municipality_manual_valid),
    municipality_bfs_final = coalesce(municipality_manual_valid, municipality_exact),
    municipality_bfs_final = as.character(municipality_bfs_final),
    match_type = case_when(
      !is.na(municipality_manual_valid) ~ "manual",
      !is.na(municipality_exact) ~ "exact",
      TRUE ~ "unmatched"
    )
  ) %>%
  select(
    name,
    localities,
    locality_key,
    municipality_bfs_final,
    match_type
  ) %>%
  arrange(match_type, name, localities)
write_csv(mapping_complete, "data_preprocessed/mapping_localities_bfs.csv")

# mapping_complete %>%
#   count(match_type)
# mapping_complete %>%
#   filter(match_type == "unmatched") %>%
#   select(name, localities) %>%
#   distinct() %>%
#   print(n = 300)


df_localities_clean <- mapping_complete %>%
  filter(!is.na(municipality_bfs_final)) %>%
  mutate(locality_key = clean_name(municipality_bfs_final)) %>%
  distinct(name, localities, locality_key)

df_localities_clean <- df_localities_clean %>%
  distinct(name, locality_key)


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


df_turism_needed <- df_turism %>%
  semi_join(
    df_localities %>% distinct(locality_key),
    by = c("municipality_key" = "locality_key")
  )

# annuali

hotel_yearly <- df_hotel_needed %>%
  filter(month == "YYYY") %>%
  select(year, municipality_key, indicator, value) %>%
  group_by(year, municipality_key, indicator) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = indicator,
    values_from = value
  ) %>%
  clean_names()

turism_yearly <- df_turism_needed %>%
  filter(month == "YYYY") %>%
  select(year, municipality_key, indicator, value) %>%
  group_by(year, municipality_key, indicator) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = indicator,
    values_from = value
  ) %>%
  clean_names()

# municipality + resort

hotel_by_resort_year <- df_localities_clean %>%
  left_join(
    hotel_yearly,
    by = c("locality_key" = "municipality_key"),
    relationship = "many-to-many"
  ) %>%
  group_by(name, year) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

hotel_by_resort_year <- hotel_by_resort_year %>%
  filter(!is.na(year))

turism_by_resort_year <- df_localities_clean %>%
  left_join(
    turism_yearly,
    by = c("locality_key" = "municipality_key"),
    relationship = "many-to-many"
  ) %>%
  filter(!is.na(year)) %>%
  group_by(name, year) %>%
  summarise(
    establishments = sum(establishments, na.rm = TRUE),
    rooms = sum(rooms, na.rm = TRUE),
    beds = sum(beds, na.rm = TRUE),
    arrivals = sum(arrivals, na.rm = TRUE),
    overnight_stays = sum(overnight_stays, na.rm = TRUE),
    room_nights = sum(room_nights, na.rm = TRUE),
    room_occupancy = mean(room_occupancy, na.rm = TRUE),
    bed_occupancy = mean(bed_occupancy, na.rm = TRUE),
    .groups = "drop"
  )

turism_by_resort_year <- turism_by_resort_year %>%
  filter(!is.na(year))

hotel_cols <- setdiff(names(hotel_by_resort_year), c("name", "year"))
turism_cols <- setdiff(names(turism_by_resort_year), c("name", "year"))

hotel_by_resort_wide <- hotel_by_resort_year %>%
  pivot_wider(
    names_from = year,
    values_from = all_of(hotel_cols),
    names_glue = "{.value}_{year}"
  )

turism_by_resort_wide <- turism_by_resort_year %>%
  pivot_wider(
    names_from = year,
    values_from = c(
      establishments,
      rooms,
      beds,
      arrivals,
      overnight_stays,
      room_nights,
      room_occupancy,
      bed_occupancy
    ),
    names_glue = "{.value}_{year}"
  )

# merge

df_analysis_final <- df_analysis %>%
  left_join(hotel_by_resort_wide, by = "name") %>%
  left_join(turism_by_resort_wide, by = "name")

View(df_analysis_final)



