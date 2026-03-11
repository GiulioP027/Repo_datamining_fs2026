library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(readr)
library(openmeteo)
library(tidyr)
library(ggplot2)

# load open meteo ----
writeLines('PATH="${RTOOLS45_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

Sys.which("make")

remotes::install_github("mgirlich/tibblify")

remotes::install_github("tpisel/openmeteo")

# coordinates dfcleanin ----

ski_areas <- read_csv("data_raw/ski_areas.csv")

CHski_areas <- ski_areas %>% 
  filter(countries == "Switzerland")
save(CHski_areas, file = "data_raw/CHski_areas.rda")

str(CHski_areas)

df_Chski <- as.data.frame(CHski_areas) %>%
  filter(!is.na(name))
View(df_Chski)
str(df_Chski)

lat <- df_Chski$lat
lng <- df_Chski$lng

coords <- df_Chski %>%
  filter(!is.na(lat), !is.na(lng))

coords_clean <- coords %>%
  select(name, lat, lng)

writeLines(paste(df_Chski$lat, df_Chski$lng, sep=","), "locations.txt")

# "https://archive-api.open-meteo.com/v1/archive?latitude=46.888219,47.064442,47.343393,47.474943,47.332043,46.774952,47.122512,46.678332,46.571709,46.780622,46.252442,46.806238,46.090733,46.309956,46.27056,46.124223,47.124386,46.910923,47.288205,47.385723,46.808615,47.141072,46.432683,46.598146,46.536457,46.237947,46.61713,46.656434,46.839774,46.547275,46.990354,46.597337,46.869478,47.224089,47.091113,47.062496,46.751642,46.479875,45.930357,46.639696,47.32227,46.313608,46.215908,46.770045,46.602601,46.859551,46.884831,46.834611,46.790259,46.556851,46.513429,47.177468,46.795169,46.892794,46.579191,47.493977,46.734555,46.607006,46.901713,46.934218,46.278359,47.146502,46.850634,46.316734,46.874477,47.048793,47.258061,46.596635,46.492579,47.366582,46.087361,46.657111,46.820805,46.989366,47.301652,47.003945,46.679898,46.852105,46.33738,46.757002,46.637911,46.958511,47.35412,47.073064,46.708468,46.439983,46.756357,46.931426,47.261462,46.750777,46.111873,46.468592,47.224718,46.684017,46.602223,47.316708,47.44434,46.563894,46.903449,46.591482,47.205757,47.071233,46.62444,47.035533,46.739237,46.583333,47.421706,47.246431,46.595601,46.253162,46.022863,46.58941,46.971167,46.854179,46.835675,46.697165,47.027049,46.491559,46.91794,46.457491,47.037249,47.251796,47.105945,46.328287,47.12183,46.597793,46.510422,46.870418,46.498365,47.117632,46.598037,47.297782,46.790057,46.637911,46.970647,45.900436,47.285914,46.704372,47.444358,46.627689,46.518845,46.807537,46.640132,47.296756,46.875002,46.735031,47.040428,46.751763,47.256679,46.436208,46.776933,47.148045,46.687367,46.421158,46.80677,46.33184,47.125611,46.542957,46.116803,47.245446,46.441912,47.003826,47.42023,46.560532,46.02609,46.733518,47.413722,46.114997,46.671826,47.314394,46.397902,47.193555,46.40094,46.335879,47.425462,47.228562,46.138497,47.030437,46.117931,46.371762,46.396888,47.121199,46.725333,46.827986,47.332648,47.319985,46.217604,47.344596,47.027059,46.831458,46.009521,46.646247,46.427992,46.46324,46.490457,47.063584,46.750949,47.299194,46.840096,46.593877,47.008743,45.983133,46.791621,46.463125,47.283224,47.139859,46.853619,46.920466,46.966223,47.022841,46.19671,47.011551,47.181891,46.627724,46.202544,46.512456,47.142673,47.309297,46.216602,47.415613,46.531854,46.747483,46.492202,46.731517,46.688972,46.781531,46.438467,46.440639,47.058686,47.33937,47.33966,46.46369,46.520163,47.413285,46.521098,46.351389,47.246475,47.184951,47.35247,46.449787,45.960511,47.294734,46.427597,47.228258,46.226158,46.899101,46.532886,46.879273,46.565879,47.066638,46.393578,47.290307,46.972167,46.395192,46.874605,47.311685,47.298009,46.462934,46.957123,46.463024,46.97824,47.299216,46.522836,46.491292,47.249176,46.911458,47.206658,46.501018,46.050701,47.002515,46.45142,46.477986,46.781258,46.436098,46.803343,46.817884,46.616413,46.473997,46.403939,46.3609,47.347992,46.724125,46.681365,47.091942,47.222413,46.744262,46.400287,46.70333,47.120259,47.12613,46.808467,47.362724,47.162673,46.848447,46.23857,47.307733,46.462337,47.433571,46.535763,46.846037,47.191317,46.417344,46.567607,46.366585,46.674281,46.464288,46.820217,46.804957,46.348399,46.733838,46.444282,47.112291,46.715826,46.501225,46.950741,47.047862,46.632906,46.72941,47.440772,47.39228,46.15642,47.359689,46.661262,46.709186,46.254034,46.872438,46.377927,46.557729,46.410024,47.281971,46.164883,46.865855,46.574542,46.832607,46.646783,47.032093,46.45827,47.26305,47.146715,46.784335,46.729507,46.58243,47.320986,46.250841,46.193251,46.379189,46.741629,47.281992,47.108334,46.507567,47.07873,46.552874,46.530641,46.087872,47.088191,46.90064,46.464306,47.272425,47.046609&longitude=7.77117,8.655981,7.759754,8.429961,8.877998,10.261516,7.016693,7.166617,9.930234,6.475238,7.928561,9.908266,7.524715,8.057819,7.778685,7.268922,8.556048,8.363434,9.413753,8.913295,8.038596,8.737608,9.814586,7.537829,9.863374,7.617368,7.966453,8.062863,7.905416,7.988323,9.629418,7.965674,6.620861,7.382764,6.993601,9.046959,9.338631,8.763442,7.104197,6.319654,9.361775,7.402543,7.468487,7.677703,9.133281,9.887647,8.732719,6.531148,9.0768,7.026206,6.948498,9.30426,9.448927,8.122463,9.570925,9.119931,7.404258,9.947191,8.709588,9.761154,8.051307,7.171367,9.652781,8.467528,7.720566,8.482523,8.736773,7.341811,6.914857,9.457759,7.966026,7.010488,8.105418,9.439448,8.66531,8.2108,7.168022,9.823956,7.225607,7.449153,7.443396,9.632906,9.470115,9.093969,7.821176,8.164891,7.897396,8.484578,7.534585,9.56831,8.843212,7.236627,9.196062,9.569677,7.287253,9.344728,9.297506,7.476504,8.133771,9.597299,8.148881,8.8334,9.764728,7.792259,8.967095,9.583333,7.952778,7.136182,6.242712,7.280858,7.47239,7.027042,6.743759,6.605928,8.410063,9.367634,8.74639,8.483829,8.439814,9.163728,8.784017,7.507181,9.125776,7.130151,8.885121,8.498772,8.612196,6.548083,7.698246,7.078623,6.696676,9.346081,8.124744,7.443396,9.80446,7.196363,8.946504,7.775632,9.480784,7.725082,7.624583,8.025433,10.324352,9.466008,8.008837,7.363947,8.706242,8.22884,9.068646,7.572683,7.825548,9.177859,7.546749,7.76295,9.847589,7.494473,8.84823,6.97313,8.892291,9.245074,7.419103,8.216757,8.985994,8.932112,7.104258,7.279764,9.381337,7.456182,9.487873,9.049614,9.707111,9.105922,7.60946,7.725606,8.099248,7.270253,7.965783,6.748076,6.996463,7.641447,9.706414,8.54009,9.385198,7.678937,8.900525,8.929622,6.953557,9.226604,8.696311,6.529501,8.862373,7.733352,8.662444,8.228257,9.912169,8.469733,7.684288,8.901917,7.678072,9.596572,8.784839,7.152705,8.396699,9.196338,9.158852,8.667304,8.687229,9.146162,9.535482,8.52701,7.862656,8.721446,9.344331,7.205651,7.138183,7.343354,8.732133,9.288189,7.045774,9.548346,9.319197,7.42642,8.268051,7.853696,7.035707,9.524175,6.967465,8.16467,7.95655,8.864145,8.934942,9.644694,8.878441,9.43482,9.898338,7.158056,9.109725,8.232888,9.114363,7.292911,7.210252,9.45108,9.976315,9.156785,7.907945,8.501481,8.357677,8.013647,6.691457,6.850484,9.666089,7.674587,8.6649,7.102111,7.906016,9.210841,9.344724,7.319322,9.036849,9.196898,9.505245,8.468886,6.262985,9.90876,9.034216,9.873671,7.085739,7.552126,7.268312,8.259557,6.104811,6.913259,9.834963,9.553529,9.819152,10.26671,7.577201,7.134176,7.099159,8.183228,7.59343,7.452759,6.355849,8.823149,8.318432,7.392336,8.075779,8.806472,8.728682,8.762407,9.595879,9.198267,9.147163,9.216842,7.527984,8.166099,6.120644,9.53214,6.219593,8.60254,7.017995,7.317285,7.882011,7.024235,9.640928,8.888248,9.510425,7.886035,7.067488,9.130409,6.152703,7.786484,8.041488,8.821507,8.992013,8.696742,6.349359,7.741899,9.503933,9.002515,7.927661,9.042687,7.298233,7.822003,7.82418,8.439245,7.976081,7.484313,8.158525,9.00021,7.570549,9.727475,6.219077,6.455206,8.628673,9.063892,6.933461,7.428783,9.430604,8.281501,7.51173,7.7117,9.40182,8.03605,8.785745,7.263898,9.804042,9.17274,8.673026,9.818494,9.268977,7.332614,8.615343,7.920933,8.681971,8.647365,7.518525,8.026899,6.868018&start_date=2020-01-01&end_date=2026-01-01&daily=snowfall_sum,temperature_2m_mean"

# Api request (la lascio per ricordo, può tornarmi utile in futuro)----

# # get_weather <- function(name, lat, lng){
#   
#   req <- request("https://archive-api.open-meteo.com/v1/archive") |>
#     req_url_query(
#       latitude = lat,
#       longitude = lng,
#       start_date = "2020-01-01",
#       end_date = "2026-01-01",
#       daily = "snowfall_sum,temperature_2m_mean"
#     )
#   
#   resp <- req_perform(req)
#   
#   data <- resp_body_json(resp)
#   
#   df <- as.data.frame(data$daily)
#   
#   df$resort <- name
#   df$lat <- lat
#   df$lon <- lng
#   
#   df
# }
# 
# safe_weather_func <- possibly(
#   .f = \(name, lat, lng) {
#     Sys.sleep(1)
#     safe_get_weather(name, lat, lng)
#   }, 
#   otherwise = NULL  # Se fallisce, restituisce NULL invece di rompersi
# )
# 
# weather_list <- pmap(coords_clean, safe_weather_func)
# 
# safe_weather_func <- safely(safe_get_weather)
# 
# write.csv(weather_data, "swiss_ski_weather.csv", row.names = FALSE)
# 


# test----


df_test <- df_Chski %>% head(5)


get_ski_weather <- function(lat, lng) {
  weather_history(
    location = c(lat, lng),
    start = "2020-01-01",
    end = "2025-12-31",
    daily = c("temperature_2m_mean", "snowfall_sum"),
    response_units = list(temperature_unit = "celsius", precipitation_unit = "mm")
  )
}

# test località: Netschbuehl Eggiwill
weather_data <- get_ski_weather(df_Chski$lat[1], df_Chski$lng[1])

# la media e la neve totale
summary_weather <- weather_data %>%
  summarise(
    temp_media_periodo = mean(daily_temperature_2m_mean, na.rm = TRUE),
    neve_totale_cm = sum(daily_snowfall_sum, na.rm = TRUE) / 10 # convertiamo mm in cm approssimativi
  )

print(summary_weather)

# test automatizzazione

get_safe_weather <- safely(get_ski_weather)

results <- df_Chski %>%
  mutate(weather = map2(lat, lng, ~ {
    Sys.sleep(0.1) # Piccola pausa per non sovraccaricare l'API gratuita
    get_safe_weather(.x, .y)
  }))

# tentativo 1----

fetch_climate <- function(lat, lng) {
  tryCatch({
    # Chiamata API per il periodo 2020-2025
    data <- weather_history(
      location = c(lat, lng),
      start = "2020-01-01",
      end = "2025-12-31",
      daily = c("temperature_2m_mean", "snowfall_sum"),
      response_units = list(temperature_unit = "celsius", precipitation_unit = "mm")
    )
    
    # Restituiamo un piccolo dataframe con i risultati calcolati
    return(data.frame(
      temp_media = mean(data$daily_temperature_2m_mean, na.rm = TRUE),
      neve_totale_cm = sum(data$daily_snowfall_sum, na.rm = TRUE) / 10
    ))
  }, error = function(e) {
    # In caso di errore (es. coordinate errate), restituiamo valori vuoti
    return(data.frame(temp_media = NA, neve_totale_cm = NA))
  })
}

# 2. Eseguiamo il ciclo su tutto il dataset
# NOTA: Per 359 righe ci vorranno circa 3-5 minuti.
print("Inizio download dati... attendere qualche minuto.")

weather_results <- map2_df(df_Chski$lat, df_Chski$lng, function(l1, l2) {
  Sys.sleep(0.1) # Pausa di 0.1 secondi tra le richieste
  fetch_climate(l1, l2)
})

# 3. Uniamo i risultati al dataset originale
df_Chski_meteo <- bind_cols(df_Chski, weather_results)

# Visualizziamo i risultati
View(df_Chski_meteo)

# tentativo 2 ----

# get_weather_raw <- function(lat, lng) {
#   tryCatch({
#     # Chiamata identica al tuo test che ha funzionato
#     weather_history(
#       location = c(lat, lng),
#       start = "2020-01-01",
#       end = "2024-12-31",
#       daily = c("temperature_2m_mean", "snowfall_sum"),
#       response_units = list(temperature_unit = "celsius", precipitation_unit = "mm")
#     ) %>%
#       mutate(year = format(as.Date(date), "%Y")) %>%
#       group_by(year) %>%
#       summarise(
#         temp = mean(daily_temperature_2m_mean, na.rm = TRUE),
#         neve = sum(daily_snowfall_sum, na.rm = TRUE) / 10,
#         .groups = 'drop'
#       )
#   }, error = function(e) { 
#     return(NULL) 
#   })
# }
# 
# # Selezioniamo le prime 10 e scarichiamo
# df_risultato <- df_Chski %>%
#   head(10) %>%
#   mutate(clima = map2(lat, lng, ~ {
#     message(paste("Scaricando dati per:", .y, .x)) # Vedi cosa sta facendo
#     Sys.sleep(0.5)
#     get_weather_raw(.x, .y)
#   })) 
# 
# # Espandiamo i dati (da 1 riga a 5 righe per ogni stazione - una per anno)
# df_long <- df_risultato %>% 
#   tidyr::unnest(clima)
# 
# # Ora trasformiamolo in formato largo (una riga per stazione)
# df_finale <- df_long %>%
#   tidyr::pivot_wider(
#     names_from = year, 
#     values_from = c(temp, neve),
#     names_glue = "{.value}_{year}"
#   )
# 
# View(df_finale)

# tentativo 3----

# 1. Creiamo una tabella separata SOLO per il clima
# Usiamo map invece di map2 per sicurezza, riferendoci alle colonne
print("Inizio download... (circa 5-8 minuti per 359 resort)")

df_clima_results <- df_Chski %>%
  select(id, lat, lng) %>% # Teniamo l'ID per ricollegarli dopo
  mutate(clima_raw = map2(lat, lng, ~ {
    Sys.sleep(0.5) # Pausa più lunga per evitare blocchi dall'API
    get_weather_raw(.x, .y)
  }))

# 2. Trasformiamo i risultati in formato "Largo" (una riga per ID)
df_clima_wide <- df_clima_results %>%
  filter(!map_lgl(clima_raw, is.null)) %>% # Rimuoviamo i NULL temporaneamente per l'unnest
  unnest(clima_raw) %>%
  pivot_wider(
    names_from = year, 
    values_from = c(temp, neve),
    names_glue = "{.value}_{year}"
  )

# 3. UNIONE FINALE: Colleghiamo il clima al dataset originale
# In questo modo torni ad avere 359 righe!
df_completo_finale <- df_Chski %>%
  left_join(df_clima_wide %>% select(-lat, -lng), by = "id")

# Verifica finale
print(paste("Righe totali:", nrow(df_completo_finale)))
print(paste("Resort con dati meteo:", sum(!is.na(df_completo_finale$temp_2024))))
View(df_completo_finale)

save(df_completo_finale, file = "data_preprocessed/CHski_areas_meteo.rda")

#  analisi ----

# trend

df_analisi <- df_completo_finale %>%
  # Teniamo solo quelli che hanno i dati meteo scaricati
  filter(!is.na(temp_2020) & !is.na(neve_2020)) %>%
  mutate(
    # Variazione temperatura (Gradi Celsius)
    delta_temp = temp_2024 - temp_2020,
    
    # Variazione neve (Centimetri)
    delta_neve = neve_2024 - neve_2020,
    
    # Percentuale di perdita/guadagno neve
    perc_neve = (delta_neve / neve_2020) * 100
  )
ggplot(df_analisi, aes(x = min_elevation_m, y = delta_neve)) +
  geom_point(aes(color = delta_neve < 0), size = 3) +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  scale_color_manual(values = c("blue", "red"), labels = c("Aumento", "Calo")) +
  labs(title = "Variazione Neve 2020-2024 vs Altitudine Minima",
       x = "Altitudine alla base (m)",
       y = "Differenza neve (cm)",
       color = "Trend Neve") +
  theme_minimal()


# Resort con più neve persa,

df_analisi %>% arrange(delta_neve) %>% head(5) %>% select(name, delta_neve)

# Media riscaldamento
mean(df_analisi$delta_temp, na.rm = TRUE)

# Peggior anno per la neve (2022, 27.72)

colMeans(df_analisi %>% select(starts_with("neve_")), na.rm = TRUE)

# mappa
install.packages("leaflet")
library(leaflet)

# 1. Definiamo i colori (Rosso per calo neve, Blu per aumento)
pal <- colorNumeric(
  palette = c("darkred", "red", "yellow", "lightblue", "blue"),
  domain = df_analisi$delta_neve
)

# 2. Creiamo la mappa
mappa_sci <- leaflet(df_analisi) %>%
  addTiles() %>%  # Sfondo standard di OpenStreetMap
  addCircleMarkers(
    lng = ~lng, lat = ~lat,
    radius = 6,
    color = ~pal(delta_neve),
    stroke = FALSE, fillOpacity = 0.8,
    # Messaggio che appare cliccando sul punto
    popup = ~paste0(
      "<b>", name, "</b><br>",
      "Altitudine min: ", min_elevation_m, "m<br>",
      "Variazione Neve: ", round(delta_neve, 1), " cm<br>",
      "Variazione Temp: +", round(delta_temp, 2), " °C"
    )
  ) %>%
  addLegend(
    "bottomright", pal = pal, values = ~delta_neve,
    title = "Variazione Neve (cm)",
    labFormat = labelFormat(suffix = " cm"),
    opacity = 1
  )

# Mostra la mappa
mappa_sci
