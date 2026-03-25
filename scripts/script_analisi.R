# library

library(ggplot2)
library(viridis)
library(dplyr)
library(ggrepel) 
library(leaflet)
library(imager)
library(tidyr)





# load----

 load("data_preprocessed/df_analysis_final.rda")

str(df_analysis_final)
names(df_analysis_final)
head(df_analysis_final)

# analisi grafica----

# mappa----

MapNeve<-load.image("Rplot.png")
plot(MapNeve)

# quota minima resort----

mean_val <- mean(df_analysis_final$min_elevation_m, na.rm = TRUE)

ggplot(df_analysis_final, aes(x = reorder(row.names(df_analysis_final), min_elevation_m), y = min_elevation_m)) +
  # Barre per ogni resort (ordinate per altezza)
  geom_col(fill = "steelblue", alpha = 0.8) +
  
  # LINEA ORIZZONTALE della media (perpendicolare alle barre)
  geom_hline(yintercept = mean_val, color = "red", linetype = "dashed", size = 1) +
  
  # Etichetta della media
  annotate("text", x = 1, y = mean_val + 50, label = paste("Media:", round(mean_val, 0), "m"), 
           color = "red", fontface = "bold", hjust = 0) +
  
  labs(
    title = "Quota minima per singolo resort",
    x = "Resort",
    y = "Quota minima (m)"
  ) +
  theme_minimal() +
  # Nascondiamo i nomi dei resort se sono troppi, altrimenti diventa illeggibile
  theme(axis.text.x = element_blank(), panel.grid.major.x = element_blank())


# relazione quota, neve e temperatura 2024----

ggplot(df_analysis_final, aes(x = min_elevation_m, y = neve_2024, color = temp_2024)) +
   geom_point(size = 3, alpha = 0.7) + 
 geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  # Scala colore "fredda" per le temperature
  scale_color_viridis_c(option = "plasma", name = "Temp 2024 (°C)") +
  labs(
    title = "Relazione Quota, Neve e Temperatura (2024)",
    subtitle = "All'aumentare della quota, come variano neve e gradi?",
    x = "Quota minima (m)",
    y = "Neve totale (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# 15 resort con peggior/miglior condizione neve----


top_bottom <- bind_rows(
  df_analysis_final %>% arrange(delta_neve) %>% slice_head(n = 15), # I 15 peggiori
  df_analysis_final %>% arrange(desc(delta_neve)) %>% slice_head(n = 15) # I 15 migliori
)

ggplot(top_bottom, aes(x = reorder(name, delta_neve), y = delta_neve, fill = delta_neve > 0)) +
  geom_col(color = "white") +

  scale_fill_manual(values = c("TRUE" = "#2d6a4f", "FALSE" = "#c1121f"), 
                    labels = c("Calo Neve", "Aumento Neve"), name = "Trend") +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  labs(
    title = "I 15 Migliori vs I 15 Peggiori Resort",
    subtitle = "Variazione della neve (Delta)",
    x = "",
    y = "Delta Neve (cm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
  

# scatter rischio climatico

ggplot(df_analysis_final, aes(x = min_elevation_m, y = delta_neve)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Quota minima e cambiamento della neve",
    x = "Quota minima (m)",
    y = "Delta neve"
  ) +
  theme_minimal()

# impatto dell'altitudine su neve----

ggplot(df_analisi, aes(x = min_elevation_m, y = delta_neve)) +
 
  geom_hline(yintercept = 0, color = "grey70", size = 1) +
  
  geom_point(aes(color = delta_neve), size = 3, alpha = 0.7) +
  
  scale_color_gradient2(low = "#a1d", mid = "green", high = "#457b4d", 
                        midpoint = 0, name = "Delta Neve (cm)") +
  

  geom_smooth(method = "loess", color = "black", fill = "orange", alpha = 0.2) +
  
  geom_text_repel(data = subset(df_analisi, delta_neve > quantile(delta_neve, 0.95) | 
                                  delta_neve < quantile(delta_neve, 0.05)),
                  aes(label = name), size = 3, fontface = "italic") +
  
  labs(
    title = "Impatto dell'Altitudine sulla Tenuta della Neve",
    subtitle = "Relazione tra quota minima e variazione del manto nevoso (2020-2024)",
    x = "Altitudine minima (m)",
    y = "Differenza Neve (cm)",
    caption = "Nota: I nomi indicano i resort con le variazioni più estreme"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Analisi turismo----


df_turismo_long %>%
  group_by(year) %>%
  summarise(
    Arrivi = sum(ankunfte, na.rm = TRUE),
    Pernottamenti = sum(logiernachte, na.rm = TRUE)
  ) %>%

  pivot_longer(cols = c(Arrivi, Pernottamenti), names_to = "Tipo", values_to = "Valore") %>%

  # plot turismo trend aggregato----
  
ggplot(aes(x = year, y = Valore, color = Tipo, fill = Tipo)) +
 
  geom_area(alpha = 0.1, position = "identity") +

  geom_line(size = 1.5) +
  geom_point(size = 3, stroke = 1.5, fill = "white", shape = 21) +
  
  scale_color_manual(values = c("Arrivi" = "#2a9d8f", "Pernottamenti" = "#e76f51")) +
  scale_fill_manual(values = c("Arrivi" = "#2a9d8f", "Pernottamenti" = "#e76f51")) +
  

  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  
  labs(
    title = "Evoluzione del Turismo nei Resort",
    subtitle = "Confronto tra volumi di Arrivi e Pernottamenti Totali",
    x = "Anno",
    y = "Conteggio Totale",
    color = "Indicatore",
    fill = "Indicatore"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 18),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "grey30")
  )

# Pernottamenti/arrivi ed altitudine----

# df_turismo_long <- df_analysis_final %>%
#   select(name, regions, min_elevation_m, max_elevation_m, vertical_m,
#          temp_2020:temp_2024, neve_2020:neve_2024,
#          ankunfte_2020:ankunfte_2024,
#          logiernachte_2020:logiernachte_2024) %>%
#   pivot_longer(
#     cols = matches("^(temp|neve|ankunfte|logiernachte)_202[0-4]$"),
#     names_to = c("variabile", "year"),
#     names_pattern = "(.*)_(\\d{4})",
#     values_to = "value"
#   ) %>%
#   pivot_wider(names_from = variabile, values_from = value) %>%
#   mutate(year = as.integer(year))


plot_data_2024 <- df_turismo_long %>%
  filter(year == 2024, !is.na(logiernachte), !is.na(min_elevation_m), !is.na(neve)) %>%
  mutate(label_top = ifelse(logiernachte >= quantile(logiernachte, 0.9, na.rm = TRUE), name, NA))

ggplot(plot_data_2024, aes(x = min_elevation_m, y = logiernachte)) +
  geom_point(aes(color = regions, size = neve), alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1) +
  geom_text_repel(aes(label = label_top), size = 3.5, max.overlaps = 15) +
  scale_y_log10() +
  scale_size_continuous(name = "Neve 2024") +
  labs(
    title = "Pernottamenti e altitudine minima dei resort nel 2024",
    subtitle = "La dimensione dei punti rappresenta il livello di neve nel 2024",
    x = "Altitudine minima (m)",
    y = "Pernottamenti 2024 (scala log)",
    color = "Regione"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

plot_arrivi_2024 <- df_turismo_long %>%
  filter(year == 2024, !is.na(ankunfte), !is.na(min_elevation_m), !is.na(neve)) %>%
  mutate(label_top = ifelse(ankunfte >= quantile(ankunfte, 0.9, na.rm = TRUE), name, NA))

ggplot(plot_arrivi_2024, aes(x = min_elevation_m, y = ankunfte)) +
  geom_point(aes(color = regions, size = neve), alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1) +
  geom_text_repel(aes(label = label_top), size = 3.5, max.overlaps = 15) +
  scale_y_log10() +
  scale_size_continuous(name = "Neve 2024") +
  labs(
    title = "Arrivi e altitudine minima dei resort nel 2024",
    subtitle = "La dimensione dei punti rappresenta il livello di neve nel 2024",
    x = "Altitudine minima (m)",
    y = "Ankünfte 2024 (scala log)",
    color = "Regione"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# rapporto neve/turismo----

plot_neve_turismo <- df_turismo_long %>%
  filter(year == 2024, !is.na(logiernachte), !is.na(neve), !is.na(min_elevation_m)) %>%
  mutate(
    quota_cat = case_when(
      min_elevation_m < 1000 ~ "Bassa quota",
      min_elevation_m < 1500 ~ "Media quota",
      TRUE ~ "Alta quota"
    ),
    label_top = ifelse(logiernachte >= quantile(logiernachte, 0.9, na.rm = TRUE), name, NA)
  )

ggplot(plot_neve_turismo, aes(x = neve, y = logiernachte)) +
  geom_point(aes(color = quota_cat, size = min_elevation_m), alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "grey20", linewidth = 1) +
  geom_text_repel(aes(label = label_top), size = 3.5, max.overlaps = 15) +
  scale_y_log10() +
  scale_color_manual(values = c("Bassa quota" = "#d95f02",
                                "Media quota" = "#1b9e77",
                                "Alta quota" = "#7570b3")) +
  labs(
    title = "Neve e pernottamenti nel 2024",
    subtitle = "I colori distinguono i resort per fascia altimetrica",
    x = "Neve 2024",
    y = "Logiernächte 2024 (scala log)",
    color = "Fascia altimetrica",
    size = "Altitudine minima"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# modelli----

df_turismo <- df_analysis_final %>%
  mutate(
    delta_logiernachte = logiernachte_2024 - logiernachte_2020,
    perc_logiernachte = (delta_logiernachte / logiernachte_2020) * 100
  )



# test----

model1 <- lm(delta_logiernachte ~ delta_neve, data = df_turismo)
summary(model1)

# model----

model2 <- lm(delta_logiernachte ~ delta_neve + delta_temp + min_elevation_m + vertical_m,
             data = df_turismo)

summary(model2)
# statistic validity almost none, not so many datas, probably way more important variables ecc...

# model no na----
df_model <- df_turismo %>%
  filter(!is.na(delta_logiernachte),
         !is.na(delta_neve),
         !is.na(delta_temp),
         !is.na(min_elevation_m))

model3 <- lm(delta_logiernachte ~ delta_neve + delta_temp + min_elevation_m,
             data = df_model)

summary(model3)
# doesn't work, other motivation apart from weather, snow ecc


# economic risk----

df_risk <- df_turismo %>%
  mutate(
    snow_risk = -scale(delta_neve)[,1],
    temp_risk = scale(delta_temp)[,1],
    tourism_risk = -scale(delta_logiernachte)[,1],
    elev_risk = -scale(min_elevation_m)[,1],
    
    risk_total = snow_risk + temp_risk + tourism_risk + elev_risk
  ) %>%
  arrange(desc(risk_total))

df_risk %>%
  select(name, regions, min_elevation_m,
         delta_neve, delta_temp, delta_logiernachte, risk_total) %>%
  slice_head(n = 15)

# segmentation/confront----

df_risk <- df_risk %>%
  mutate(
    risk_group = case_when(
      risk_total > quantile(risk_total, 0.75, na.rm = TRUE) ~ "Alto rischio",
      risk_total < quantile(risk_total, 0.25, na.rm = TRUE) ~ "Basso rischio",
      TRUE ~ "Medio"
    )
  )

df_risk %>%
  group_by(risk_group) %>%
  summarise(
    avg_delta_tourism = mean(delta_logiernachte, na.rm = TRUE),
    avg_delta_snow = mean(delta_neve, na.rm = TRUE),
    avg_temp = mean(delta_temp, na.rm = TRUE)
  )


# "panel"----

lm(logiernachte ~ neve + temp + factor(year) + min_elevation_m,
   data = df_turismo_long)
# seems like the tourism biggest factor is the time, like a post pandemin recovery trend or similar.

# incom damage prevision sim----

chf_low  <- 120
chf_mid  <- 180
chf_high <- 250

# economic lost
df_damage <- df_risk %>%
  mutate(
    lost_overnights = ifelse(delta_logiernachte < 0, abs(delta_logiernachte), 0),
    
    damage_chf_low  = lost_overnights * chf_low,
    damage_chf_mid  = lost_overnights * chf_mid,
    damage_chf_high = lost_overnights * chf_high
  )
# ranking 

damage_table <- df_damage %>%
  select(name, regions, lost_overnights, damage_chf_low, damage_chf_mid, damage_chf_high) %>%
  arrange(desc(damage_chf_mid))

head(damage_table, 15)

# summary

damage_summary <- df_damage %>%
  summarise(
    total_lost_overnights = sum(lost_overnights, na.rm = TRUE),
    total_damage_low  = sum(damage_chf_low, na.rm = TRUE),
    total_damage_mid  = sum(damage_chf_mid, na.rm = TRUE),
    total_damage_high = sum(damage_chf_high, na.rm = TRUE)
  )

damage_summary

# model damage

df_reg <- df_damage %>%
  filter(
    !is.na(delta_logiernachte),
    !is.na(delta_neve),
    !is.na(delta_temp),
    !is.na(min_elevation_m),
    !is.na(vertical_m)
  )

mod_damage2 <- lm(damage_chf_mid ~ delta_neve + delta_temp + min_elevation_m + vertical_m,
                  data = df_reg)

summary(mod_damage2)

# this model is confirming than less snow means less incomes, e standart influenced by a huge outlier
