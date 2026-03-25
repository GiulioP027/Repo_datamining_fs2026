# library

library(ggplot2)
library(viridis)
library(dplyr)
library(ggrepel) 
library(leaflet)
library(imager)





# load

 load("data_preprocessed/df_analysis_final.rda")

str(df_analysis_final)
names(df_analysis_final)
head(df_analysis_final)

# analisi----

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
