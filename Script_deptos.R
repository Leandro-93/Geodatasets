install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")

library(sf)
library(ggplot2)
library(dplyr)
library(ggplot2)


setwd("C:/Users/20376993036/Desktop/Informe Deptos Alquiler 2 Semestre 2023")

Deptos_alq_2trim <- library(readxl)
Deptos_alq_2trim <- read_excel("230828_Deptos_alq_2dotrim_23.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "numeric", "numeric", "text", 
                                             "numeric", "numeric", "text", "text", 
                                             "text", "text", "numeric", "text", 
                                             "text"))

Deptos_alq_2trim_coords <- Deptos_alq_2trim %>% filter(!is.na(Latitud) & !is.na(Longitud))
                               
#resumen 

summary(Deptos_alq_2trim_coords)

#fuera de caba 
Barrios_BADATA <- st_read("C:\\Users\\20376993036\\Desktop\\Informe Deptos Alquiler 2 Semestre 2023\\190423_Barrios_BA Data\\190423_Barrios_badata.shp")

Deptos_alq_geo <- st_as_sf(Deptos_alq_2trim_coords, coords = c("Longitud", "Latitud"),
                         crs= 4326)

Deptos_alq_geo_9001 <- Deptos_alq_geo %>% st_transform(st_crs(Barrios_BADATA))

Deptos_alq_CABA <- st_intersection(Deptos_alq_geo_9001, Barrios_BADATA)

#ggmap
base_plot <- ggplot() +
  theme_void()

base_plot +
  geom_sf(data = Barrios_BADATA, color = "blue") +
  geom_sf(data = Deptos_alq_CABA, fill = "black", alpha = 0.3)

#conversion a ARS
valor_multiplicacion <- 236
condicion <- Deptos_alq_CABA$Nombre == "Dolares"

Deptos_alq_CABA <- mutate(Deptos_alq_CABA, ConversionARS = 
ifelse(condicion, MontoOperacion * valor_multiplicacion, MontoOperacion))

# Monto Minimo 60000

Deptos_alq_CABA <- Deptos_alq_CABA %>%
  filter(ConversionARS > 60000)

media_depto_alq_barrio_CABA <- Deptos_alq_CABA %>%
  group_by(BARRIO) %>%
  summarise(mean_value = mean(MontoOperacion),
            total_occurrences = n())

media_barrios_CABA <- st_join(Barrios_BADATA, media_depto_alq_barrio_CABA, join = st_intersects)

ggplot() +
  geom_sf(data = Barrios_BADATA, fill = "white", color = "black") +  # Base map
  geom_sf(data = media_barrios_CABA, aes(fill = media_barrios_CABA$mean_value)) +         # Median values
  scale_fill_viridis_c() +                                          # Color scale
  labs(fill = "Median Value") +                                     # Legend label
  theme_minimal()     

Deptos_alq_CABA_excel <- Deptos_alq_CABA %>% select(-Barrio)

st_write(media_barrios_CABA, "media_alquiler_2trim_deptos_CABA_barrios.shp")

st_write(Deptos_alq_CABA_excel, "Deptos_alq_CABA_excel.csv")


