library(dplyr)
library(tidyr)
library(sf)

#columna de modalidad
cat <- grepl("\\d", geo_escuelas_2024_04$Hopitalaria...Inicial) | grepl("\\d", geo_escuelas_2024_04$Hopitalaria...Primario)| grepl("\\d", geo_escuelas_2024_04$Hopitalaria...Secundario)| grepl("\\d", geo_escuelas_2024_04$Talleres...Artística)

geo_esc_24_clean = geo_escuelas_2024_04[!cat, ]

geo_esc_24_columns = geo_esc_24_clean %>% select(-c(Hopitalaria...Inicial, Hopitalaria...Primario, Hopitalaria...Secundario, Talleres...Artística, Servicios.complementarios, Validez_títulos, pre, geometry))

geo_esc_24_columns$Modalidad_Común <- if_else(geo_esc_24_columns$Modalidad_Común == 1, "Comun", "0", missing = NA)
geo_esc_24_columns$Modalidad_Especial <- if_else(geo_esc_24_columns$Modalidad_Especial == 1, "Especial", "0", missing = NA)
geo_esc_24_columns$Modalidad_Adultos <- if_else(geo_esc_24_columns$Modalidad_Adultos == 1, "Adultos", "0", missing = NA)

geo_esc_24_columns <- geo_esc_24_columns %>% mutate_if(is.character, ~replace_na(.,"")) 

geo_esc_24_columns <- geo_esc_24_columns %>% mutate(Modalidad = paste(Modalidad_Común, Modalidad_Especial, Modalidad_Adultos, sep = ";"))

#columna de submodalidad y update

geo_esc_24_columns$Común...Nivel.inicial...Jardín.maternal <- if_else(geo_esc_24_columns$Común...Nivel.inicial...Jardín.maternal == 1, "Jardín Maternal", "0", missing = NA)
geo_esc_24_columns$Común...Nivel.inicial...Jardín.de.infantes <- if_else(geo_esc_24_columns$Común...Nivel.inicial...Jardín.de.infantes == 1, "Jardín de Infantes", "0", missing = NA)
geo_esc_24_columns$Común...Primario <- if_else(geo_esc_24_columns$Común...Primario == 1, "Primario", "0", missing = NA)
geo_esc_24_columns$Común...Secundario <- if_else(geo_esc_24_columns$Común...Secundario == 1, "Secundario", "0", missing = NA)
geo_esc_24_columns$Común...Secundario...INET <- if_else(geo_esc_24_columns$Común...Secundario...INET == 1, "Secundario Técnico", "0", missing = NA)
geo_esc_24_columns$Común...SNU <- if_else(geo_esc_24_columns$Común...SNU == 1, "Superior No Universitario", "0", missing = NA)
geo_esc_24_columns$Común...SNU...INET <- if_else(geo_esc_24_columns$Común...SNU...INET == 1, "Superior No Universitario Técnico", "0", missing = NA)
geo_esc_24_columns$Común...SNU...Cursos <- if_else(geo_esc_24_columns$Común...SNU...Cursos == 1, "Superior No Universitario Cursos", "0", missing = NA)
geo_esc_24_columns$Especial...Nivel.inicial...Educación.temprana <- if_else(geo_esc_24_columns$Especial...Nivel.inicial...Educación.temprana == 1, "Educación Temprana", "0", missing = NA)
geo_esc_24_columns$Especial...Nivel.inicial...Jardín.de.infantes <- if_else(geo_esc_24_columns$Especial...Nivel.inicial...Jardín.de.infantes == 1, "Jardín de Infantes", "0", missing = NA)
geo_esc_24_columns$Especial...Primario <- if_else(geo_esc_24_columns$Especial...Primario == 1, "Primario", "0", missing = NA)
geo_esc_24_columns$Especial...Secundario <- if_else(geo_esc_24_columns$Especial...Secundario == 1, "Secundario", "0", missing = NA)
geo_esc_24_columns$Adultos...Primario <- if_else(geo_esc_24_columns$Adultos...Primario == 1, "Primario", "0", missing = NA)
geo_esc_24_columns$Adultos...Secundario <- if_else(geo_esc_24_columns$Adultos...Secundario == 1, "Secundario", "0", missing = NA)
geo_esc_24_columns$Adultos...Formación.Profesional <- if_else(geo_esc_24_columns$Adultos...Formación.Profesional == 1, "Formacion Profesional", "0", missing = NA)
geo_esc_24_columns$Adultos...Formación.Profesional...INET <- if_else(geo_esc_24_columns$Adultos...Formación.Profesional...INET == 1, "Formacion Profesional Tecnica", "0", missing = NA)
geo_esc_24_columns$Adultos...Alfabetización <- if_else(geo_esc_24_columns$Adultos...Alfabetización == 1, "Alfabetizacion Adultos", "0", missing = NA)

geo_esc_24_columns$update_2024 <- if_else(geo_esc_24_columns$update_2024 == 1, "Si", "No", missing = NA)

geo_esc_24_columns <- geo_esc_24_columns %>% 
  mutate(Submodalidad = paste(Común...Nivel.inicial...Jardín.maternal, Común...Nivel.inicial...Jardín.de.infantes, Común...Primario, Común...Secundario, Común...Secundario...INET, Común...SNU, 
                              Común...SNU...INET, Común...SNU...Cursos, Especial...Nivel.inicial...Educación.temprana, Especial...Nivel.inicial...Jardín.de.infantes, Especial...Primario, 
                              Especial...Secundario, Adultos...Primario, Adultos...Secundario, Adultos...Formación.Profesional, Adultos...Formación.Profesional...INET, Adultos...Alfabetización ,sep = ";"))

geo_esc_24_04 <- geo_esc_24_columns %>% select(Provincia,Sector,Ámbito,Departamento,Localidad,ID_CUE,Nombre,Domicilio,Modalidad,Submodalidad,lon,lat,update_2024)

geo_escuelas_2024 <- geo_esc_24_04 %>% mutate_all(~gsub(";+", ";", .)) 

write.csv(geo_escuelas_2024, "geo_escuelas_2024.csv", fileEncoding = "UTF-8")

