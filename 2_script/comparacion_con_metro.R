### Configuración inicial ----
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readr, scales, readxl, lubridate, janitor)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Bases de datos ----

#metro <- read_csv("https://datos.cdmx.gob.mx/explore/dataset/afluencia-diaria-del-metro-cdmx/download/?format=csv&timezone=America/Mexico_City&lang=es&use_labels_for_header=true&csv_separator=%2C")

metro <- read_csv("1_data/metro.csv")

suburbano <- read_excel("1_data/pasajeros.xlsx", 
                        sheet = "Hoja1")


### Transformaciones y visualización ----

## Comparación entre líneas

#Transformación
 metro_lineas <- metro %>% 
   mutate(Linea = case_when(Linea == "Linea 1" ~ "Línea 1",
                              Linea == "Linea 2" ~ "Línea 2",
                              Linea == "Linea 3" ~ "Línea 3",
                              Linea == "Linea 4" ~ "Línea 4",
                              Linea == "Linea 5" ~ "Línea 5",
                              Linea == "Linea 6" ~ "Línea 6",
                              Linea == "Linea 7" ~ "Línea 7",
                              Linea == "Linea 8" ~ "Línea 8",
                              Linea == "Linea 9" ~ "Línea 9",
                              Linea == "Linea A" ~ "Línea A",
                              Linea == "Linea B" ~ "Línea B",
                              Linea == "Linea 12" ~ "Línea 12")) %>%
          
   group_by(Linea, Año) %>% 
   summarise(sum = sum(Afluencia),
             diario = sum/365) %>% 
    ungroup() %>% 
    mutate(servicio = "Metro")
 
suburbano_lineas <- suburbano %>% 
   select(year, total) %>% 
   mutate(Linea = "Suburbano",
          diario = total/365,
          servicio = "Suburbano") %>% 
   rename(Año = year,
          sum = total) %>% 
   select(Linea, Año, sum, diario, servicio)


base <- rbind(metro_lineas, suburbano_lineas)

# Visualización
base %>% 
   filter(Año == 2018) %>% 
      ggplot(aes(x = fct_reorder(Linea, diario), y = diario, fill = servicio)) + 
   geom_col() + 
   scale_y_continuous(labels = comma) +
   coord_flip() +
   scale_fill_manual(values = c("steelblue", "red")) +
   labs(x = "",
        y = "Promedio de pasajeras diarias",
        caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (2019) \ny del Gobierno de la Ciudad de México (2020)") + 
   theme_minimal() +
   theme(legend.position = "none",
         plot.caption = element_text(hjust = 0))

ggsave("comparacion1.png",
       width = 6,
       height = 4,
       dpi = 800)


## Comaración de estaciones

# Transformaciones
metro_estaciones <- metro %>% 
   mutate(Linea = case_when(Linea == "Linea 1" ~ "Línea 1",
                            Linea == "Linea 2" ~ "Línea 2",
                            Linea == "Linea 3" ~ "Línea 3",
                            Linea == "Linea 4" ~ "Línea 4",
                            Linea == "Linea 5" ~ "Línea 5",
                            Linea == "Linea 6" ~ "Línea 6",
                            Linea == "Linea 7" ~ "Línea 7",
                            Linea == "Linea 8" ~ "Línea 8",
                            Linea == "Linea 9" ~ "Línea 9",
                            Linea == "Linea A" ~ "Línea A",
                            Linea == "Linea B" ~ "Línea B",
                            Linea == "Linea 12" ~ "Línea 12")) %>%
   
   group_by(Estacion, Año) %>% 
   summarise(sum = sum(Afluencia),
             diario = sum/365) %>% 
   ungroup() %>% 
   mutate(servicio = "Metro")


suburbano_estaciones <- suburbano %>% 
   select(-total) %>% 
   pivot_longer(buenavista:cuautitlan,
                names_to = "Estacion",
                values_to = "sum") %>% 
   mutate(diario = sum/365,
          Linea = "Suburbano",
          servicio = "Suburbano") %>% 
   rename(Año = year) %>% 
   select(Estacion, Año, sum, diario, servicio)

base <- rbind(metro_estaciones, suburbano_estaciones)


# Visualización 1 (todas las estaciones)
base %>% 
   filter(Año == 2018) %>% 
   ggplot(aes(x = fct_reorder(Estacion, -diario), y = diario, color = servicio, fill = servicio)) + 
   geom_col() + 
   scale_y_continuous(labels = comma,
                      breaks = c(seq(0, 350000, 50000))) +
   scale_color_manual(values = c("steelblue", "red")) +
   scale_fill_manual(values = c("steelblue", "red")) +
   labs(title = "title",
        subtitle = "subtitle",
        x = "Estaciones",
        y = "Pasajeras diarias",
        fill = "Servicio",
        color = "Servicio",
        caption = "Elaboración propia con datos de X (AÑO)") + 
   theme_minimal() +
   theme(axis.text.x = element_blank(),
         legend.position = c(.8,.8),
         panel.grid.major.x = element_blank(),
         plot.caption = element_text(hjust = 0))


# Visualización 2 (sin Pantitlán)
base %>% 
   filter(Año == 2018,
          Estacion != "Pantitlán") %>% 
   ggplot(aes(x = fct_reorder(Estacion, -diario), y = diario, color = servicio, fill = servicio)) + 
   geom_col() + 
   scale_y_continuous(labels = comma,
                      breaks = c(seq(0, 100000, 20000))) +
   scale_color_manual(values = c("white", "grey0")) +
   scale_fill_manual(values = c("steelblue", "red")) +
   labs(
        x = "Estaciones ordenadas (sin Pantitlán)",
        y = "Promedio de pasajeras diarias",
        fill = "Servicio",
        color = "Servicio",
        caption = "EElaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (2019) \ny del Gobierno de la Ciudad de México (2020)") + 
   theme_minimal() +
   theme(axis.text.x = element_blank(),
         legend.position = c(.8,.8),
         panel.grid.major.x = element_blank(),
         plot.caption = element_text(hjust = 0))


ggsave("comparacion2.png",
       width = 6,
       height = 4,
       dpi = 800)



metro %>% 
   mutate(Linea = case_when(Linea == "Linea 1" ~ "Línea 1",
                            Linea == "Linea 2" ~ "Línea 2",
                            Linea == "Linea 3" ~ "Línea 3",
                            Linea == "Linea 4" ~ "Línea 4",
                            Linea == "Linea 5" ~ "Línea 5",
                            Linea == "Linea 6" ~ "Línea 6",
                            Linea == "Linea 7" ~ "Línea 7",
                            Linea == "Linea 8" ~ "Línea 8",
                            Linea == "Linea 9" ~ "Línea 9",
                            Linea == "Linea A" ~ "Línea A",
                            Linea == "Linea B" ~ "Línea B",
                            Linea == "Linea 12" ~ "Línea 12")) %>%
   filter(Estacion == c("Pantitlán"),
          Año >= 2015,
          Año < 2020) %>% 
   group_by(Estacion, Año) %>% 
   summarise(Af = sum(Afluencia)) %>% 
   arrange(Af)  
   ggplot(aes(x = Af, y = Estacion)) + 
   geom_boxplot() + 
   facet_wrap(~ Año) +
   coord_flip()

base %>% 
   filter(Año == 2019) %>% 
   arrange(-diario) 
