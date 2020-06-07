### Configuración inicial ----
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readr, scales, readxl, lubridate, janitor, viridis)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Bases y manipulación de base ----

## Importar base 
pasajeros <- read_excel("1_data/pasajeros.xlsx", 
                        sheet = "Hoja1")

pasajeros <- clean_names(pasajeros)

glimpse(pasajeros)


### Visualizaciones ----

# Mapa de calor de variación porcentual de pasajeras por estación
pasajeros %>% 
  select(-total) %>% 
  pivot_longer(buenavista:cuautitlan,
               names_to = "estacion",
               values_to = "n") %>% 
  mutate(n_rez = lag(n, 7),
         cambio_porcentual = (n - n_rez) / n_rez,
         cambio_p = ((n - n_rez) / n_rez) * 100) %>% 
  filter(year > 2010) %>% 
ggplot() +
  geom_tile(aes(x = year, y = factor(estacion,
                                     order = T, 
                                     levels = c("cuautitlan",
                                                "tultitlan",
                                                "lecheria",
                                                "san_rafael",
                                                "tlalnepantla",
                                                "fortuna",
                                                "buenavista")),
                fill = cambio_porcentual)) +
  geom_text(aes(x = year, y = factor(estacion,
                                     order = T, 
                                     levels = c("cuautitlan",
                                                "tultitlan",
                                                "lecheria",
                                                "san_rafael",
                                                "tlalnepantla",
                                                "fortuna",
                                                "buenavista")),
                label = round(cambio_p, 1))) +
  scale_fill_viridis(labels = percent) +
  scale_x_continuous(breaks = c(seq(2011,2018,1))) +
  scale_y_discrete(labels = c("Cuautitlán",
                              "Tultitlán",
                              "Lechería",
                              "San Rafael",
                              "Tlalnepantla",
                              "Fortuna",
                              "Buenavista")) + 
  labs(x = "Año",
       y = "Estación\n",
       fill = "Variación\nporcentual",
       caption = "Elaboración propia con datos de la Agencia Regulatoria de Transporte Ferroviario (2019)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))


# Mapa de calor de variación porcentual de pasajeras por estación y total
pasajeros %>% 
  pivot_longer(buenavista:total,
               names_to = "estacion",
               values_to = "n") %>% 
  mutate(n_rez = lag(n, 8),
         cambio_porcentual = (n - n_rez) / n_rez,
         cambio_p = ((n - n_rez) / n_rez) * 100) %>% 
  filter(year > 2010) %>% 
  ggplot() +
  geom_tile(aes(x = year, y = factor(estacion,
                                     order = T, 
                                     levels = c("cuautitlan",
                                                "tultitlan",
                                                "lecheria",
                                                "san_rafael",
                                                "tlalnepantla",
                                                "fortuna",
                                                "buenavista",
                                                "total")),
                fill = cambio_porcentual)) +
  geom_text(aes(x = year, y = factor(estacion,
                                     order = T, 
                                     levels = c("cuautitlan",
                                                "tultitlan",
                                                "lecheria",
                                                "san_rafael",
                                                "tlalnepantla",
                                                "fortuna",
                                                "buenavista",
                                                "total")),
                label = round(cambio_p, 1))) +
  scale_fill_gradient2(labels = percent) +
  scale_x_continuous(breaks = c(seq(2011,2018,1))) +
  scale_y_discrete(labels = c("Cuautitlán",
                              "Tultitlán",
                              "Lechería",
                              "San Rafael",
                              "Tlalnepantla",
                              "Fortuna",
                              "Buenavista",
                              "Todas las\nestaciones")) + 
  labs(title = "Variación porcentual de usuarias por estación",
       subtitle =  "Comparación el año anterior",
       x = "Año",
       y = "Estación\n",
       fill = "Variación\nporcentual",
       caption = "Elaboración propia con datos de la Agencia Regulatoria de Transporte Ferroviario (2019)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = .1))

ggsave("suburbano.png",
       width = 6,
       height = 4,
       dpi = 800)

# Mapa de calor dos de variación porcentual de pasajeras por estación y total
pasajeros %>% 
  pivot_longer(buenavista:total,
               names_to = "estacion",
               values_to = "n") %>% 
  mutate(n_rez = lag(n, 8),
         cambio_porcentual = (n - n_rez) / n_rez,
         cambio_p = ((n - n_rez) / n_rez) * 100) %>% 
  filter(year > 2010) %>% 
  ggplot() +
  geom_tile(aes(x = year, y = factor(estacion,
                                     order = T, 
                                     levels = c("cuautitlan",
                                                "tultitlan",
                                                "lecheria",
                                                "san_rafael",
                                                "tlalnepantla",
                                                "fortuna",
                                                "buenavista",
                                                "total")),
                fill = cambio_porcentual)) +
  scale_fill_gradient2(labels = percent) +
  scale_x_continuous(breaks = c(seq(2011,2018,1))) +
  scale_y_discrete(labels = c("Cuautitlán",
                              "Tultitlán",
                              "Lechería",
                              "San Rafael",
                              "Tlalnepantla",
                              "Fortuna",
                              "Buenavista",
                              "Todas las\nestaciones")) + 
  labs(title = "Variación porcentual de usuarias por estación",
       subtitle =  "Comparación el año anterior",
       x = "Año",
       y = "Estación\n",
       fill = "Variación\nporcentual",
       caption = "Elaboración propia con datos de la Agencia Regulatoria de Transporte Ferroviario (2019)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = .1))

ggsave("suburbanoA.png",
       width = 6,
       height = 4,
       dpi = 800)


# Pasajeros por estación 1

pasajeros %>% 
  select(-total) %>% 
  pivot_longer(buenavista:cuautitlan,
               names_to = "estacion",
               values_to = "n") %>% 
  ggplot() +
  geom_col(aes(x = year,
               y = n,
               fill = fct_reorder(estacion, n)),
           position = "fill") + 
  scale_x_continuous(breaks = c(seq(2009,2018,1))) +
  scale_fill_viridis_d(labels = c("San Rafael",
                                  "Tultitlán",
                                  "Lechería",
                                  "Tlalnepantla",
                                  "Fortuna",
                                  "Cuautitlán",
                                  "Buenavista")) + 
  labs(title = "title",
       subtitle = "subtitle",
       x = "Año",
       y = "Y",
       fill = "Estación",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte (AÑO)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))


# Pasajeros por estación 2

pasajeros %>% 
  select(-total) %>% 
  pivot_longer(buenavista:cuautitlan,
               names_to = "estacion",
               values_to = "n") %>% 
  ggplot() +
  geom_col(aes(x = year,
               y = n,
               fill = fct_reorder(estacion, n)),
           position = "dodge") + 
  scale_x_continuous(breaks = c(seq(2009,2018,1))) +
  scale_y_continuous(labels = comma) + 
  scale_fill_viridis_d(labels = c("San Rafael",
                                  "Tultitlán",
                                  "Tlalnepantla",
                                  "Fortuna",
                                  "Lechería",
                                  "Cuautitlán",
                                  "Buenavista")) +
  labs(title = "Variación porcentual de usuarias por estación",
       subtitle =  "Comparación el año anterior",
       x = "Año",
       y = "Y",
       fill = "Estación",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte (AÑO)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))


# Pasajeros por estación 3

pasajeros %>% 
  select(-total) %>% 
  pivot_longer(buenavista:cuautitlan,
               names_to = "estacion",
               values_to = "n") %>% 
  ggplot() +
  geom_col(aes(x = year,
               y = n,
               fill = fct_reorder(estacion, n))) + 
  scale_x_continuous(breaks = c(seq(2009,2018,1))) +
  scale_y_continuous(labels = comma) +
  scale_fill_viridis_d(labels = c("San Rafael",
                                  "Tultitlán",
                                  "Tlalnepantla",
                                  "Fortuna",
                                  "Lechería",
                                  "Cuautitlán",
                                  "Buenavista")) +
  labs(title = "title",
       subtitle = "subtitle",
       x = "Año",
       y = "Y",
       fill = "Estación",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte (AÑO)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))


# Nuevos pasajeros por año

pasajeros %>% 
  select(-total) %>% 
  pivot_longer(buenavista:cuautitlan,
               names_to = "estacion",
               values_to = "n") %>% 
  mutate(n_rez = lag(n, 7),
         dif = n - n_rez) %>% 
  ggplot() + 
  geom_col(aes(x = year,
               y =  dif,
               group = year,
               fill = estacion)) + 
  scale_fill_discrete(labels = c("Buenavista",
                                  "Cuautitlán",
                                  "Fortuna",
                                  "Lechería",
                                  "San Rafael",
                                  "Tlalnepantla",
                                  "Tultitlán")) +
  scale_x_continuous(breaks = c(seq(2009,2018,1))) +
  scale_y_continuous(labels = comma) +
  labs(title = "title",
       subtitle = "subtitle",
       x = "Año",
       y = "Y",
       fill = "Estación",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte (AÑO)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))


# Nuevos pasajeros por año

pasajeros %>% 
  select(-total) %>% 
  pivot_longer(buenavista:cuautitlan,
               names_to = "estacion",
               values_to = "n") %>% 
  mutate(n_rez = lag(n, 7),
         dif = n - n_rez) %>% 
  filter(year > 2010) %>%
  ggplot() + 
  geom_col(aes(x = year,
               y =  dif,
               fill = estacion),
           color = "white") + 
  scale_fill_discrete(labels = c("Buenavista",
                                  "Cuautitlán",
                                  "Fortuna",
                                  "Lechería",
                                  "San Rafael",
                                  "Tlalnepantla",
                                  "Tultitlán")) +
  scale_x_continuous(breaks = c(seq(2010,2018,1))) +
  scale_y_continuous(labels = comma,
                     breaks = c(seq(0,6e6,1e6))) +

  labs(subtitle = "Comparación con el año anterior",
       x = "Año",
       y = "Incremento anual de pasajeras",
       fill = "Estación",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (2019)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))

ggsave("suburbano3.png",
       width = 6,
       height = 4,
       dpi = 800)



# Cambio porcentual de pasajeros por año
pasajeros %>% 
  select(year, total) %>% 
  mutate(n_rez = lag(total, 1),
         dif = total - n_rez,
         cambio_porcentual = (total - n_rez) / n_rez) %>% 
  filter(year > 2010) %>% 
  ggplot() +
  geom_col(aes(x = year,
               y =  cambio_porcentual),
           fill = "grey0") +
  geom_text(aes(x = year,
               y =  cambio_porcentual,
               label = percent(round(cambio_porcentual,4))),
           vjust = -.8) +
  scale_x_continuous(breaks = c(seq(2010,2018,1))) +
  scale_y_continuous(labels = percent,
                     breaks = c(seq(0,.14,.02)),
                     limits = c(0,.14)) +
  scale_fill_discrete(labels = c("Cuautitlán",
                                 "Tultitlán",
                                 "Lechería",
                                 "San Rafael",
                                 "Tlalnepantla",
                                 "Fortuna",
                                 "Buenavista")) + 
  labs( subtitle = "Comparación con el año anterior",
       x = "Año",
       y = "Variación porcentual",
       fill = "Estación",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (2019)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = 0))

ggsave("suburbano2.png",
       width = 6,
       height = 4,
       dpi = 800)


