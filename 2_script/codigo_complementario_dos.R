### Configuración inicial ----
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readr, scales, readxl, lubridate, janitor)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Bases y manipulación de base ----

## Importar base 
pasajeros <- read_excel("1_data/pasajeros.xlsx")
pasajeros <- clean_names(pasajeros)

base <- pasajeros %>% 
  select(-total) %>%
  mutate(year = as.character(year)) %>% 
  rename("01" = enero,
         "02" = febrero,
         "03" = marzo,
         "04" = abril,
         "05" = mayo,
         "06" = junio,
         "07" = julio,
         "08" = agosto,
         "09" = septiembre,
         "10" = octubre,
         "11" = noviembre,
         "12" = diciembre) %>% 
  pivot_longer(`01`:`12`,
               names_to = "month",
               values_to = "n") %>% 
  mutate(fecha = make_date(year, month)) %>% 
  select(fecha, n) %>% 
  mutate(n_rez_mensual = lag(n, 1),
         n_rez_anual = lag(n, 12),
         dif_mensual = n - n_rez_mensual,
         dif_anual = n - n_rez_anual,
         cambio_porcentual_mensual = ((n - n_rez_mensual) / n_rez_mensual),
         cambio_porcentual_anual = ((n - n_rez_anual) / n_rez_anual),
         n_1 = lag(n, 1),
         n_2 = lag(n, 2),
         n_3 = lag(n, 3),
         n_4 = lag(n, 4),
         n_5 = lag(n, 5),
         n_6 = lag(n, 6),
         n_7 = lag(n, 7),
         n_8 = lag(n, 8),
         n_9 = lag(n, 9),
         n_10 = lag(n, 10),
         n_11 = lag(n, 11),
         n_prom_tres = (n + n_1 + n_2) / 3 ,
         n_prom_seis = (n + n_1 + n_2 + n_3 + n_4 + n_5 / 6),
         n_prom_doce = (n + n_1 + n_2 + n_3 + n_4 + n_5 + n_6 + n_7 + n_8 + n_9 + n_10 + n_11) / 11)


# Cuadros para señar años
color1 <- "white"
color2 <- "grey40"
rects <- data.frame(xstart = seq(as.Date("2008-01-01"),
                                 as.Date("2020-07-01"),
                                 "1 month"),
                    xend = seq(as.Date("2008-02-01"),
                               as.Date("2020-08-01"),
                               "1 month"))

rects <- rects %>% 
  mutate(year= floor_date(xstart, "1 year")) 

# Pasajeros mensuales 
base %>% 
  filter(fecha >= as.Date("2008-06-01")) %>% 
  ggplot() + 
  geom_rect(aes(xmin = rects$xstart,
                xmax = rects$xend,
                ymin = -Inf,
                ymax = Inf,
                fill = factor(rects$year)),
            alpha = 0.15) +
  scale_fill_manual(values = c(color1, color2,
                               color1, color2,
                               color1, color2,
                               color1, color2,
                               color1, color2,
                               color1, color2,
                               color1)) + 
  geom_col(aes(x = fecha,
            y = n),
           fill = "grey0",
           color = "white") +
  geom_line(aes(x = fecha,
                y = n_prom_doce,
                color = "Promedio móvil de 12 meses"),
            size = 2) + 
  annotate("text",
           x = as.Date("2008-01-01"), 
           y = 4.5e6, 
           label = "Promedio móvil 12 meses",
           color = "salmon",
           size = 3.8,
           hjust = 0,
           fontface = "bold") +
  scale_x_date(breaks = c(seq(as.Date("2008-06-01"),
                              as.Date("2020-06-01"),
                              by = "1 year")),
               labels = date_format("%Y")) + 
  scale_y_continuous(labels = comma,
                     breaks = c(seq(0, 6000000, 1000000)),
                     limits = c(0, 6000000),
                     sec.axis = sec_axis(~ . *1/30,
                                       name = "Promedio mensual de pasajeras por día\n",
                                       breaks = c(seq(0,2e5,2e4)),
                                       labels = comma)) + 
  labs(x = "Año",
       y = "Personas pasajeras por mes\n",
       color = "",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (2019, 2020).") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = .5,
                                   angle = 90),
        plot.caption = element_text(hjust = .5),
        legend.position = "none",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("pasajeras.png",
       width = 6,
       height = 4,
       dpi = 800)

# Pasajeros nuevos con respecto al anterior mes
base %>% 
  filter(fecha >= as.Date("2008-07-01")) %>% 
  ggplot() + 
  geom_col(aes(x = fecha,
                y = dif_mensual)) + 
  scale_x_date(breaks = c(seq(as.Date("2008-01-01"),
                              as.Date("2021-01-01"),
                              by = "2 year")))

# Pasajeros nuevos con respecto al mismo mes del año anterior

rects2 <- rects %>% 
  filter(xstart > as.Date("2008-12-01 "))

base %>% 
  filter(fecha >= as.Date("2009-06-01")) %>% 
  mutate(positivo = ifelse(dif_anual >= 0, 1, 0)) %>% 
  ggplot() + 
  geom_col(aes(x = fecha,
               y = dif_anual,
           fill = factor(positivo)),
           color = "white") + 
  scale_x_date(breaks = c(seq(as.Date("2009-01-01"),
                              as.Date("2020-12-01"),
                              by = "2 year")))+
  scale_x_date(breaks = c(seq(as.Date("2009-01-01"),
                              as.Date("2020-12-01"),
                              by = "1 year")),
               labels = date_format("%Y")) + 
  scale_y_continuous(labels = comma,
                     breaks = c(seq(-16000000, 1600000, 250000))) + 
  scale_fill_manual(values = c("red", "green4")) + 
  labs(x = "Año",
       y = "Variación de pasajeras mensuales",
       color = "",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (2019, 2020).") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 2.1,
                                 angle = 90),
          
        plot.caption = element_text(hjust = 1.2),
        legend.position = "none",
        panel.grid.minor.x = element_blank())

ggsave("variacion_absoluta.png",
       width = 6,
       height = 4,
       dpi = 800)


# Incremento porcentual con respecto al mes anterior
base %>% 
  filter(fecha >= as.Date("2008-07-01")) %>% 
  ggplot() + 
  geom_col(aes(x = fecha,
               y = cambio_porcentual_mensual)) + 
  scale_x_date(breaks = c(seq(as.Date("2008-01-01"),
                              as.Date("2021-01-01"),
                              by = "2 year")))

# Incremento porcentual con respecto al mismo mes del año anterior
base %>% 
  filter(fecha >= as.Date("2009-06-01")) %>% 
  mutate(positivo = ifelse(dif_anual >= 0, 1, 0)) %>% 
  ggplot() + 
  geom_col(aes(x = fecha,
               y = cambio_porcentual_anual,
               fill = factor(positivo)),
           color = "white") + 
  scale_x_date(breaks = c(seq(as.Date("2008-01-01"),
                              as.Date("2021-01-01"),
                              by = "2 year"))) + 
  scale_x_date(breaks = c(seq(as.Date("2009-01-01"),
                              as.Date("2020-12-01"),
                              by = "2 year")))+
  scale_x_date(breaks = c(seq(as.Date("2009-01-01"),
                              as.Date("2020-12-01"),
                              by = "1 year")),
               labels = date_format("%Y")) + 
  scale_y_continuous(labels = percent,
                     breaks = c(seq(-.2, 1.6, .2))) + 
  scale_fill_manual(values = c("red", "green4")) + 
  labs(subtitle = "Comparación con el mismo mes del año anterior",
       x = "Año",
       y = "Variación de pasajeras mensuales",
       color = "",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (AÑO)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 2.45,
                                   angle = 90),
        
        plot.caption = element_text(hjust = 0),
        legend.position = "none",
        panel.grid.minor.x = element_blank())

ggsave("variacion_relativa.png",
       width = 6,
       height = 4,
       dpi = 800)


# Incremento porcentual con respecto al mismo mes del año anterior (filtrada)
base %>% 
  filter(fecha >= as.Date("2011-01-01")) %>% 
  mutate(positivo = ifelse(dif_anual >= 0, 1, 0)) %>% 
  ggplot() + 
  geom_col(aes(x = fecha,
               y = cambio_porcentual_anual,
               fill = factor(positivo)),
           color = "white") + 
  scale_x_date(breaks = c(seq(as.Date("2008-01-01"),
                              as.Date("2021-01-01"),
                              by = "2 year"))) + 
  scale_x_date(breaks = c(seq(as.Date("2009-01-01"),
                              as.Date("2020-12-01"),
                              by = "2 year")))+
  scale_x_date(breaks = c(seq(as.Date("2009-01-01"),
                              as.Date("2020-12-01"),
                              by = "1 year")),
               labels = date_format("%Y")) + 
  scale_y_continuous(labels = percent,
                     breaks = c(seq(-.25, .2, .05))) + 
  scale_fill_manual(values = c("red", "green4")) + 
  labs(subtitle = "Comparación con el mismo mes del año anterior",
       x = "Año",
       y = "Variación de pasajeras mensuales",
       color = "",
       caption = "Elaboración propia con datos de la Agencia Reguladora de Transporte Ferroviario (AÑO)") + 
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 2.45,
                                   angle = 90),
        
        plot.caption = element_text(hjust = 0),
        legend.position = "none",
        panel.grid.minor.x = element_blank())

ggsave("variacion_relativa2.png",
       width = 6,
       height = 4,
       dpi = 800)
