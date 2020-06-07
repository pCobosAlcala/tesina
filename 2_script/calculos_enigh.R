### Configuración inicial ----
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse, readr, doBy, survey, gt)

## Setup
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Mac
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Windows
options(scipen=999) 

## Eliminar objetos
rm(list = ls())


### Base de datos ----

Conc <- read_csv("1_data/datos_abiertos_enigh_2018/conjunto_de_datos_concentradohogar_enigh_2018_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh_2018_ns.csv")


### Código del INEGI ----

# 2.1 Ingreso corriente total promedio trimestral por hogar en deciles de hogares


# opción para tratar los casos de los estratos con una sola una UPM
options(survey.lonely.psu="adjust") 

# selección de las variables de interés (añadí la de transporte público)
Conc <- Conc [ c("folioviv", "foliohog", "ing_cor", "ingtrab", "trabajo", "negocio", "otros_trab", "rentas", "utilidad",
                 "arrenda", "transfer", "jubilacion", "becas", "donativos", "remesas", "bene_gob", "transf_hog", "trans_inst",
                 "estim_alqu", "otros_ing","factor","upm","est_dis", "transporte", "publico")]

# se crea una variable para agregar la entidad federativa
Conc$entidad <- substr(Conc$folioviv,1,2)

## Añadí esto para filtrar la base y quedarme sólo con el Estado de México
Conc <- Conc %>% 
  filter(entidad == 15)

# se define la columna con el nombre de las entidades federativas
Numdec<-c("Total", "I", "II", "III","IV", "V", "VI", "VII", "VIII", "IX","X")

# se crea una bandera para numerar a los hogares
Conc$Nhog <- 1


# deja activa la tabla Conc
attach(Conc)

# ordena Conc de acuerdo a ing_cor, folioviv, foliohog.
Conc<- orderBy (~+ing_cor+folioviv+foliohog, data=Conc)
# suma todos los factores y guarda el valor en el vector tot_hogares.
tot_hogares <- sum(factor,to.data.frame=TRUE)

# se divide la suma de factores entre diez para sacar el tamaño del decil
# se debe de truncar el resultado quitando los decimales.
tam_dec<-trunc(tot_hogares/10)
# muestra la suma del factor en variable hog.
Conc$tam_dec=tam_dec


# se renombra la tabla concentrado a BD1.
BD1 <- Conc
# dentro de la tabla BD1 se crea la variable MAXT y se le asigna los valores que tienen el ing_cor.
BD1$MAXT<-BD1$ing_cor
# se ordena de menor a mayor según la variable MAXT.
BD1<-BD1[with(BD1, order(rank(MAXT))),]
# se aplica la función cumsum, suma acumulada a la variable factor.
BD1$ACUMULA<-cumsum(BD1$factor)
# entra a un ciclo donde iremos generando los deciles 1 a 10.
for(i in 1:9)
{
  a1<-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1,]$factor
  BD1<-rbind(BD1[1:(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),],
             BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1):dim(BD1[1])[1],])
  b1<-tam_dec*i-BD1[dim(BD1[BD1$ACUMULA<tam_dec*i,])[1],]$ACUMULA
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+1),]$factor<-b1
  BD1[(dim(BD1[BD1$ACUMULA<tam_dec*i,])[1]+2),]$factor<-(a1-b1)
}
BD1$ACUMULA2<-cumsum(BD1$factor)
BD1$DECIL<-0
BD1[(BD1$ACUMULA2<=tam_dec),]$DECIL<-1
for(i in 1:9)
{
  BD1[((BD1$ACUMULA2>tam_dec*i)&(BD1$ACUMULA2<=tam_dec*(i+1))),]$DECIL<-(i+1)
}
BD1[BD1$DECIL%in%"0",]$DECIL<-10


x<-tapply(BD1$factor,BD1$Nhog,sum)
# DECILES
y<-tapply(BD1$factor,BD1$DECIL,sum)
# se calcula el promedio (ingreso entre los hogares) tanto para el total como para cada uno de los deciles
ingreso_promedio<-tapply(BD1$factor*BD1$ing_cor,BD1$Nhog,sum)/x
ingreso_deciles<-tapply(BD1$factor*BD1$ing_cor,BD1$DECIL,sum)/y


# Fin del código del INEGI y cálculo del gasto en transporte público
tp_promedio<-tapply(BD1$factor*BD1$publico,BD1$Nhog,sum)/x
tp_deciles<-tapply(BD1$factor*BD1$publico,BD1$DECIL,sum)/y

transporte_promedio<-tapply(BD1$factor*BD1$transporte,BD1$Nhog,sum)/x
transporte_deciles<-tapply(BD1$factor*BD1$transporte,BD1$DECIL,sum)/y


# Conformación de una nueva base
base <- as_tibble(cbind(ingreso_deciles, tp_deciles, transporte_deciles))

base <- base %>%
  mutate(decil = factor(c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"),
                        order = T,
                        levels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X"))) %>% 
  select(decil, ingreso_deciles, tp_deciles, transporte_deciles)



### Visualizaciones ----

# Porcentaje de gasto en transporte
base %>% 
  mutate(publico_entre_ingreso = tp_deciles/ingreso_deciles,
         transporte_entre_ingreso = transporte_deciles/ingreso_deciles,
         transporte_menos_publico = transporte_entre_ingreso - publico_entre_ingreso,
         ingresos_menos_transporte = 1 - transporte_entre_ingreso) %>% 
  select(decil, transporte_menos_publico, publico_entre_ingreso, ingresos_menos_transporte) %>% 
  pivot_longer(-decil, 
               names_to = "variable",
               values_to = "n") %>% 
  ggplot() + 
  geom_col(aes(x = decil,
               y = n,
               fill = factor(variable,
                             order = T,
                             levels = c("ingresos_menos_transporte", 
                                        "transporte_menos_publico",
                                        "publico_entre_ingreso")))) +
  scale_y_continuous(labels = percent,
                     breaks = c(seq(0, 1, .1)),
                     limits = c(0, 1)) +
  scale_fill_manual(values = c("grey", "steelblue", "salmon"),
                    labels = c("Ingreso", "-TP", "TP")) +
  labs(title = "title",
       subtitle = "subtitle",
       x = "Decil",
       y = "Porcentaje",
       fill = "Rubro",
       caption = "Elaboración propia con datos de X (AÑO)") + 
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))
  

# Gasto en transporte y transporte público
base %>% 
  mutate(dif_publico_transporte = transporte_deciles - tp_deciles,
         dif_publico_ingreso = ingreso_deciles - tp_deciles,
         dif_transporte_ingreso = ingreso_deciles - tp_deciles) %>%
  select(decil, dif_publico_transporte, tp_deciles) %>% 
  pivot_longer(-decil, 
               names_to = "variable",
               values_to = "n") %>% 
  ggplot() + 
  geom_col(aes(x = decil, y = n, fill = variable)) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("steelblue", "salmon"),
                    labels = c("-TP", "TP")) +
  labs(title = "title",
       subtitle = "subtitle",
       x = "Decil",
       y = "Porcentaje",
       fill = "Rubro",
       caption = "Elaboración propia con datos de X (AÑO)") + 
  theme_minimal() +
  theme(legend.position = c(.4,.8),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))


# Gasto en transporte público e ingreso
base %>% 
  mutate(dif_publico_transporte = transporte_deciles - tp_deciles,
         dif_publico_ingreso = ingreso_deciles - tp_deciles,
         dif_transporte_ingreso = ingreso_deciles - tp_deciles) %>%
  select(decil, dif_publico_ingreso, tp_deciles) %>% 
  pivot_longer(-decil, 
               names_to = "variable",
               values_to = "n") %>% 
  ggplot() + 
  geom_col(aes(x = decil, y = n, fill = variable)) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("steelblue", "salmon"),
                    labels = c("Gasto", "Gasto en transporte público")) +
  labs(title = "title",
       subtitle = "subtitle",
       x = "Decil",
       y = "Porcentaje",
       fill = "Rubro",
       caption = "Elaboración propia con datos de X (AÑO)") + 
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        legend.position = c(.3,.8),
        panel.grid.major.x = element_blank(),
        plot.caption = element_text(hjust = 0))


# Cálculos finales

q <- base %>% 
  mutate(porcentaje = tp_deciles/ingreso_deciles) %>% 
  select(decil, tp_deciles, porcentaje)

gt_tbl <- gt(data = q)
gt_tbl %>% 
  tab_header(title = "Gasto trimestral en transporte público",
             subtitle = "Hogares por deciles de ingreso en el Estado de México") %>% 
  tab_source_note(source_note = "Elaboración propia con datos de la ENIGH (INEGI 2018).") %>% 
  fmt_currency(columns = "tp_deciles", currency = "USD") %>% 
  fmt_percent(columns = "porcentaje") %>% 
  cols_label(decil = "Decil",
             tp_deciles = "Gasto",
             porcentaje = "% del ingreso")
