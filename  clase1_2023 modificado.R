rm(list=ls())
  
# ~~ Clase 01: Contact Prediction Challenge ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
    
# Librerías 

library(data.table) #para cargar la data
library(ggplot2)
library(dplyr) # para transformar la data
library(tidyr)# para transformar la data
library(corrplot) 
library(scales)

options(scipen=999) # evitar notacion cientifica
set.seed(123) #para que todos tengamos el mismo sampleo

## Funciones para cargar los datos

# Carga un único archivo
load_single_ads_file <- function(ads_file, sample_ratio = 1,
                                 drop_cols = NULL, sel_cols = NULL) {
  
  print(paste0("Loading: ", ads_file, " - Sample ratio: ", sample_ratio))
  
  dt <- fread(ads_file, header = TRUE, quote = "\"",
              stringsAsFactors = TRUE, na.strings = "",
              drop = drop_cols, select = sel_cols,
              showProgress = TRUE, encoding = "UTF-8")
  
  if (sample_ratio < 1) {
    sample_size <- as.integer(sample_ratio * nrow(dt))
    dt <- dt[sample(.N, sample_size)]
  }
  
  return(dt)
}


# Carga múltiples archivos. En este caso, solo carga Enero 2020 pero deberían ver como cargar eficientemente todos los archivos
load_competition_data <- function(comp_dir, sample_ratio = 1, limit_date,
                                  drop_cols = NULL, sel_cols = NULL) {
  
  ads_data_AR_1 <- load_single_ads_file(paste0(DATA_PATH, "ads_data/2020_01_AR.txt"), sample_ratio)
  ads_data_CO_1 <- load_single_ads_file(paste0(DATA_PATH, "ads_data/2020_01_CO.txt"), sample_ratio)
  ads_data_EC_1 <- load_single_ads_file(paste0(DATA_PATH, "ads_data/2020_01_EC.txt"), sample_ratio)
  ads_data_PE_1 <- load_single_ads_file(paste0(DATA_PATH, "ads_data/2020_01_PE.txt"), sample_ratio)
  
  ads_data <- rbind(ads_data_AR_1, ads_data_CO_1, ads_data_EC_1, ads_data_PE_1) #pegamos uno abajo del otro
  ads_data$ad_id <- as.numeric(as.character(ads_data$ad_id)) 
  
  print("Loading and merging train contacts")
  
  contacts <- fread(paste0(DATA_PATH, "train_contacts.csv"), sep=",")
  contacts$ad_id <- as.numeric(as.character(contacts$ad_id))
  
  ads_data <- merge(ads_data, contacts[,c("ad_id", "contacts")], by="ad_id", all.x=TRUE)
  ads_data$contacts <- ifelse(!is.na(ads_data$contacts), ads_data$contacts, 0)# si hay NAs ponemos un 0
  ads_data$contacts <- ifelse(ads_data$created_on < strptime(limit_date,
                                                             format = "%Y-%m-%d",
                                                             tz = "UTC"), #hasta que fecha cargamos
                              ads_data$contacts, NA) 
  return(ads_data)
}
 

# Indicamos el path donde tenemos los archivos
DATA_PATH <- "~/Documents/DITELLA/Mineria de Datos/competition_data/" 
 
# Usamos la función y cargamos todo Enero 2020
ads_data <- load_competition_data(DATA_PATH, sample_ratio = 1,  limit_date = "2022-06-16")
 
# Tomamos una muestra del 20% 
ads_data_sample <- load_competition_data(DATA_PATH, sample_ratio = 0.2, limit_date = "2022-06-16")
 

## Análisis Exploratorio de los Datos (EDA)
# ¿Cuántas observaciones tenemos?
# Todo enero
dim(ads_data) # La ultima columna es la de contactos que mergeamos
# En la muestra
dim(ads_data_sample) 
 

#¿Qué estructura (tipos) tienen los datos?
str(ads_data)
 

# Estadística descriptiva de las variables númericas 
numeric_vars <- select_if(ads_data_sample, is.numeric) 
summary(numeric_vars)
 

### Análisis de la variable de interés
# En primer lugar, analicemos la variable de interes tal cual se nos presenta.
# ¿Están balanceadas las cantidades de contactos?
table(ads_data$contacts)
prop.table(table(ads_data$contacts)) 
# Veámoslo gráficamente
ggplot(ads_data) + geom_histogram(aes(contacts)) # La mayoria de las observaciones son 0
 

# Veamos si la muestra se comporta de manera similar
head(prop.table(table(ads_data_sample$contacts)))
# Veámoslo gráficamente
ggplot(ads_data_sample) + geom_histogram(aes(contacts))
 

# Filtramos los 0s y vemos como se distribuye
ads_data_sin_0_contacts <- ads_data_sample %>% filter(contacts > 0)
head(prop.table(table(ads_data_sin_0_contacts$contacts)), 10)
# Lo vemos gráficamente
ggplot(ads_data_sin_0_contacts) + geom_histogram(aes(contacts))
# Mejoramos el gráfico estéticamente
ggplot(ads_data_sin_0_contacts) + 
  geom_histogram(aes(contacts), bins = 1000) + 
  coord_cartesian(xlim = c(0, 25)) + #hacemos zoom en el grafico
  labs(title = "Histograma de contactos", 
       subtitle = "Sin tener en cuenta los 0s", 
       x = "Número de Contactos", y = "Cantidad") + theme_bw()
 

# Ahora, generemos la variable que vamos a predecir. 
# Una variable binaria, que indica 1 si el anuncio tuvo al menos tres contactos.
ads_data$contacts <- ifelse(ads_data$contacts >= 3, 1, 0)
ads_data_sample$contacts <- as.factor(ifelse(ads_data_sample$contacts >= 3, 1, 0))

# ¿Están balanceadas las clases?
prop.table(table(ads_data$contacts))

# Veamoslos en la muestra
prop.table(table(ads_data_sample$contacts))

### Analicemos otras variables
# Fecha de publicación (created_on)
# Generamos una variable de tipo fecha con la fecha de creación
ads_data_sample$date <- as.Date(ads_data_sample$created_on) #lo pasamos a fecha
 
# Analicemos cómo se comporta en relación a la variable de interés
data_for_plot <- ads_data_sample %>% drop_na(date, contacts) %>% count(date, contacts)

ggplot(data_for_plot) +
  geom_line(aes(x = date, y = n, color = contacts))+
  theme_minimal() +
  gghighlight::gghighlight() +                      
  facet_wrap(. ~ contacts) + 
  theme(strip.text.x = element_blank()) +   
  labs(title = 'Cantidad de publicaciones con y sin al menos 3 contactos según la fecha de publicación', 
       y='Cantidad', x='Fecha de publicación') +
  scale_x_date(date_labels = "%d", breaks = '2 days')
 
# Ratio entre la cantidad de la variable de contactos y la fecha de publicación
data_for_plot <- ads_data_sample %>% drop_na(date, contacts) %>% group_by(date) %>%
  summarise(ratio = mean(as.numeric(as.character(contacts)), na.rm = TRUE))

ggplot(data_for_plot) +
  geom_line(aes(x = date, y = ratio))+
  theme_minimal() +
  theme(strip.text.x = element_blank()) +   
  labs(title = 'Ratio de publicaciones con al menos 3 contactos según la fecha de publicación', 
       y='Cantidad', x='Fecha de publicación') + 
  scale_x_date(date_labels = "%d", breaks = '2 days')
 
# Generamos otra variable que sea el dia de la semana 
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/strptime 
ads_data_sample$weekday <- strftime(ads_data_sample$created_on, "%w")
 
# A partir de la variable anterior, generamos una variable booleana para el fin de semana
ads_data_sample$fin_de_semana <- factor(ifelse(ads_data_sample$weekday %in% c(0, 6),
                                               "Fin de semana", "Día de semana"))
# Veamos la proporción de contactos según el tipo de día de publicación
prop.table(table(ads_data_sample$contacts, ads_data_sample$fin_de_semana), 2) # ponemos 2 para generar una proporcion por dia y no del total

# Veámoslo gráficamente
ggplot(as.data.frame(prop.table(table(ads_data_sample$contacts, ads_data_sample$fin_de_semana), 2)), 
         aes(x = Var2, y = Freq, color = Var1, fill = Var1)) +
  geom_col(alpha = 0.5, position = "dodge") +                      
  facet_grid(. ~ Var2, scales = "free") + 
  labs(title = 'Proporción de observaciones con y sin al menos 3 contactos segun el tipo de día de publicación', 
       y='Proporción', x = "", fill = "Contacto", color = "Contacto") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# Filtremos solo los que Contacto = 1
data_for_plot <- as.data.frame(prop.table(table(ads_data_sample$contacts, ads_data_sample$fin_de_semana), 2)) %>%
  filter(Var1 == 1)

ggplot(data_for_plot,
       aes(x = Var2, y = Freq, color = Var1, fill = Var1)) +
  geom_col(alpha = 0.5, position = "dodge") +                      
  facet_grid(. ~ Var2, scales = "free") + 
  labs(title = 'Proporción de observaciones con y sin al menos 3 contactos segun el tipo de día de publicación', 
       y='Proporción', x = "", fill = "Contacto", color = "Contacto") +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
 

# Precio en USD (price_usd) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary(ads_data_sample$price_usd)
# Veamos cómo se distribuye gráficamente
ggplot(ads_data_sample) + geom_histogram(aes(price_usd)) # La mayoría de las publicaciones tienen valores bajos

# En relación a la variable de contactos 
ggplot(ads_data_sample) + geom_boxplot(aes(contacts, price_usd))
# Eliminamos los outliers
ggplot(ads_data_sample) + geom_boxplot(aes(contacts, price_usd)) +
  scale_y_continuous(limits = quantile(ads_data_sample$price_usd, c(0, 0.9), na.rm = TRUE))

# Veamosla en relación a la fecha de publicación
data_for_plot <- ads_data_sample %>% drop_na(date, contacts) %>% group_by(date, operation) %>%
  summarise(mean_price = mean(price_usd/1000, na.rm = TRUE))

ggplot(data_for_plot %>% filter(operation == "Venta")) + 
  geom_smooth(aes(x = date, y = mean_price), show.legend = FALSE) + 
  scale_x_date(date_labels = "%d", breaks = '2 days') +   
  labs(title = 'Precio promedio en miles de USD de las propiedas en venta en el mes',
       y='Precio en miles de USD', x='Fecha de publicación')
 
# Agreguemmos la variable de contactos
data_for_plot <- ads_data_sample %>% drop_na(date, contacts) %>% group_by(date, operation, contacts) %>%
  summarise(mean_price = mean(price_usd/1000, na.rm = TRUE))

ggplot(data_for_plot %>% filter(operation == "Venta")) + 
  geom_smooth(aes(x = date, y = mean_price, color = contacts)) + 
  scale_x_date(date_labels = "%d", breaks = '2 days') +   
  labs(title = 'Precio promedio en miles de USD de las propiedas en venta en el mes',
       subtitle =  "Diferenciando por contacto",
       y='Precio en miles de USD', x='Fecha de publicación')

# Lo abrimos por tipo de operación
# Tipo de operacion (operation) 
summary(ads_data_sample$operation)
prop.table(table(ads_data_sample$operation))

ggplot(data_for_plot) + 
  geom_smooth(aes(x = date, y = mean_price, color = contacts)) + 
  scale_x_date(date_labels = "%d", breaks = '2 days') +   
  facet_grid(operation~., scales = "free")+
  labs(title = 'Precio promedio en miles de USD de las propiedas en venta en el mes',
       subtitle =  "Diferenciando por contacto",
       y='Precio en miles de USD', x='Fecha de publicación')
# No tenemos en cuenta development
ggplot(data_for_plot %>% filter(operation != "development")) + 
  geom_smooth(aes(x = date, y = mean_price, color = contacts)) + 
  scale_x_date(date_labels = "%d", breaks = '2 days') +   
  facet_grid(operation~., scales = "free")+ #nivel fila o nivel columna`~`
  labs(title = 'Precio promedio en miles de USD de las propiedas en venta en el mes',
       subtitle =  "Diferenciando por contacto",
       y='Precio en miles de USD', x='Fecha de publicación')

# Países (place_l1)
summary(ads_data_sample$place_l1)
prop.table(table(ads_data_sample$place_l1))

# Veamos el porcentaje de contactos segun el motivo de publicación y país
data_for_plot <- ads_data_sample %>% 
  drop_na(contacts, property_type, operation)  %>%
  group_by(place_l1, operation) %>%
  summarize(perc_with_contact = mean(as.numeric(as.character(contacts))))

ggplot(data_for_plot, aes(x=perc_with_contact*100, y=operation, fill = operation)) +
  geom_bar(stat="identity") + facet_grid(place_l1~., scales="fixed") +
  geom_text(aes(label=round(perc_with_contact*100,2)), hjust = -0.2, size = 3) + #te agrega el valor al lado de las barras
  coord_cartesian(xlim = c(0, 20)) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(title = 'Porcentaje de publicaciones con al menos 3 contactos', 
       subtitle = "Diferenciando por motivo de publicación y país",
       y=NULL, x = 'Proporción', fill = "Operación") 

# Analicemos que sucede en cada país con el motivo de publicación según si tuvo al menos 3 contactos o no
ggplot(ads_data_sample[!is.na(contacts) & !is.na(price_usd),], 
       aes(x=as.factor(contacts), y=(price_usd+1)/1000)) +
  geom_boxplot() +
  scale_y_continuous(trans='log10', labels = comma) + # Acotamos el rango de la variable, nos permite apreciar mejor
  facet_grid(operation ~ place_l1, scales="free_y")  +   
  labs(title = 'Cantidad de publicaciones con y sin al menos 3 contactos', 
       subtitle = "Diferencia por motivo de publicación y país",
       y='Log del precio en miles de USD', x='Contacto')

# Tipo de propiedad según el país
table(ads_data_sample$property_type, ads_data_sample$place_l1)
# Veámoslo proporcionalmente
prop.table(table(ads_data_sample$property_type, ads_data_sample$place_l1),2)
# Veámoslo gráficamente
ggplot(ads_data_sample %>% drop_na(property_type)) + 
  geom_bar(aes(x = property_type, fill = place_l1), show.legend = FALSE) + 
  facet_wrap(place_l1 ~ contacts, scales = "free_x", ncol = 4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +   
  labs(title = 'Tipos de propiedades', subtitle = "Diferenciando por país y contacto",
       y='Cantidad', x='Tipo de propiedad') + coord_flip()
# Analicemos de por el tipo de propiedad la proporcion que tuvo al menos 3 contactos
data_for_plot <- ads_data_sample %>% drop_na(property_type) %>% group_by(place_l1, property_type) %>% 
  summarise(percentage = mean(as.numeric(as.character(contacts)), na.rm = TRUE))
ggplot(data_for_plot) + 
  geom_col(aes(x = property_type, y=(percentage*100), fill = place_l1), show.legend = FALSE) + 
  facet_wrap(place_l1 ~ ., scales = "free_x") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +   
  labs(title = 'Proporción de publicaciones con al menos 3 contactos', 
       subtitle = "Diferenciando por tipo de propiedad y país",
       y='Porcentaje', x='Tipo de propiedad') + coord_flip()

# Veamos que sucede con la moneda en la que se publica en cada pais
prop.table(table(ads_data_sample$currency_id, ads_data_sample$place_l1), 2)
# Convertimos la variable para que esten todas en minuscula
ads_data_sample$currency_id <- tolower(ads_data_sample$currency_id)
prop.table(table(ads_data_sample$currency_id, ads_data_sample$place_l1), 2)
 
# Veamos graficamente que incidencia tiene esto en la variable de contactos
data_for_plot <- ads_data_sample %>% filter(!is.na(currency_id)) %>% 
  group_by(currency_id, place_l1, operation) %>% 
  summarise(percentage = mean(as.numeric(as.character(contacts)), na.rm = TRUE))

ggplot(data_for_plot) + 
  geom_col(aes(currency_id, percentage, fill = currency_id), position = "dodge") + 
  facet_grid(operation~place_l1, scales = "free")  +   
  labs(title = 'Porcentaje de publicaciones con al menos 3 contactos', 
       subtitle = "Diferenciando por país y moneda",
       y='Porcentaje', x='Moneda', fill = "Moneda") 
 

# Ambientes (rooms)
summary(ads_data_sample$rooms)
ggplot(ads_data_sample) + geom_histogram(aes(rooms))

# Vemos la cantidad en función del precio
data_for_plot <- ads_data_sample %>% 
  filter(operation == "Alquiler") %>% 
  group_by(rooms, place_l1) %>% 
  summarise(mean_price = mean(price_usd, na.rm = TRUE)) 

ggplot(data_for_plot) + 
  geom_col(aes(x = rooms, y = mean_price), position = "dodge") + 
  facet_grid(. ~ place_l1, scales = "free_y")  +   
  labs(title = 'Precio promedio en USD de las propiedas en alquiler', 
       subtitle = "Según la cantidad de ambientes - Diferenciando por país",
       y='Precio en USD', x='Ambientes')
 
# Incorporemos la variable de interes y el pais
# Filtro hasta 6 ambientes
data_for_plot <- ads_data_sample %>% 
  filter(operation == "Alquiler") %>% filter(rooms <= 6)

ggplot(data_for_plot) + 
  geom_violin(aes(x = contacts, y = (price_usd+1)/1000, fill = contacts), adjust = .5) + 
  facet_grid(place_l1 ~ rooms, scales = "free_y")  +  guides(fill="none")+
  labs(title = 'Precio en USD de las propiedas en alquiler según la cantidad de ambientes', 
       subtitle = "Diferenciando por país y contacto",
       y='Log del precio en miles de USD', x='Ambientes') +
  scale_y_continuous(trans='log10', labels = comma) 

# Cambiemos el pais con el tipo de propiedad
data_for_plot <- ads_data_sample %>% 
  filter(property_type %in% c("Casa", "Departamento", "PH"))  %>% 
  filter(rooms <= 6)
# Apliquemosle una transformación para que se vea mejor
ggplot(data_for_plot) + 
  geom_violin(aes(x = contacts, y = (price_usd+1)/1000, fill = contacts), adjust = .5) + 
  facet_grid(property_type ~ rooms, scales = "free_y")  +  guides(fill="none")+
  labs(title = 'Precio en USD de las propiedas en alquiler según la cantidad de ambientes', 
       subtitle = "Diferenciando por tipo de propiedad y contacto",
       y='Log del precio en miles de USD', x='Ambientes') +
  scale_y_continuous(trans='log10', labels = comma) 
 

# Gráficos de correlación de todas las variables númericas
corrplot(as.matrix(cor(numeric_vars %>% drop_na())), #CORRPLOT hace una matriz de correlaciones
         order ="AOE", method = "color", addCoef.col = 'dark grey')
