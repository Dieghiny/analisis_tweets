# Librerías
library(rtweet) # Versión 0.7.0
library(tidyverse)
library(svDialogs)
library(lubridate)
library(janitor)
library(tokenizers)

# Parámetros de búsqueda
busqueda <- dlgInput(message = "Búsqueda:")$res
fecha_inicio <- dlgInput(message = "Fecha de inicio:")$res
fecha_final <- dlgInput(message = "Fecha final:")$res
numero_tweets <- dlgInput(message = "Número de tweets solicitados:")$res %>% 
  as.numeric()

# Creación data frame
tweets_df <- search_tweets(q = busqueda,
                           n = numero_tweets,
                           lang ="es",
                           include_rts = FALSE,
                           type = "recent",
                           retryonratelimit = TRUE)

# Ejecutamos script que aloja la función tz_cdmx()
source(paste0(getwd(), "/funciones/tz_cdmx.R"))

tweets_df <- tz_cdmx(tweets_df)

palabras_vacias <- tokenize_words(busqueda) %>% 
  purrr::as_vector() %>% 
  unique()