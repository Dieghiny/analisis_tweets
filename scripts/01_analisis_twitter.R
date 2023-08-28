# Librerías----
library(rtweet) # Versión 0.7.0
library(tidyverse)
library(svDialogs)
library(lubridate)
library(janitor)
library(tokenizers)

# Parámetros de búsqueda----
busqueda <- dlgInput(message = "Búsqueda:")$res
fecha_inicio <- dlgInput(message = "Fecha de inicio:")$res
fecha_final <- dlgInput(message = "Fecha final:")$res
numero_tweets <- dlgInput(message = "Número de tweets solicitados:")$res %>% 
  as.numeric()

# Creación data frame----
tweets_df <- search_tweets(q = busqueda,
                           n = numero_tweets,
                           lang ="es",
                           include_rts = FALSE,
                           type = "recent",
                           retryonratelimit = TRUE)

tweets_df <- tweets_df %>% 
  mutate(created_at = with_tz(created_at,
                              tzone = "America/Mexico_City")) %>%
  mutate(account_created_at = with_tz(account_created_at, 
                                      tzone = "America/Mexico_City")) %>%
  dplyr::filter(created_at >= fecha_inicio) %>%
  dplyr::filter(created_at <= fecha_final) %>% 
  arrange(desc(created_at)) %>% 
  clean_names()

palabras_vacias <- tokenize_words(busqueda) %>% 
  purrr::as_vector() %>% 
  unique()