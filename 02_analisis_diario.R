
#Librerías----

library(readr) #Facilita la lectura de archivos .csv
library(stopwords) #Diccionario de palabras comunes para eliminacion 
library(wordcloud2) #Creacion de las nubes de palabras
library(ggplot2) #Creacion de las graficas
library(readxl) #Facilita la lectura de archivos .xlsx
library(tidytext) #Esctructuracion y analisis de textos
library(dplyr) #Manipulacion del dataframe
library(lubridate) #Manejo de fechas y horas
library(openxlsx) #Manipulacion y escritura de archivos .xlsx
library(tidyr) #Transformacion y y reorganizacion de datos 
library(kableExtra) #Presentacion de tablas en R Markdown
library(svDialogs) #Interfaz de dialogos para windows
library(tweetbotornot) #Predice si una cuenta es un bot o no 
library(stringr) #Manipulacion de cadenas de texto
library(rtweet) #Analisis de datos de twitter API
library(webshot) #Captura de graficos de R como imagenes
library(htmlwidgets) #Manipulacion de HTML con R

#Prueba GitHub

tweets1 <- read.csv("tweets.csv")
tweets <- tweets1

tweets1 <- rtweet::read_twitter_csv("tweets.csv", unflatten = FALSE)

# Gráfica: hashtags----

tweets1 %>%
  tidytext::unnest_tokens(output = "hashtags",
                          input = "hashtags",
                          token = "words") %>% 
  dplyr::select("hashtags") %>% 
  drop_na() %>% 
  count(hashtags, sort = TRUE) %>%
  head(10) %>%
  mutate(hashtags = reorder(hashtags, n)) %>%
  ggplot(aes(x = hashtags, y = n)) +
  theme_classic()+
  geom_col(aes(fill = n),
           show.legend = FALSE) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Conteo de hashtags",
       x = NULL,
       title = "Hashtags relevantes",
       subtitle = "",
       caption = "Fuente: Twitter, Inc.") +
  geom_text(aes(label = n), size = 3, hjust = 1.5, color = "white")

ggplot2::ggsave("01_hashtags.png",
                device = "png",
                width = 30,
                height = 20,
                units = "cm",
                dpi = "retina")

# Alcance de usuarios----

alcance <- tweets %>% 
  dplyr::group_by(usuario, nombre, seguidores) %>%
  dplyr::summarise(publicaciones = dplyr::n(), 
                   alcance = sum(seguidores),
                   .groups = "keep") %>% 
  dplyr::arrange(desc(alcance))

openxlsx::write.xlsx(alcance, "02_alcance_general.xlsx")

alcance %>% 
  head(20) %>% 
  kable() %>% 
  kable_styling(full_width = TRUE,
                font_size = 20,
                row_label_position = "c") %>%
  row_spec(0, color = "#F5F8FA", background = "#000") %>% 
  column_spec(1, bold = TRUE, border_right = TRUE, color = "#F5F8FA", background = "#979797") %>% 
  save_kable("02_alcance_general.png")

# Tweets líderes de opinión----

lideres <- readxl::read_xlsx("C:/Desarrollo Python/WebScraping Twitter/clasificacion_usuarios.xlsx",
                             sheet = "lideres",
                             col_names = TRUE,) %>% 
  dplyr::pull()

tweets_lideres <- tweets1 %>%  
  dplyr::filter(usuario %in% lideres) %>% 
  dplyr::arrange(desc(fecha))

openxlsx::write.xlsx(tweets_lideres, "03_tweets_lideres.xlsx")

# Alcance de líderes de opinión----

alcance_lideres <- tweets_lideres %>% 
  dplyr::group_by(usuario, nombre, seguidores) %>%
  dplyr::summarise(publicaciones = dplyr::n(), 
                   alcance = sum(seguidores),
                   .groups = "keep") %>% 
  dplyr::arrange(desc(alcance))

openxlsx::write.xlsx(alcance_lideres, "03_alcance_lideres.xlsx")

alcance_lideres %>% 
  head(20) %>% 
  kable() %>% 
  kable_styling(full_width = TRUE,
                font_size = 20) %>%
  row_spec(0, color = "#F5F8FA", background = "#000") %>% 
  column_spec(1, bold = TRUE, border_right = TRUE, color = "#F5F8FA", background = "#979797") %>%
  save_kable("03_alcance_lideres.png")

# Gráfica lideres de opinión----

tweets_lideres %>%
  dplyr::count(nombre, sort = TRUE) %>% 
  dplyr::mutate(nombre = reorder(nombre, n)) %>% 
  head(10) %>% 
  ggplot(aes(x = nombre, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  scale_fill_gradient(low = "#B38E5D",
                      high = "#4E232E") +
  geom_text(aes(label = n),
            hjust = -0.2,
            size = 4) +
  labs(title = "Líderes de opinión",
       subtitle = "Usuarios con más tweets emitidos",
       x = NULL,
       y = "cantidad de tweets",
       caption = "Fuente: Twitter, Inc.")

ggplot2::ggsave("03_lideres.png",
                width = 25,
                height = 12.5,
                units = "cm",
                device = "png",
                dpi = "retina")

# Tweets medios de comunicación----

medios <- readxl::read_xlsx("C:/Desarrollo Python/WebScraping Twitter/clasificacion_usuarios.xlsx",
                            sheet = "medios",
                            col_names = TRUE,) %>% 
  dplyr::pull()

tweets_medios <- tweets1 %>%  
  dplyr::filter(usuario %in% medios) %>% 
  dplyr::arrange(desc(fecha))

openxlsx::write.xlsx(tweets_medios, "06_tweets_medios_locales.xlsx")

# Gráfica medios de comunicación----

tweets_medios %>%
  dplyr::count(nombre, sort = TRUE) %>% 
  dplyr::mutate(nombre = reorder(nombre, n)) %>% 
  head(10) %>% 
  ggplot(aes(x = nombre, y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  theme_classic() +
  scale_fill_gradient(low = "#B38E5D",
                      high = "#4E232E") +
  geom_text(aes(label = n),
            hjust = -0.2,
            size = 4) +
  labs(title = "Medios de comunicación",
       subtitle = "Usuarios con más tweets emitidos",
       x = NULL,
       y = "cantidad de tweets",
       caption = "Fuente: Twitter, Inc.")

ggplot2::ggsave("06_medios_locales.png",
                width = 25,
                height = 12.5,
                units = "cm",
                device = "png",
                dpi = "retina")

# Alcance de medios de comunicación----

alcance_medios <- tweets_medios %>% 
  dplyr::group_by(usuario, nombre, seguidores) %>%
  dplyr::summarise(publicaciones = dplyr::n(), 
                   alcance = sum(seguidores),
                   .groups = "keep") %>% 
  dplyr::arrange(desc(alcance))

openxlsx::write.xlsx(alcance_medios, "06_alcance_medios_locales.xlsx")

alcance_medios %>% 
  head(20) %>% 
  kable() %>% 
  kable_styling(full_width = TRUE,
                font_size = 20) %>%
  row_spec(0, color = "#F5F8FA", background = "#000") %>% 
  column_spec(1, bold = TRUE, border_right = TRUE, color = "#F5F8FA", background = "#979797") %>%
  save_kable("06_alcance_medios_locales.png")

# Lista de palabras vacias----

palabras_vacias <- stopwords("es") %>%
  base::as.data.frame()

palabras_custom <- c("si", "va", "tan", "ricardo", "así", "monreal",
                     "t.co", "https", "dio", "q", "d", "ricardomonreala",
                     "van", "l", "of", "to", "the", "solo", "hace",
                     "pues", "cómo", "hacer", "ser","m_ebrard", "ebrard","marcelo") %>% 
  base::as.data.frame()

# Separación de tweets en palabras----

tweets_token <- tweets %>%
  tidytext::unnest_tokens(output = "palabra",
                          input = "tweet",
                          token = "words") %>% 
  dplyr::anti_join(palabras_vacias, 
                   by = c("palabra" = ".")) %>% 
  dplyr::anti_join(palabras_custom, 
                   by = c("palabra" = "."))

# Nube de palabras----

tweets_palabras <- tweets_token %>% 
  dplyr::count(palabra, sort = TRUE) %>%
  dplyr::mutate(palabra = reorder(palabra, n)) %>% 
  head(50)

wordcloud2::wordcloud2(tweets_palabras,
                       size = 1,
                       color = "#B38E5D")
nube <- wordcloud2(data = tweets_palabras, size = 1, color = "#B38E5D")

htmlwidgets::saveWidget(nube, 
                        "C:/Desarrollo Python/WebScraping Twitter/Reporte Diario/MarceloEbrard/nube_palabras.html", 
                        selfcontained = FALSE)

webshot::webshot("C:/Desarrollo Python/WebScraping Twitter/Reporte Diario/MarceloEbrard/nube_palabras.html", 
                 "C:/Desarrollo Python/WebScraping Twitter/Reporte Diario/MarceloEbrard/nube_palabras.png", 
                 delay = 5, vwidth = 1240, vheight = 914)

# Clasificación de tweets por sentimiento y creación de engagement----

valores_affin <- readxl::read_xlsx("C:/Desarrollo Python/WebScraping Twitter/sentimientos.xlsx",
                                   col_names = TRUE)

tweets_afinn <- tweets %>%
  tidytext::unnest_tokens(input = "tweet",
                          output = "Palabra",
                          token = "words") %>%
  dplyr::inner_join(valores_affin, ., by = "Palabra") %>%
  dplyr::mutate(Tipo = ifelse(Puntuacion > 0, "Positivo", "Negativo"))

tweets_engagement <- tweets_afinn %>%
  dplyr::group_by(status_id) %>%
  dplyr::summarise(Puntuacion_tweet = base::mean(Puntuacion)) %>%
  dplyr::left_join(tweets, ., by = "status_id") %>% 
  dplyr::mutate(Puntuacion_tweet = base::ifelse(is.na(Puntuacion_tweet), 0, Puntuacion_tweet)) %>%
  dplyr::mutate(Sentimiento = base::ifelse(Puntuacion_tweet == 0, "Neutro", 
                                           base::ifelse(Puntuacion_tweet > 0, "Positivo", "Negativo"))) %>% 
  select(usuario, nombre, seguidores, fecha, tweet, favs, rts, sentimiento = Sentimiento)

openxlsx::write.xlsx(tweets_engagement, "07_tweets_engagement.xlsx")

openxlsx::write.xlsx(tweets_palabras,"08_tweets_palabras.xlsx")

#Gráfica de emojis ----

# Librerías
library(stringr)
library(plotly)

#Carga de dataframe
data <- data.frame(tweets_engagement$tweet)

# Función para extraer emojis de un texto
extract_emojis <- function(text) {
  emojis <- str_extract_all(text, "[\\p{So}]")
  emojis <- unlist(emojis)
  return(emojis)
}

# Aplicar la función a los comentarios
all_emojis <- unlist(lapply(data$tweet, extract_emojis))

# Contar la frecuencia de cada emoji
emoji_counts <- table(all_emojis)

# Crear un dataframe con los resultados
emoji_df <- data.frame(
  emoji = names(emoji_counts),
  count = as.numeric(emoji_counts)
)

# Filtrar emojis no deseados (M y X)
emojis_no_deseados <- c("🇲", "🇽")
emoji_df <- emoji_df[!emoji_df$emoji %in% emojis_no_deseados, ]

# Ordenar el dataframe por frecuencia
emoji_df <- emoji_df %>%
  arrange(desc(count))

# Tomar el top 10 de emojis
top_emoji_df <- head(emoji_df, 10)

# Colores específicos para cada emoji
emoji_colors <- c("#FF5733", "#FFC300", "#33FF57", "#3357FF", "#FF33D0",
                  "#33D0FF", "#33FFC8", "#FF3384", "#33FF3E", "#FFA433")

# Visualización interactiva con plotly
emoji <- plot_ly(data = top_emoji_df, x = ~count, y = ~reorder(emoji, +count), type = "bar",
                 orientation = "h", marker = list(color = emoji_colors),
                 text = ~paste(count)) %>%
  layout(
    title = "Top 10 Emojis usados en la conversación",
    xaxis = list(title = "Conteo"),
    yaxis = list(title = "Emojis", tickfont = list(size = 19))
  )

openxlsx::write.xlsx(top_emoji_df,"04_emoji_cuenta.xlsx")

print(emoji)

#Nube Emojis ----

library(tm)
library(slam)
library(wordcloud2)

# Crear un corpus con los emojis repetidos según su conteo
corpus <- Corpus(VectorSource(rep(emoji_df$emoji, times = emoji_df$count)))

# Preprocesar el corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))  # Opcional: quitar stopwords
corpus <- tm_map(corpus, stripWhitespace)

# Crear una matriz de términos
dtm <- DocumentTermMatrix(corpus)
# Obtener la frecuencia de términos
term_freq <- col_sums(as.matrix(dtm))
# Ordenar la frecuencia de términos
term_freq <- term_freq[order(term_freq, decreasing = TRUE)]

# Crear la nube de palabras con emojis
nube_emoji <- wordcloud2::wordcloud2(
  data = data.frame(word = names(term_freq), freq = term_freq),
  size = 1.5,
  color = "random-light",
  backgroundColor = "black",
)

print(nube_emoji)

openxlsx::write.xlsx(emoji_df,"09_lista_emojis.xlsx")
#openxlsx::write.xlsx(top_emoji_df,"top_emojis.xlsx")