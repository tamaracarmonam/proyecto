install.packages(c("tidyverse", "tidytext", "wordcloud2", "ggplot2"))
install.packages("tidyverse") 
install.packages("cluster")     
install.packages("factoextra") 
install.packages("word2vec")
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(ggplot2)
library(dplyr)
library(lubridate)
library(word2vec)
library(cluster)
library(factoextra)

#cargar bases de datos
insta <- read.csv("C:/Users/Asus/Desktop/covid_2020/noticias_covid_igram_2020(JAN_DEC)_ESP.csv", sep = ";", stringsAsFactors = FALSE)
facebook <- read.csv("C:/Users/Asus/Desktop/covid_2020/noticias_covid_fbk_2020(JAN_DEC)_ESP.csv", sep = ";", stringsAsFactors = FALSE)
twitter <- read.csv("C:/Users/Asus/Desktop/covid_2020/noticias_covid_twitter_2020(JAN_DEC)_ESP.csv", sep = ";", stringsAsFactors = FALSE)

# Ver las primeras filas de los dataframes
head(insta)
head(facebook)
head(twitter)

# Ver las columnas disponibles en cada dataframe
colnames(insta)
colnames(facebook)
colnames(twitter)

# Corregir la codificación de caracteres especiales
insta$Description <- iconv(insta$Description, from = "UTF-8", to = "ASCII//TRANSLIT")
facebook$Message <- iconv(facebook$Message, from = "UTF-8", to = "ASCII//TRANSLIT")
twitter$Message <- iconv(twitter$Message, from = "UTF-8", to = "ASCII//TRANSLIT")

# Tokenizar los textos de las descripciones de Instagram
#unnest_tokens(): Divide los textos en palabras (tokens) y las organiza en un nuevo dataframe donde cada fila es una palabra.
insta_tokens <- insta %>%
  unnest_tokens(word, Description)

# Tokenizar los textos de los mensajes de Facebook
facebook_tokens <- facebook%>%
  unnest_tokens(word, Message)

# Tokenizar los textos de los mensajes de Twitter
twitter_tokens <- twitter %>%
  unnest_tokens(word, Message)

# Contar frecuencia de palabras en Instagram
insta_word_count <- insta_tokens %>%
  count(word, sort = TRUE)

# Contar frecuencia de palabras en Facebook
facebook_word_count <- facebook_tokens %>%
  count(word, sort = TRUE)

# Contar frecuencia de palabras en Twitter
twitter_word_count <- twitter_tokens %>%
  count(word, sort = TRUE)

# Cargar stop words en español
#palabras comunes que no aportan un valor significativo para el análisis de texto
stop_words_es <- c("y", "de", "el", "la", "los", "las", "en", "un", "una", "que", "con", "por", "para","a","se","del","al","su","es","este","ha","mas","lo","esta","han","como","sus","http","https","so","www","t.co","pic.twitter.com","bit.ly","html")

# Filtrar palabras vacías de los resultados de Instagram
insta_word_count <- insta_word_count %>%
  filter(!word %in% stop_words_es)

# Filtrar palabras vacías de los resultados de Facebook
facebook_word_count <- facebook_word_count %>%
  filter(!word %in% stop_words_es)

# Filtrar palabras vacías de los resultados de Twitter
twitter_word_count <- twitter_word_count %>%
  filter(!word %in% stop_words_es)

# Gráfico de palabras más frecuentes en Instagram
ggplot(insta_word_count %>% top_n(10), aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Palabras más frecuentes en Instagram", x = "Palabras", y = "Frecuencia")

# Gráfico de palabras más frecuentes en Facebook
ggplot(facebook_word_count %>% top_n(10), aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Palabras más frecuentes en Facebook", x = "Palabras", y = "Frecuencia")

# Gráfico de palabras más frecuentes en Twitter
ggplot(twitter_word_count %>% top_n(10), aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Palabras más frecuentes en Twitter", x = "Palabras", y = "Frecuencia")

# Usar el diccionario 'bing' para analizar sentimientos
sentiments_insta <- insta_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE)

# Ver resultados
sentiments_insta

# Crear un gráfico de barras para los sentimientos
ggplot(sentiments_insta, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribución de Sentimientos en Instagram",
       x = "Sentimiento",
       y = "Frecuencia") +
  scale_fill_manual(values = c("positive" = "steelblue", "negative" = "firebrick")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Usar el diccionario 'bing' para analizar sentimientos
sentiments_face <- facebook_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE)

# Ver resultados
sentiments_face

# Crear un gráfico de barras para los sentimientos
ggplot(sentiments_face, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribución de Sentimientos en Instagram",
       x = "Sentimiento",
       y = "Frecuencia") +
  scale_fill_manual(values = c("positive" = "steelblue", "negative" = "firebrick")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# Usar el diccionario 'bing' para analizar sentimientos
sentiments_twitter <- twitter_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, sort = TRUE)

# Ver resultados
sentiments_twitter

# Crear un gráfico de barras para los sentimientos
ggplot(sentiments_twitter, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Distribución de Sentimientos en Instagram",
       x = "Sentimiento",
       y = "Frecuencia") +
  scale_fill_manual(values = c("positive" = "steelblue", "negative" = "firebrick")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

################################################
#intentaré usar el word2vec

str(insta_tokens)

# Agrupar las palabras tokenizadas en oraciones completas
insta_sentences <- insta_tokens %>%
  group_by(X_id) %>%  
  summarise(text = paste(word, collapse = " ")) %>%  # Juntar las palabras tokenizadas
  pull(text)  # Extraer las oraciones en formato de lista

# Entrenar el modelo Word2Vec
model <- word2vec(x = insta_sentences, type = "skip-gram", dim = 100, iter = 5)
#x = insta_sentences: el conjunto de oraciones (una lista de secuencias de palabras).
#type = "skip-gram": Word2Vec tiene dos tipos de arquitecturas: skip-gram y CBOW. Skip-gram suele ser más útil para textos pequeños, ya que predice el contexto de una palabra dada.
#dim = 100: el número de dimensiones del vector de palabras 
#iter = 5: cuántas veces se repite el entrenamiento, podemos aumentar esto por "precisión"

# Palabras similares a "pandemia"
similar_words <- predict(model, newdata = "pandemia", type = "nearest", top_n = 5)
print(similar_words)
#aca estamos "prediciendo" las palabras más similares a "pandemia" en términos de significado y contexto.

# Extraer los vectores de palabras
word_vectors <- as.matrix(model)
#Cada palabra en el conjunto de datos tiene un vector asociado que describe su contexto y significado en función de cómo aparece con otras palabras.

# Usar PCA para reducir la dimensionalidad
pca <- prcomp(word_vectors, scale. = TRUE)
#pca: analisis de componentes principales
# para poder visualizar/analizar las relaciones entre palabras de manera más simple

# Graficar las dos primeras componentes principales
plot(pca$x[,1], pca$x[,2], 
     main = "Visualización de Word2Vec con PCA",
     xlab = "Componente Principal 1", 
     ylab = "Componente Principal 2")
#Si notas que algunos puntos (palabras) tienden a formar grupos o clústeres,
#eso indica que esas palabras están relacionadas semánticamente y tienen 
#significados o contextos similares. 
# se ve una mancha gigante, por lo que muchas palabras tienen una distribución similar en el espacio vectorial y, por tanto, sus contextos de uso no son muy distintos entre sí.
#tiene sentido dado que son comentarios sobre algo en especifico, el covid

# Con estas visualizaciones, puedes explorar cómo las palabras que 
#entrenaste con Word2Vec se agrupan y qué relaciones semánticas existen
#entre ellas en tu corpus de descripciones de Instagram.


###################################################################################

#Clusterización de temas: agrupar palabras o publicaciones en torno a temas comunes usando los vectores generados

# Convertir los vectores de palabras a un DataFrame
word_vectors_df <- as.data.frame(word_vectors)

# Calcular la matriz de distancias ( dist euclidiana=distancia directa ("en línea recta") entre dos puntos en un espacio multidimensional)
dist_matrix <- dist(word_vectors_df, method = "euclidean")

# Realizar K-means clustering
set.seed(123)  # Para reproducibilidad #use una secuencia fija de números aleatorios
k <- 5  # Número de clusters 
kmeans_result <- kmeans(word_vectors_df, centers = k)

#Agregar la clasificación de clusters a un DataFrame
# Debemos identificar a qué palabras pertenecen los vectores
clusters <- data.frame(word = rownames(word_vectors_df), cluster = as.factor(kmeans_result$cluster))
#el proceso de formación de clusters se basa en asignar puntos a los centroides más cercanos
#agrupa palabras que son similares entre sí

# Mostrar los resultados de clusterización
print(clusters)

# Visualizar los clusters
fviz_cluster(kmeans_result, data = word_vectors_df, 
             geom = "point", 
             ellipse.type = "convex", 
             main = "Clusterización de Temas con K-means")

# Inspeccionar las palabras en cada cluster
for (i in 1:k) {
  cat("Cluster", i, ":\n")
  print(clusters$word[clusters$cluster == i])
  cat("\n")
}

# creamos un DataFrame que almacena los centros de cada cluster generados por el algoritmo K-means
cluster_centers <- data.frame(cluster = 1:k, matrix(kmeans_result$centers, nrow = k))

# Convertir word_vectors_df a matriz para operaciones matemáticas
word_vectors_matrix <- as.matrix(word_vectors_df)

# Función para encontrar la palabra más cercana
find_nearest_word <- function(center_vector, word_vectors_matrix) {
  # Calcular distancias entre el vector del centro y todos los vectores de palabras
  distances <- rowSums((word_vectors_matrix - center_vector)^2) # Usar la distancia euclidiana
  
  # Obtener el índice de la palabra más cercana
  nearest_word_index <- which.min(distances)
  
  # Devolver la palabra más cercana
  return(rownames(word_vectors_df)[nearest_word_index])
}

# Encontrar el título de cada cluster
titles <- sapply(1:k, function(i) find_nearest_word(as.numeric(cluster_centers[i, -1]), word_vectors_matrix))
# el titula se determina según la palabra más cercana al centro de cada cluster

# Mostrar los títulos de los clusters
titles_df <- data.frame(cluster = 1:k, title = titles)
print(titles_df)


##########################################################
#####LO MISMO PERO CON K=4#####################

# Convertir los vectores de palabras a un DataFrame 
word_vectors_df <- as.data.frame(word_vectors)

# Calcular la matriz de distancias (usamos distancia euclidiana)
dist_matrix <- dist(word_vectors_df, method = "euclidean")

# Realizar K-means clustering
set.seed(123)  # Para reproducibilidad
k <- 4  # Número de clusters 
kmeans_result <- kmeans(word_vectors_df, centers = k)

#Agregar la clasificación de clusters a un DataFrame
# Debemos identificar a qué palabras pertenecen los vectores
clusters <- data.frame(word = rownames(word_vectors_df), cluster = as.factor(kmeans_result$cluster))

# Mostrar los resultados de clusterización
print(clusters)

# Visualizar los clusters
fviz_cluster(kmeans_result, data = word_vectors_df, 
             geom = "point", 
             ellipse.type = "convex", 
             main = "Clusterización de Temas con K-means")

# Inspeccionar las palabras en cada cluster
for (i in 1:k) {
  cat("Cluster", i, ":\n")
  print(clusters$word[clusters$cluster == i])
  cat("\n")
}

# 
cluster_centers <- data.frame(cluster = 1:k, matrix(kmeans_result$centers, nrow = k))

# Convertir word_vectors_df a matriz para operaciones matemáticas
word_vectors_matrix <- as.matrix(word_vectors_df)

# Función para encontrar la palabra más cercana
find_nearest_word <- function(center_vector, word_vectors_matrix) {
  # Calcular distancias entre el vector del centro y todos los vectores de palabras
  distances <- rowSums((word_vectors_matrix - center_vector)^2) # Usar la distancia euclidiana
  
  # Obtener el índice de la palabra más cercana
  nearest_word_index <- which.min(distances)
  
  # Devolver la palabra más cercana
  return(rownames(word_vectors_df)[nearest_word_index])
}

# Encontrar el título de cada cluster
titles <- sapply(1:k, function(i) find_nearest_word(as.numeric(cluster_centers[i, -1]), word_vectors_matrix))

# Mostrar los títulos de los clusters
titles_df <- data.frame(cluster = 1:k, title = titles)
print(titles_df)

##########################################################
########LO MISMO PERO CON K=3########################

# Convertir los vectores de palabras a un DataFrame (si no lo has hecho)
word_vectors_df <- as.data.frame(word_vectors)

# Calcular la matriz de distancias (usamos distancia euclidiana)
dist_matrix <- dist(word_vectors_df, method = "euclidean")

# Realizar K-means clustering
set.seed(123)  # Para reproducibilidad
k <- 3  # Número de clusters 
kmeans_result <- kmeans(word_vectors_df, centers = k)

#Agregar la clasificación de clusters a un DataFrame
# Debemos identificar a qué palabras pertenecen los vectores
clusters <- data.frame(word = rownames(word_vectors_df), cluster = as.factor(kmeans_result$cluster))

# Mostrar los resultados de clusterización
print(clusters)

# Visualizar los clusters
fviz_cluster(kmeans_result, data = word_vectors_df, 
             geom = "point", 
             ellipse.type = "convex", 
             main = "Clusterización de Temas con K-means")

# Inspeccionar las palabras en cada cluster
for (i in 1:k) {
  cat("Cluster", i, ":\n")
  print(clusters$word[clusters$cluster == i])
  cat("\n")
}

# Asegúrate de que cluster_centers es un data frame
cluster_centers <- data.frame(cluster = 1:k, matrix(kmeans_result$centers, nrow = k))

# Convertir word_vectors_df a matriz para operaciones matemáticas
word_vectors_matrix <- as.matrix(word_vectors_df)

# Función para encontrar la palabra más cercana
find_nearest_word <- function(center_vector, word_vectors_matrix) {
  # Calcular distancias entre el vector del centro y todos los vectores de palabras
  distances <- rowSums((word_vectors_matrix - center_vector)^2) # Usar la distancia euclidiana
  
  # Obtener el índice de la palabra más cercana
  nearest_word_index <- which.min(distances)
  
  # Devolver la palabra más cercana
  return(rownames(word_vectors_df)[nearest_word_index])
}

# Encontrar el título de cada cluster
titles <- sapply(1:k, function(i) find_nearest_word(as.numeric(cluster_centers[i, -1]), word_vectors_matrix))

# Mostrar los títulos de los clusters
titles_df <- data.frame(cluster = 1:k, title = titles)
print(titles_df)


####################################################
####################################################
##################################





#realizar una clasificación de textos, utilizando los vectores obtenidos 
#con Word2Vec como características de entrada, y un modelo de clasificación 
#supervisada, como Regresión Logística o SVM, para categorizar los textos.

##jeje no pude:'c