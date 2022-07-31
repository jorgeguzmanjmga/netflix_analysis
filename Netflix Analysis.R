library(tidyverse)

# Primero importamos nuestras bases
# Estas fueron obtenidas de kaggle en el siguiente link
# https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies

titles<-read.csv("titles.csv", encoding="UTF-8") #Usamos UTF-8 para los acentos
cast<-read.csv("credits.csv", encoding="UTF-8")

# Pequeña exploracion de nuestras bases
# titles
head(titles)
str(titles) # 5806 rows y 15 columnas
#Nos quedaremos con las columnas:
# id, title, type, release_year, age_certification, runtime, seasons, imdb_id, imdb_score
titles1<-select(titles, c(id, title, type, release_year, age_certification, runtime, 
                          seasons, imdb_id, imdb_score))
#cast
head(cast)
str(cast) # 77213 rows y 5 columnas



# Data Visualization
#La base ya se encuentra limpia, haremos algunas visualizaciones

#Empezaremos haciendo un histograma para observar la composicion de peliculas por año

ggplot(titles1)+
  geom_histogram(aes(x=release_year), binwidth = 5)+
  theme_minimal()
#Notamos que la mayoría de las películas y series se estrenaron hace menos de 5 años
#Procedemos a ver la verdadera proporcion

titles2010 <- titles1 %>% 
  filter(release_year>2010) 
str(titles2010) #5106
5106/5806
#El 88% de las películas es de 2010 en adelante
titles2015 <- titles1 %>% 
  filter(release_year>2015) 
str(titles2015)
4353/5806
#75% es de 2015 en adelante




#Ahora veamos cómo se compone el catálogo entre series o películas
ggplot(titles1)+
  geom_bar(aes(x=type, fill=type))+
  labs(title="Cantidad de contenido por tipo", y="Cantidad", x="Tipo")+
  theme_minimal()

#Veamos cual es la duración promedio de las peliculas
titles1 %>% filter(type=="MOVIE") %>% 
  ggplot()+
  geom_histogram(aes(x=runtime), fill="midnightblue")+
  labs(title="Duración de las películas")+
  theme_minimal()
#Tiene forma de normal
#qqnorm(titles$runtime)

#Para las series veamos el número de temporadas promedio
titles1 %>% filter(type=="SHOW") %>% 
  ggplot()+
  geom_bar(aes(x=seasons))+
  theme_minimal()
#Notamos que hay un outlier que dice que tiene 42 temporados, vamos a buscarlo
titles1 %>%  filter(seasons > 14) %>% select(title, seasons, imdb_score) %>% arrange(-seasons)
#Calificacion promedio de las series
series<-titles1 %>% filter(type=="SHOW") 
summary(series$imdb_score) #Media de las series es 7.017

# Vemos que se trata de programas que si han tenido una gran cantidad de temporadas, 
#Para no perjudicar el análisis prescindiremos de ellos

titles %>% filter(type=="SHOW"&seasons<20) %>% 
  ggplot()+
  geom_bar(aes(x=seasons))+
  labs(title="\t Número de temporadas promedio en series")+
  theme_void()
#La mayoría de los títulos tiene entre 1 y 2 temporadas

#Ahora veremos cuales son los actores con más apariciones en Netflix
cast %>% filter(role=="ACTOR") %>% 
  group_by(name) %>%
  summarise(n=n()) %>% 
  filter(n>20) %>% 
  arrange(-n) 

#### Ideas de analisis

#Relacion entre la calificacion de temporadas previas y creacion de nuevas, responder a la pregunta
#¿Que tanto influye la critica para continuar alguna serie?

#Averiguar qué actores han hecho más películas y su rating promedio (necesitaría hacer un join)