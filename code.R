#--------Tecnicas de Muestreo---------
#Muestreo aleatorio simple
sample(1:100, 20, replace = FALSE) #muestra con aleatorio simple de 20 numeros entre el 1 y el 100
datos[sample(nrow(datos), 12), ] #muestreo aleatorio simple para la base 'datos' de tamaño 12
sample_simple_random <- datos[sample(nrow(datos), 12), ]

getwd()
write.csv(sample_simple_random, file = "muestra_base.csv") #guarda la muestra
rm(sample_simple_random) #elimina el elemento

#Muestreo estratificado
library(dplyr) #libreria para el muestreo
set.seed(65) #ayuda a obtener resultado replicables
sample_stratified <- datos %>%
  group_by(edad_int) %>%
  sample_n(size=5)

getwd()
write.csv(sample_stratified, file = "muestra_estratificada.csv") #guarda la muestra
rm(sample_stratified) #elimina el elemento


#Muestreo por conglomerados
clusters <- sample(unique(datos$edo_civil), size=3, replace=F)
cluster_sample <- datos[datos$edo_civil %in% clusters, ]
cluster_sample$edo_civil<-factor(cluster_sample$edo_civil)

sample_clusters<-cluster_sample[sample(nrow(cluster_sample), 15), ]

levels(datos$edo_civil)
levels(sample_clusters$edo_civil)
table(sample_clusters$edo_civil, exclude = NULL)
rm(clusters)
rm(cluster_sample)

write.csv(sample_clusters, file = "muestra_conglomerados.csv") #guarda la muestra
rm(sample_clusters)

#-----------------Ponderador-----------
datos$factor<-NA #creamos una variable 'factor' que este vacia
#se le agregan los valores a 'factor' segun el criterio del ponderador de la edad
datos$factor[datos$edad_int == "18 a 35"] = 0.5
datos$factor[datos$edad_int == "36 a 50"] = 0.3
datos$factor[datos$edad_int == "51 o más"] = 0.2
table(datos$factor,datos$edad_int)

#datos$factor[datos$sexo == "Hombre"] = 0.80
#----EJEMPLO de llenado de valores del ponderador que cumplan cons 2 criterios------
#datos$factor[datos$edad == "18 a 35" & datos$edo_civil=="Casada" ] = 0.4
#datos$factor[datos$edad == "18 a 35" & datos$edo_civil=="Divorciada" ] = 0.2
#datos$factor[datos$edad == "18 a 35" & datos$edo_civil=="Soltera" ] = 0.1
#datos$factor[datos$edad == "18 a 35" & datos$edo_civil=="Viuda" ] = 0.3

#calculo de nuevas tablas agregando el ponderador
library(questionr) #requiere llamar esta biblioteca

#Promedio ponderado (solo para variables numericas)
mean(datos$edad) #promedio sin ponderador
wtd.mean(datos$edad, weights=NULL) #promedio ponderado "sin ponderador"
wtd.mean(datos$edad, weights=datos$factor) #promedio ponderado con ponderador 'factor'

#Porcentaje ponderado para UNA SOLA variable------
table(datos$edad_int) #tabla de frecuencias sin ponderador
options(digits=4)
prop.table(table(datos$edad_int))*100 #tabla de frecuencias en % pero sin ponderador
wtd.table(datos$edad_int, weights=datos$factor) #frecuencias observadas con ponderador
prop.table(wtd.table(datos$edad_int, weights=datos$factor))*100 #frecuencias observadas en % con ponderador

#Tabla cruzada ponderada (para DOS VARIABLES)-------
table(datos$edad_int, datos$edo_civil, exclude=NULL)
round(prop.table(table(datos$edad_int, datos$edo_civil))*100,1)
round(prop.table(wtd.table(datos$edad_int, datos$edo_civil, weights=datos$factor))*100,3)
