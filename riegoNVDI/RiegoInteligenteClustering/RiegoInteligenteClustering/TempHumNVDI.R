#Script para preparación de los datos de humedad y temperatura junto con el NVDI para tener un dataset que relacione estas tres variables y determinar si además la humedad
#y temperatura son anómalas respecto a ciertos valores del NVDI que determinaremos como anómalos (valores de NVDI muy bajos o altos para un parque en concreto)
#Author: Carlos Hernández Hidalgo
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)
library(plyr)
library(readr)
#auxx3 <- data.frame(auxx[[1]],auxx[[2]], auxx[[3]]) auxx <- contrato_MU21[1:3]
################################################
ind = dflt$ind   #índice de medición, en este caso NDVI 
mask = dflt$mask #máscara, en este caso "scl_7_8_9"
agg = dflt$agg #"mean"
serie = dflt$serie #tipo de serie, "Verdor"
pkg = dflt$pkg #paquete usado para clustering, "TSClust"
ini = as.Date("2018-01-13") #dflt$ini #Fecha de inicio para filtrar a partir de ahí los datos sobre los que queremos hacer clustering
fin = as.Date("2022-02-20") #dflt$fin #Fecha de fin para filtrar los datos sobre los que queremos aplicar clustering
aggl = "complete" #usado para ejecutar el clustering
################################################
#Obtenemos y preparamos todos los datos de humedad y temperatura de las distintas estaciones meteorológicas
################################################
ini_mediciones = as.Date("2018-01-16")  #Fecha de inicio para filtrar a partir de ahí los datos de humedad y temperatura
fin_mediciones = as.Date("2021-03-31") #Fecha de fin para filtrar los datos de humedad y temperatura
################################################
#Estación CA42
################################################
dat_CA42 <- read_csv2(file.path("datos","TempHum","CA42.txt"),col_types = cols(.default = "c"))
#Suprimimos las columnas que no necesitamos
aux <- dat_CA42 %>%
  dplyr::select(fecha,tmed,hrmed)
#Transformamos los datos de temperatura y humedad a numéricos. Eran "Name characters"
#eliminamos los nombres
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#Hay que transformar chr en num
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#Quitamos las filas que contengan NAs
aux <- na.omit(aux)
#Agrupamos por fechas y obtenemos la media de las mediciones de cada día
aux1 <- ddply(aux, "fecha",colwise(mean))
#Nos quedamos solo con las fechas para las que tenemos datos de NVDI
TH_CA42 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#Estación CA91
################################################
dat_CA91 <- read_csv2(file.path("datos","TempHum","CA91.txt"),col_types = cols(.default = "c"))
#Suprimimos las columnas que no necesitamos
aux <- dat_CA91 %>%
  dplyr::select(fecha,tmed,hrmed)
#Transformamos los datos de temperatura y humedad a numéricos. Eran "Name characters"
#eliminamos los nombres
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#Hay que transformar chr en num
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#Quitamos las filas que contengan NAs
aux <- na.omit(aux)
#Agrupamos por fechas y obtenemos la media de las mediciones de cada día
aux1 <- ddply(aux, "fecha",colwise(mean))
#Nos quedamos solo con las fechas para las que tenemos datos de NVDI
TH_CA91 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#Estación MO12
################################################
dat_MO12 <- read_csv2(file.path("datos","TempHum","MO12.txt"),col_types = cols(.default = "c"))
#Suprimimos las columnas que no necesitamos
aux <- dat_MO12 %>%
  dplyr::select(fecha,tmed,hrmed)
#Transformamos los datos de temperatura y humedad a numéricos. Eran "Name characters"
#eliminamos los nombres
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#Hay que transformar chr en num
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#Quitamos las filas que contengan NAs
aux <- na.omit(aux)
#Agrupamos por fechas y obtenemos la media de las mediciones de cada día
aux1 <- ddply(aux, "fecha",colwise(mean))
#Nos quedamos solo con las fechas para las que tenemos datos de NVDI
TH_MO12 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#Estación MU21
################################################
dat_MU21 <- read_csv2(file.path("datos","TempHum","MU21.txt"),col_types = cols(.default = "c"))
#Suprimimos las columnas que no necesitamos
aux <- dat_MU21 %>%
  dplyr::select(fecha,tmed,hrmed)
#Transformamos los datos de temperatura y humedad a numéricos. Eran "Name characters"
#eliminamos los nombres
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#Hay que transformar chr en num
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#Quitamos las filas que contengan NAs
aux <- na.omit(aux)
#Agrupamos por fechas y obtenemos la media de las mediciones de cada día
aux1 <- ddply(aux, "fecha",colwise(mean))
#Nos quedamos solo con las fechas para las que tenemos datos de NVDI
TH_MU21 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#Estación MU62
################################################
dat_MU62 <- read_csv2(file.path("datos","TempHum","MU62.txt"),col_types = cols(.default = "c"))
#Suprimimos las columnas que no necesitamos
aux <- dat_MU62 %>%
  dplyr::select(fecha,tmed,hrmed)
#Transformamos los datos de temperatura y humedad a numéricos. Eran "Name characters"
#eliminamos los nombres
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#Hay que transformar chr en num
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#Quitamos las filas que contengan NAs
aux <- na.omit(aux)
#Agrupamos por fechas y obtenemos la media de las mediciones de cada día
aux1 <- ddply(aux, "fecha",colwise(mean))
#Nos quedamos solo con las fechas para las que tenemos datos de NVDI
TH_MU62 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#Preparamos los contratos
################################################
contratos <- dflt$contratos("ConSeries", ind, mask, agg)
################################################
#Obtengo los contratos y acometidas clasificadas según la estación meteorológica más cercana
################################################
estaciones <- DatGeo$leerEstMet() #Las estaciones meteorológicas con sus ubicaciones
dflt$eemm #Los nombres de las estaciones meteorológicas
#Obtenemos el numero de acometida, contrato y estación meteorologica mas cercana o principal
sg <- DatGeo$asignarEstMet() %>%
  dplyr::select(Acometida, Contrato, Principal) 
contrato_ca42 <- sg%>%
  dplyr::filter(Principal == dflt$eemm[1]) %>% na.omit()
contrato_ca42 <- data.frame(contrato_ca42[[1]],contrato_ca42[[2]], contrato_ca42[[3]]) #Eliminamos la columna "geometry"
colnames(contrato_ca42) <- c("Acometida","Contrato","Estacion") #Cambiamos el nombre de las columnas
contrato_CA91 <- sg%>%
  dplyr::filter(Principal == dflt$eemm[2]) %>% na.omit()
contrato_CA91 <- data.frame(contrato_CA91[[1]],contrato_CA91[[2]], contrato_CA91[[3]]) #Eliminamos la columna "geometry"
colnames(contrato_CA91) <- c("Acometida","Contrato","Estacion") #Cambiamos el nombre de las columnas
contrato_MO12 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[3]) %>% na.omit()
contrato_MO12 <- data.frame(contrato_MO12[[1]],contrato_MO12[[2]], contrato_MO12[[3]]) #Eliminamos la columna "geometry"
colnames(contrato_MO12) <- c("Acometida","Contrato","Estacion") #Cambiamos el nombre de las columnas
contrato_MU21 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[4]) %>% na.omit()
contrato_MU21 <- data.frame(contrato_MU21[[1]],contrato_MU21[[2]], contrato_MU21[[3]]) #Eliminamos la columna "geometry"
colnames(contrato_MU21) <- c("Acometida","Contrato","Estacion") #Cambiamos el nombre de las columnas
contrato_MU62 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[5]) %>% na.omit()
contrato_MU62 <- data.frame(contrato_MU62[[1]],contrato_MU62[[2]], contrato_MU62[[3]]) #Eliminamos la columna "geometry"
colnames(contrato_MU62) <- c("Acometida","Contrato","Estacion") #Cambiamos el nombre de las columnas
################################################
#Preparamos S, que serán las series iniciales de NVDI
#Cada parque está identificado por un número de contrato
#Por cada parque tenemos los siguientes datos:
#   -Fecha: Día en el que se realizaron las mediciones de NVDI, se toman mediciones cada 5 días
#   -NDVI.scl_7_8_9.Min: El mínimo valor de NVDI de ese día
#   -NDVI.scl_7_8_9.Q1: El primer cuartil de los valores de NVDI
#   -NDVI.scl_7_8_9.Median: La mediana de los valores de NVDI
#   -NDVI.scl_7_8_9.Mean: La media de los valores de NVDI
#   -NDVI.scl_7_8_9.Q3: El tercer cuartil de los valores de NVDI
#   -NDVI.scl_7_8_9.Max: El máximo valor de NVDI de ese día
#   -NDVI.scl_7_8_9.n: El número de mediciones de NVDI realizadas ese día para ese parque
################################################
S <- Verdor$readSumInd(contratos, ind, mask, ini = ini, fin = fin)
################################################
#obtenemos las fechas
################################################
Date <-
  listDF2DF(S) %>%
  dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>%
  fil2col() %>% dplyr::select(Fecha)
#Para cada uno de los parques vamos añadiendo al dataset la media de NVDI para ese día y si presenta anomalía o no con el formato:
#NVDI_mean_numeroContrato 
#Anomalia_numeroContrato
#Esto lo haremos para cada una de las estaciones meteorológicas. De esta forma tendremos un dataset distinto para cada estación
#Con las temperaturas y humedades recogidas, los contratos, las fechas, un summary de anomalías de todos los parques en cada fecha
#Se decidirá si una medición de NVDI es anómala o no tomando de referencia la media de NVDI de todas las mediciones de cada parque
#Si pertenece al percentil 90 o 10, entonces será anómalo.
################################################
# Dataset Estación CA42
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
TH_CA42 <- subset(TH_CA42, fecha %in% Date$Fecha)
auxx3 <- TH_CA42 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_ca42$Contrato) {
  # print(contrato)
  #Obtenemos las medias de NVDI de ese parque
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_CA42$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #Obtenemos los dos valores que nos indicarán si han sido valores de NVDI anómalos o no
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #añadimos las dos nuevas columnas a nuestro dataset
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #Rellenamos con NA cuando no hay valores de NVDI
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_CA42$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia <- append(anomalia,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia <-append(anomalia,0)
      }
    }
  }
  TH_CA42$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  #las renombramos
  names(TH_CA42)[names(TH_CA42) == "media"] <- columna_media
  names(TH_CA42)[names(TH_CA42) == "anomalia"] <- columna_anomalia
  
  
}
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_CA42,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_CA42$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_CA42$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_CA42$anomaliesAgg2 <-ONES / (ONES+ZEROS)
TH_CA42$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_CA42, "anomaliesCA42.csv", row.names = F, sep=";")
################################################
# Dataset Estación CA91
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
TH_CA91 <- subset(TH_CA91, fecha %in% Date$Fecha)
auxx3 <- TH_CA91 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_MU62$Contrato) {
  # print(contrato)
  #Obtenemos las medias de NVDI de ese parque
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_CA91$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #Obtenemos los dos valores que nos indicarán si han sido valores de NVDI anómalos o no
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #añadimos las dos nuevas columnas a nuestro dataset
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #Rellenamos con NA cuando no hay valores de NVDI
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_CA91$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia <- append(anomalia,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia <-append(anomalia,0)
      }
    }
  }
  TH_CA91$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  #las renombramos
  names(TH_CA91)[names(TH_CA91) == "media"] <- columna_media
  names(TH_CA91)[names(TH_CA91) == "anomalia"] <- columna_anomalia
  
  
}
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_CA91,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_CA91$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_CA91$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_CA91$anomaliesAgg2 <-ONES / (ONES+ZEROS)
TH_CA91$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_CA91, "anomaliesCA91.csv", row.names = F, sep=";")
################################################
# Dataset Estación MO12
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
TH_MO12 <- subset(TH_MO12, fecha %in% Date$Fecha)
auxx3 <- TH_MO12 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_MU62$Contrato) {
  # print(contrato)
  #Obtenemos las medias de NVDI de ese parque
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_MO12$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #Obtenemos los dos valores que nos indicarán si han sido valores de NVDI anómalos o no
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #añadimos las dos nuevas columnas a nuestro dataset
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #Rellenamos con NA cuando no hay valores de NVDI
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_MO12$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia <- append(anomalia,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia <-append(anomalia,0)
      }
    }
  }
  TH_MO12$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  #las renombramos
  names(TH_MO12)[names(TH_MO12) == "media"] <- columna_media
  names(TH_MO12)[names(TH_MO12) == "anomalia"] <- columna_anomalia
  
  
}
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_MO12,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_MO12$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MO12$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MO12$anomaliesAgg2 <-ONES / (ONES+ZEROS)
TH_MO12$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_MO12, "anomaliesMO12.csv", row.names = F, sep=";")
################################################
# Dataset Estación MU21
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
TH_MU21 <- subset(TH_MU21, fecha %in% Date$Fecha)
auxx3 <- TH_MU21 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_MU62$Contrato) {
  # print(contrato)
  #Obtenemos las medias de NVDI de ese parque
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_MU21$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #Obtenemos los dos valores que nos indicarán si han sido valores de NVDI anómalos o no
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #añadimos las dos nuevas columnas a nuestro dataset
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #Rellenamos con NA cuando no hay valores de NVDI
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_MU21$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia <- append(anomalia,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia <-append(anomalia,0)
      }
    }
  }
  TH_MU21$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  #las renombramos
  names(TH_MU21)[names(TH_MU21) == "media"] <- columna_media
  names(TH_MU21)[names(TH_MU21) == "anomalia"] <- columna_anomalia
  
  
}
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_MU21,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_MU21$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MU21$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MU21$anomaliesAgg2 <-ONES / (ONES+ZEROS)
TH_MU21$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_MU21, "anomaliesMU21.csv", row.names = F, sep=";")
################################################
# Dataset Estación MU62
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
TH_MU62 <- subset(TH_MU62, fecha %in% Date$Fecha)
auxx3 <- TH_MU62 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_MU62$Contrato) {
  # print(contrato)
  #Obtenemos las medias de NVDI de ese parque
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_MU62$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #Obtenemos los dos valores que nos indicarán si han sido valores de NVDI anómalos o no
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #añadimos las dos nuevas columnas a nuestro dataset
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #Rellenamos con NA cuando no hay valores de NVDI
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_MU62$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia <- append(anomalia,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia <-append(anomalia,0)
      }
    }
  }
  TH_MU62$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  #las renombramos
  names(TH_MU62)[names(TH_MU62) == "media"] <- columna_media
  names(TH_MU62)[names(TH_MU62) == "anomalia"] <- columna_anomalia
  
  
}
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_MU62,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_MU62$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MU62$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MU62$anomaliesAgg2 <-ONES / (ONES+ZEROS)
TH_MU62$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_MU62, "anomaliesMU62.csv", row.names = F, sep=";")
