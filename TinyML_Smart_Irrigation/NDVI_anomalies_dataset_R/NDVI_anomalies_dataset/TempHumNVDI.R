#Script for preparing humidity and temperature data along with NDVI to create a dataset that relates these three variables and determine if humidity and temperature are anomalous with respect
#to certain NDVI values that we will define as anomalies (very low or high NDVI values for a specific park).
#Author: Carlos Hernández Hidalgo
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)
library(plyr)
library(readr)
################################################
ind = dflt$ind   #Measurement index, in this case, NDVI.
mask = dflt$mask #Mask, in this case, "scl_7_8_9".
agg = dflt$agg #"mean"
serie = dflt$serie #type of series, "Verdor"
pkg = dflt$pkg #packet used for clustering, "TSClust"
ini = as.Date("2018-01-13") #dflt$ini #Start date for filtering to gather the data for which we want to perform clustering.
fin = as.Date("2022-02-20") #dflt$fin #End date for filtering the data on which we intend to apply clustering.
aggl = "complete" #Used to execute the clustering.
################################################
#We gather and prepare all humidity and temperature data from different weather stations.
################################################
ini_mediciones = as.Date("2018-01-6")  #Start date for filtering to obtain humidity and temperature data (ten days prior to get the 10 measurements before the first one).
fin_mediciones = as.Date("2021-03-31") #End date for filtering the humidity and temperature data.
################################################
# CA42 Station
################################################
dat_CA42 <- read_csv2(file.path("datos","TempHum","CA42.txt"),col_types = cols(.default = "c"))
#We remove the columns that are not needed.
aux <- dat_CA42 %>%
  dplyr::select(fecha,tmed,hrmed)
#We convert the temperature and humidity data to numerical values. They were in "Name characters".
#We remove the names.
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#We need to convert characters to numbers.
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#We remove the rows that contain NAs (missing values).
aux <- na.omit(aux)
#We group by dates and calculate the mean of the measurements for each day.
aux1 <- ddply(aux, "fecha",colwise(mean))
#We keep only the dates for which we have NDVI data.
TH_CA42 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#CA91 station
################################################
dat_CA91 <- read_csv2(file.path("datos","TempHum","CA91.txt"),col_types = cols(.default = "c"))
#We remove the columns that we don't need.
aux <- dat_CA91 %>%
  dplyr::select(fecha,tmed,hrmed)
#We convert the temperature and humidity data to numerical values. They were initially in the form of "Name characters".
#We remove the names.
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#We need to convert characters to numbers.
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#We remove the rows that contain NAs (missing values).
aux <- na.omit(aux)
#We group by dates and calculate the mean of the measurements for each day.
aux1 <- ddply(aux, "fecha",colwise(mean))
#We keep only the dates for which we have NDVI data.
TH_CA91 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#MO12 station
################################################
dat_MO12 <- read_csv2(file.path("datos","TempHum","MO12.txt"),col_types = cols(.default = "c"))
#We remove the columns that we don't need.
aux <- dat_MO12 %>%
  dplyr::select(fecha,tmed,hrmed)
#We convert the temperature and humidity data to numerical values. They were initially in the form of "Name characters".
#We remove the names.
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#We need to convert characters to numbers.
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#We remove the rows that contain NAs (missing values).
aux <- na.omit(aux)
#We group by dates and calculate the mean of the measurements for each day.
aux1 <- ddply(aux, "fecha",colwise(mean))
#We keep only the dates for which we have NDVI data.
TH_MO12 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#MU21 station
################################################
dat_MU21 <- read_csv2(file.path("datos","TempHum","MU21.txt"),col_types = cols(.default = "c"))
#We remove the columns that we don't need.
aux <- dat_MU21 %>%
  dplyr::select(fecha,tmed,hrmed)
#We convert the temperature and humidity data to numerical values. They were initially in the form of "Name characters".
#We remove the names.
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#We need to convert characters to numbers.
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#We remove the rows that contain NAs (missing values)
aux <- na.omit(aux)
#We group by dates and calculate the mean of the measurements for each day.
aux1 <- ddply(aux, "fecha",colwise(mean))
#We keep only the dates for which we have NDVI data.
TH_MU21 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#MU62 station
################################################
dat_MU62 <- read_csv2(file.path("datos","TempHum","MU62.txt"),col_types = cols(.default = "c"))
#We remove the columns that we don't need.
aux <- dat_MU62 %>%
  dplyr::select(fecha,tmed,hrmed)
#We convert the temperature and humidity data to numerical values. They were initially in the form of "Name characters".
#We remove the names.
names(aux$tmed) <- NULL 
names(aux$hrmed) <- NULL
#We need to convert characters to numbers.
aux$tmed <- as.numeric(aux$tmed)
aux$hrmed <- as.numeric(aux$hrmed)
aux$fecha <- as.Date(aux$fecha, format = "%d/%m/%y")
#We remove the rows that contain NAs (missing values)
aux <- na.omit(aux)
#We group by dates and calculate the mean of the measurements for each day.
aux1 <- ddply(aux, "fecha",colwise(mean))
#We keep only the dates for which we have NDVI data.
TH_MU62 <- aux1 %>%
  dplyr::filter(fecha >= ini_mediciones) %>%
  dplyr::filter(fecha <= fin_mediciones)
################################################
#We prepare the contracts.
################################################
contratos <- dflt$contratos("ConSeries", ind, mask, agg)
################################################
#I obtain the contracts and connections classified according to the nearest weather station.
################################################
estaciones <- DatGeo$leerEstMet() #The weather stations with their locations.
dflt$eemm #The names of the weather stations.
#We obtain the connection number, contract, and nearest or main weather station.
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
#Nos quedamos primero con las 10 primeras mediciones(Las usaremos para comparar los indices de anomalia con las 10 mediciones anteriores para que sea más real.)
ant_CA42 <- TH_CA42[1:10,]
TH_CA42 <- subset(TH_CA42, fecha %in% Date$Fecha)
TH_CA42 <- rbind(ant_CA42, TH_CA42)
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
  #añadimos tambien las anomalias del percentil 20 y 80, para tener un indeice de anomalias distinto
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
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
  #Obtenemos el segundo tipo de anomalía
  anomalia2 <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia2 <- append(anomalia2,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_CA42$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  TH_CA42$anomalia2 <- unlist(anomalia2) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  columna_anomalia2 <- paste("Anomalia2_", contrato,sep = "")
  #las renombramos
  names(TH_CA42)[names(TH_CA42) == "media"] <- columna_media
  names(TH_CA42)[names(TH_CA42) == "anomalia"] <- columna_anomalia
  names(TH_CA42)[names(TH_CA42) == "anomalia2"] <- columna_anomalia2
  
  
}
#Añadimos como columnas las 10 mediciones de temperatura y humedad anteriores a cada fecha
hum1 <- list()
hum2 <- list()
hum3 <- list()
hum4 <- list()
hum5 <- list()
hum6 <- list()
hum7 <- list()
hum8 <- list()
hum9 <- list()
hum10 <- list()
temp1 <- list()
temp2 <- list()
temp3 <- list()
temp4 <- list()
temp5 <- list()
temp6 <- list()
temp7 <- list()
temp8 <- list()
temp9 <- list()
temp10 <- list()

for(i in 1:nrow(TH_CA42)){
  if(i>10){#Las 10 primeras mediciones eran solo para no perder datos
    #Añadimos las 10 mediciones de humedad anteriores
    hum1 <- append(hum1,TH_CA42$hrmed[i-1])
    hum2 <- append(hum2,TH_CA42$hrmed[i-2])
    hum3 <- append(hum3,TH_CA42$hrmed[i-3])
    hum4 <- append(hum4,TH_CA42$hrmed[i-4])
    hum5 <- append(hum5,TH_CA42$hrmed[i-5])
    hum6 <- append(hum6,TH_CA42$hrmed[i-6])
    hum7 <- append(hum7,TH_CA42$hrmed[i-7])
    hum8 <- append(hum8,TH_CA42$hrmed[i-8])
    hum9 <- append(hum9,TH_CA42$hrmed[i-9])
    hum10 <- append(hum10,TH_CA42$hrmed[i-10])
    #Añadimos las 10 mediciones de temperatura anteriores
    temp1 <- append(temp1,TH_CA42$tmed[i-1])
    temp2 <- append(temp2,TH_CA42$tmed[i-2])
    temp3 <- append(temp3,TH_CA42$tmed[i-3])
    temp4 <- append(temp4,TH_CA42$tmed[i-4])
    temp5 <- append(temp5,TH_CA42$tmed[i-5])
    temp6 <- append(temp6,TH_CA42$tmed[i-6])
    temp7 <- append(temp7,TH_CA42$tmed[i-7])
    temp8 <- append(temp8,TH_CA42$tmed[i-8])
    temp9 <- append(temp9,TH_CA42$tmed[i-9])
    temp10 <- append(temp10,TH_CA42$tmed[i-10])
    
  }

}
TH_CA42 <- TH_CA42[11:nrow(TH_CA42),]
#Añadimos las 20 columnas anteriores al dataframe
TH_CA42$temp1 <- unlist(temp1)
TH_CA42$temp2 <- unlist(temp2)
TH_CA42$temp3 <- unlist(temp3)
TH_CA42$temp4 <- unlist(temp4)
TH_CA42$temp5 <- unlist(temp5)
TH_CA42$temp6 <- unlist(temp6)
TH_CA42$temp7 <- unlist(temp7)
TH_CA42$temp8 <- unlist(temp8)
TH_CA42$temp9 <- unlist(temp9)
TH_CA42$temp10 <- unlist(temp10)

TH_CA42$hum1 <- unlist(hum1)
TH_CA42$hum2 <- unlist(hum2)
TH_CA42$hum3 <- unlist(hum3)
TH_CA42$hum4 <- unlist(hum4)
TH_CA42$hum5 <- unlist(hum5)
TH_CA42$hum6 <- unlist(hum6)
TH_CA42$hum7 <- unlist(hum7)
TH_CA42$hum8 <- unlist(hum8)
TH_CA42$hum9 <- unlist(hum9)
TH_CA42$hum10 <- unlist(hum10)
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_temp <- select(TH_CA42, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_CA42$mean_temp_last10 <- mean_diffs
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_hum <- select(TH_CA42, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_CA42$mean_hum_last10 <- mean_diffs
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_CA42,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_CA42$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_CA42$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_CA42$anomaliesAgg2 <-ONES / (ONES+ZEROS)

#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_CA42,contains("Anomalia2"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs
TH_CA42$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#Por ultimo obtenemos el numero de mediciones de NDVI en cada fecha
TH_CA42$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_CA42, "anomaliesCA42.csv", row.names = F, sep=";")
################################################
# Dataset Estación CA91
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
ant_CA91 <- TH_CA91[1:10,]
TH_CA91 <- subset(TH_CA91, fecha %in% Date$Fecha)
TH_CA91 <- rbind(ant_CA91, TH_CA91)
auxx3 <- TH_CA91 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_CA91$Contrato) {
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
  #añadimos tambien las anomalias del percentil 20 y 80, para tener un indeice de anomalias distinto
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
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
  #Obtenemos el segundo tipo de anomalía
  anomalia2 <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia2 <- append(anomalia2,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_CA91$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  TH_CA91$anomalia2 <- unlist(anomalia2) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  columna_anomalia2 <- paste("Anomalia2_", contrato,sep = "")
  #las renombramos
  names(TH_CA91)[names(TH_CA91) == "media"] <- columna_media
  names(TH_CA91)[names(TH_CA91) == "anomalia"] <- columna_anomalia
  names(TH_CA91)[names(TH_CA91) == "anomalia2"] <- columna_anomalia2
  
  
}
#Añadimos como columnas las 10 mediciones de temperatura y humedad anteriores a cada fecha
hum1 <- list()
hum2 <- list()
hum3 <- list()
hum4 <- list()
hum5 <- list()
hum6 <- list()
hum7 <- list()
hum8 <- list()
hum9 <- list()
hum10 <- list()
temp1 <- list()
temp2 <- list()
temp3 <- list()
temp4 <- list()
temp5 <- list()
temp6 <- list()
temp7 <- list()
temp8 <- list()
temp9 <- list()
temp10 <- list()

for(i in 1:nrow(TH_CA91)){
  if(i>10){#Las 10 primeras mediciones eran solo para no perder datos
    #Añadimos las 10 mediciones de humedad anteriores
    hum1 <- append(hum1,TH_CA91$hrmed[i-1])
    hum2 <- append(hum2,TH_CA91$hrmed[i-2])
    hum3 <- append(hum3,TH_CA91$hrmed[i-3])
    hum4 <- append(hum4,TH_CA91$hrmed[i-4])
    hum5 <- append(hum5,TH_CA91$hrmed[i-5])
    hum6 <- append(hum6,TH_CA91$hrmed[i-6])
    hum7 <- append(hum7,TH_CA91$hrmed[i-7])
    hum8 <- append(hum8,TH_CA91$hrmed[i-8])
    hum9 <- append(hum9,TH_CA91$hrmed[i-9])
    hum10 <- append(hum10,TH_CA91$hrmed[i-10])
    #Añadimos las 10 mediciones de temperatura anteriores
    temp1 <- append(temp1,TH_CA91$tmed[i-1])
    temp2 <- append(temp2,TH_CA91$tmed[i-2])
    temp3 <- append(temp3,TH_CA91$tmed[i-3])
    temp4 <- append(temp4,TH_CA91$tmed[i-4])
    temp5 <- append(temp5,TH_CA91$tmed[i-5])
    temp6 <- append(temp6,TH_CA91$tmed[i-6])
    temp7 <- append(temp7,TH_CA91$tmed[i-7])
    temp8 <- append(temp8,TH_CA91$tmed[i-8])
    temp9 <- append(temp9,TH_CA91$tmed[i-9])
    temp10 <- append(temp10,TH_CA91$tmed[i-10])
    
  }
  
}
TH_CA91 <- TH_CA91[11:nrow(TH_CA91),] #Quitamos las 10 filas innecesarias para las que no hay mediciones de NDVI
#Añadimos las 20 columnas anteriores al dataframe
TH_CA91$temp1 <- unlist(temp1)
TH_CA91$temp2 <- unlist(temp2)
TH_CA91$temp3 <- unlist(temp3)
TH_CA91$temp4 <- unlist(temp4)
TH_CA91$temp5 <- unlist(temp5)
TH_CA91$temp6 <- unlist(temp6)
TH_CA91$temp7 <- unlist(temp7)
TH_CA91$temp8 <- unlist(temp8)
TH_CA91$temp9 <- unlist(temp9)
TH_CA91$temp10 <- unlist(temp10)

TH_CA91$hum1 <- unlist(hum1)
TH_CA91$hum2 <- unlist(hum2)
TH_CA91$hum3 <- unlist(hum3)
TH_CA91$hum4 <- unlist(hum4)
TH_CA91$hum5 <- unlist(hum5)
TH_CA91$hum6 <- unlist(hum6)
TH_CA91$hum7 <- unlist(hum7)
TH_CA91$hum8 <- unlist(hum8)
TH_CA91$hum9 <- unlist(hum9)
TH_CA91$hum10 <- unlist(hum10)
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_temp <- select(TH_CA91, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_CA91$mean_temp_last10 <- mean_diffs
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_hum <- select(TH_CA91, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_CA91$mean_hum_last10 <- mean_diffs
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_CA91,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_CA91$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_CA91$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_CA91$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_CA91,contains("Anomalia2"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs
TH_CA91$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#Por ultimo obtenemos el numero de mediciones de NDVI en cada fecha
TH_CA91$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_CA91, "anomaliesCA91.csv", row.names = F, sep=";")
################################################
# Dataset Estación MO12
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
ant_MO12 <- TH_MO12[1:10,]
TH_MO12 <- subset(TH_MO12, fecha %in% Date$Fecha)
TH_MO12 <- rbind(ant_MO12, TH_MO12)
auxx3 <- TH_MO12 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_MO12$Contrato) {
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
  #añadimos tambien las anomalias del percentil 20 y 80, para tener un indeice de anomalias distinto
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
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
  #Obtenemos el segundo tipo de anomalía
  anomalia2 <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia2 <- append(anomalia2,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_MO12$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  TH_MO12$anomalia2 <- unlist(anomalia2) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  columna_anomalia2 <- paste("Anomalia2_", contrato,sep = "")
  #las renombramos
  names(TH_MO12)[names(TH_MO12) == "media"] <- columna_media
  names(TH_MO12)[names(TH_MO12) == "anomalia"] <- columna_anomalia
  names(TH_MO12)[names(TH_MO12) == "anomalia2"] <- columna_anomalia2
  
  
}
#Añadimos como columnas las 10 mediciones de temperatura y humedad anteriores a cada fecha
hum1 <- list()
hum2 <- list()
hum3 <- list()
hum4 <- list()
hum5 <- list()
hum6 <- list()
hum7 <- list()
hum8 <- list()
hum9 <- list()
hum10 <- list()
temp1 <- list()
temp2 <- list()
temp3 <- list()
temp4 <- list()
temp5 <- list()
temp6 <- list()
temp7 <- list()
temp8 <- list()
temp9 <- list()
temp10 <- list()

for(i in 1:nrow(TH_MO12)){
  if(i>10){#Las 10 primeras mediciones eran solo para no perder datos
    #Añadimos las 10 mediciones de humedad anteriores
    hum1 <- append(hum1,TH_MO12$hrmed[i-1])
    hum2 <- append(hum2,TH_MO12$hrmed[i-2])
    hum3 <- append(hum3,TH_MO12$hrmed[i-3])
    hum4 <- append(hum4,TH_MO12$hrmed[i-4])
    hum5 <- append(hum5,TH_MO12$hrmed[i-5])
    hum6 <- append(hum6,TH_MO12$hrmed[i-6])
    hum7 <- append(hum7,TH_MO12$hrmed[i-7])
    hum8 <- append(hum8,TH_MO12$hrmed[i-8])
    hum9 <- append(hum9,TH_MO12$hrmed[i-9])
    hum10 <- append(hum10,TH_MO12$hrmed[i-10])
    #Añadimos las 10 mediciones de temperatura anteriores
    temp1 <- append(temp1,TH_MO12$tmed[i-1])
    temp2 <- append(temp2,TH_MO12$tmed[i-2])
    temp3 <- append(temp3,TH_MO12$tmed[i-3])
    temp4 <- append(temp4,TH_MO12$tmed[i-4])
    temp5 <- append(temp5,TH_MO12$tmed[i-5])
    temp6 <- append(temp6,TH_MO12$tmed[i-6])
    temp7 <- append(temp7,TH_MO12$tmed[i-7])
    temp8 <- append(temp8,TH_MO12$tmed[i-8])
    temp9 <- append(temp9,TH_MO12$tmed[i-9])
    temp10 <- append(temp10,TH_MO12$tmed[i-10])
    
  }
  
}
TH_MO12 <- TH_MO12[11:nrow(TH_MO12),] #Quitamos las 10 filas innecesarias para las que no hay mediciones de NDVI
#Añadimos las 20 columnas anteriores al dataframe
TH_MO12$temp1 <- unlist(temp1)
TH_MO12$temp2 <- unlist(temp2)
TH_MO12$temp3 <- unlist(temp3)
TH_MO12$temp4 <- unlist(temp4)
TH_MO12$temp5 <- unlist(temp5)
TH_MO12$temp6 <- unlist(temp6)
TH_MO12$temp7 <- unlist(temp7)
TH_MO12$temp8 <- unlist(temp8)
TH_MO12$temp9 <- unlist(temp9)
TH_MO12$temp10 <- unlist(temp10)

TH_MO12$hum1 <- unlist(hum1)
TH_MO12$hum2 <- unlist(hum2)
TH_MO12$hum3 <- unlist(hum3)
TH_MO12$hum4 <- unlist(hum4)
TH_MO12$hum5 <- unlist(hum5)
TH_MO12$hum6 <- unlist(hum6)
TH_MO12$hum7 <- unlist(hum7)
TH_MO12$hum8 <- unlist(hum8)
TH_MO12$hum9 <- unlist(hum9)
TH_MO12$hum10 <- unlist(hum10)
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_temp <- select(TH_MO12, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_MO12$mean_temp_last10 <- mean_diffs
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_hum <- select(TH_MO12, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_MO12$mean_hum_last10 <- mean_diffs
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_MO12,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_MO12$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MO12$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MO12$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_MO12,contains("Anomalia2"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs
TH_MO12$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#Por ultimo obtenemos el numero de mediciones de NDVI en cada fecha
TH_MO12$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_MO12, "anomaliesMO12.csv", row.names = F, sep=";")
################################################
# Dataset Estación MU21
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
ant_MU21 <- TH_MU21[1:10,]
TH_MU21 <- subset(TH_MU21, fecha %in% Date$Fecha)
TH_MU21 <- rbind(ant_MU21, TH_MU21)
auxx3 <- TH_MU21 #para usarlo y que al hacer el merge no se acumule
for(contrato in contrato_MU21$Contrato) {
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
  #añadimos tambien las anomalias del percentil 20 y 80, para tener un indeice de anomalias distinto
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
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
  #Obtenemos el segundo tipo de anomalía
  anomalia2 <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia2 <- append(anomalia2,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_MU21$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  TH_MU21$anomalia2 <- unlist(anomalia2) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  columna_anomalia2 <- paste("Anomalia2_", contrato,sep = "")
  #las renombramos
  names(TH_MU21)[names(TH_MU21) == "media"] <- columna_media
  names(TH_MU21)[names(TH_MU21) == "anomalia"] <- columna_anomalia
  names(TH_MU21)[names(TH_MU21) == "anomalia2"] <- columna_anomalia2
  
  
}
#Añadimos como columnas las 10 mediciones de temperatura y humedad anteriores a cada fecha
hum1 <- list()
hum2 <- list()
hum3 <- list()
hum4 <- list()
hum5 <- list()
hum6 <- list()
hum7 <- list()
hum8 <- list()
hum9 <- list()
hum10 <- list()
temp1 <- list()
temp2 <- list()
temp3 <- list()
temp4 <- list()
temp5 <- list()
temp6 <- list()
temp7 <- list()
temp8 <- list()
temp9 <- list()
temp10 <- list()

for(i in 1:nrow(TH_MU21)){
  if(i>10){#Las 10 primeras mediciones eran solo para no perder datos
    #Añadimos las 10 mediciones de humedad anteriores
    hum1 <- append(hum1,TH_MU21$hrmed[i-1])
    hum2 <- append(hum2,TH_MU21$hrmed[i-2])
    hum3 <- append(hum3,TH_MU21$hrmed[i-3])
    hum4 <- append(hum4,TH_MU21$hrmed[i-4])
    hum5 <- append(hum5,TH_MU21$hrmed[i-5])
    hum6 <- append(hum6,TH_MU21$hrmed[i-6])
    hum7 <- append(hum7,TH_MU21$hrmed[i-7])
    hum8 <- append(hum8,TH_MU21$hrmed[i-8])
    hum9 <- append(hum9,TH_MU21$hrmed[i-9])
    hum10 <- append(hum10,TH_MU21$hrmed[i-10])
    #Añadimos las 10 mediciones de temperatura anteriores
    temp1 <- append(temp1,TH_MU21$tmed[i-1])
    temp2 <- append(temp2,TH_MU21$tmed[i-2])
    temp3 <- append(temp3,TH_MU21$tmed[i-3])
    temp4 <- append(temp4,TH_MU21$tmed[i-4])
    temp5 <- append(temp5,TH_MU21$tmed[i-5])
    temp6 <- append(temp6,TH_MU21$tmed[i-6])
    temp7 <- append(temp7,TH_MU21$tmed[i-7])
    temp8 <- append(temp8,TH_MU21$tmed[i-8])
    temp9 <- append(temp9,TH_MU21$tmed[i-9])
    temp10 <- append(temp10,TH_MU21$tmed[i-10])
    
  }
  
}
TH_MU21 <- TH_MU21[11:nrow(TH_MU21),] #Quitamos las 10 filas innecesarias para las que no hay mediciones de NDVI
#Añadimos las 20 columnas anteriores al dataframe
TH_MU21$temp1 <- unlist(temp1)
TH_MU21$temp2 <- unlist(temp2)
TH_MU21$temp3 <- unlist(temp3)
TH_MU21$temp4 <- unlist(temp4)
TH_MU21$temp5 <- unlist(temp5)
TH_MU21$temp6 <- unlist(temp6)
TH_MU21$temp7 <- unlist(temp7)
TH_MU21$temp8 <- unlist(temp8)
TH_MU21$temp9 <- unlist(temp9)
TH_MU21$temp10 <- unlist(temp10)

TH_MU21$hum1 <- unlist(hum1)
TH_MU21$hum2 <- unlist(hum2)
TH_MU21$hum3 <- unlist(hum3)
TH_MU21$hum4 <- unlist(hum4)
TH_MU21$hum5 <- unlist(hum5)
TH_MU21$hum6 <- unlist(hum6)
TH_MU21$hum7 <- unlist(hum7)
TH_MU21$hum8 <- unlist(hum8)
TH_MU21$hum9 <- unlist(hum9)
TH_MU21$hum10 <- unlist(hum10)
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_temp <- select(TH_MU21, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_MU21$mean_temp_last10 <- mean_diffs
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_hum <- select(TH_MU21, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_MU21$mean_hum_last10 <- mean_diffs
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_MU21,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_MU21$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MU21$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MU21$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_MU21,contains("Anomalia2"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs
TH_MU21$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#Por ultimo obtenemos el numero de mediciones de NDVI en cada fecha
TH_MU21$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_MU21, "anomaliesMU21.csv", row.names = F, sep=";")
################################################
# Dataset Estación MU62
################################################
################################################
#Añadiendo los valores de temperatura y humedad de la estación, quitando las fechas para las que no tenemos mediciones de NVDI
################################################
ant_MU62 <- TH_MU62[1:10,]
TH_MU62 <- subset(TH_MU62, fecha %in% Date$Fecha)
TH_MU62 <- rbind(ant_MU62, TH_MU62)
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
  #añadimos tambien las anomalias del percentil 20 y 80, para tener un indeice de anomalias distinto
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
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
  #Obtenemos el segundo tipo de anomalía
  anomalia2 <- list()
  #para cada una de las mediciones de NVDI de cada contrato determinamos si es una medición anómala o no (si pertenece al percentil 90 o al 10)
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #si pertenece al percentil 90 o al 10, entonces es un valor anómalo
        anomalia2 <- append(anomalia2,1)
      }
      else{ #en otro caso no es un valor de NVDI anómalo
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_MU62$anomalia <- unlist(anomalia) #añadimos las anomalías al dataset
  TH_MU62$anomalia2 <- unlist(anomalia2) #añadimos las anomalías al dataset
  #Para renombrar correctamente las columnas añadidas
  columna_media <- paste("NDVI_mean_",contrato,sep = "")
  columna_anomalia <- paste("Anomaly_", contrato,sep = "")
  columna_anomalia2 <- paste("Anomalia2_", contrato,sep = "")
  #las renombramos
  names(TH_MU62)[names(TH_MU62) == "media"] <- columna_media
  names(TH_MU62)[names(TH_MU62) == "anomalia"] <- columna_anomalia
  names(TH_MU62)[names(TH_MU62) == "anomalia2"] <- columna_anomalia2
  
  
}
#Añadimos como columnas las 10 mediciones de temperatura y humedad anteriores a cada fecha
hum1 <- list()
hum2 <- list()
hum3 <- list()
hum4 <- list()
hum5 <- list()
hum6 <- list()
hum7 <- list()
hum8 <- list()
hum9 <- list()
hum10 <- list()
temp1 <- list()
temp2 <- list()
temp3 <- list()
temp4 <- list()
temp5 <- list()
temp6 <- list()
temp7 <- list()
temp8 <- list()
temp9 <- list()
temp10 <- list()

for(i in 1:nrow(TH_MU62)){
  if(i>10){#Las 10 primeras mediciones eran solo para no perder datos
    #Añadimos las 10 mediciones de humedad anteriores
    hum1 <- append(hum1,TH_MU62$hrmed[i-1])
    hum2 <- append(hum2,TH_MU62$hrmed[i-2])
    hum3 <- append(hum3,TH_MU62$hrmed[i-3])
    hum4 <- append(hum4,TH_MU62$hrmed[i-4])
    hum5 <- append(hum5,TH_MU62$hrmed[i-5])
    hum6 <- append(hum6,TH_MU62$hrmed[i-6])
    hum7 <- append(hum7,TH_MU62$hrmed[i-7])
    hum8 <- append(hum8,TH_MU62$hrmed[i-8])
    hum9 <- append(hum9,TH_MU62$hrmed[i-9])
    hum10 <- append(hum10,TH_MU62$hrmed[i-10])
    #Añadimos las 10 mediciones de temperatura anteriores
    temp1 <- append(temp1,TH_MU62$tmed[i-1])
    temp2 <- append(temp2,TH_MU62$tmed[i-2])
    temp3 <- append(temp3,TH_MU62$tmed[i-3])
    temp4 <- append(temp4,TH_MU62$tmed[i-4])
    temp5 <- append(temp5,TH_MU62$tmed[i-5])
    temp6 <- append(temp6,TH_MU62$tmed[i-6])
    temp7 <- append(temp7,TH_MU62$tmed[i-7])
    temp8 <- append(temp8,TH_MU62$tmed[i-8])
    temp9 <- append(temp9,TH_MU62$tmed[i-9])
    temp10 <- append(temp10,TH_MU62$tmed[i-10])
    
  }
  
}
TH_MU62 <- TH_MU62[11:nrow(TH_MU62),] #Quitamos las 10 filas innecesarias para las que no hay mediciones de NDVI
#Añadimos las 20 columnas anteriores al dataframe
TH_MU62$temp1 <- unlist(temp1)
TH_MU62$temp2 <- unlist(temp2)
TH_MU62$temp3 <- unlist(temp3)
TH_MU62$temp4 <- unlist(temp4)
TH_MU62$temp5 <- unlist(temp5)
TH_MU62$temp6 <- unlist(temp6)
TH_MU62$temp7 <- unlist(temp7)
TH_MU62$temp8 <- unlist(temp8)
TH_MU62$temp9 <- unlist(temp9)
TH_MU62$temp10 <- unlist(temp10)

TH_MU62$hum1 <- unlist(hum1)
TH_MU62$hum2 <- unlist(hum2)
TH_MU62$hum3 <- unlist(hum3)
TH_MU62$hum4 <- unlist(hum4)
TH_MU62$hum5 <- unlist(hum5)
TH_MU62$hum6 <- unlist(hum6)
TH_MU62$hum7 <- unlist(hum7)
TH_MU62$hum8 <- unlist(hum8)
TH_MU62$hum9 <- unlist(hum9)
TH_MU62$hum10 <- unlist(hum10)
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_temp <- select(TH_MU62, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_MU62$mean_temp_last10 <- mean_diffs
#Calculamos ahora una columna que contenga la media de las diferencias de temperatura de un día con su siguiente de los últimos diez días (respecto a la fecha de la fila)
med_hum <- select(TH_MU62, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# calcular las diferencias absolutas entre las columnas
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# calcular la media de las diferencias absolutas de cada par de columnas
mean_diffs <- rowMeans(abs_diffs)
TH_MU62$mean_hum_last10 <- mean_diffs
#Calcular columna con moda de anomalías para cada fecha
#seleccionamos las columnas que empiezan por anomaly
anomalies_df <- dplyr::select(TH_MU62,contains("Anomaly"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs

#TH_MU62$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MU62$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MU62$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_MU62,contains("Anomalia2"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # numero de ceros
ONES = rowSums(anomalies_df, na.rm=T)        # numero de unos
NAS = rowSums(is.na(anomalies_df))          # numero de NAs
TH_MU62$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#Por ultimo obtenemos el numero de mediciones de NDVI en cada fecha
TH_MU62$n_mediciones <- (ONES+ZEROS)
write.table(x = TH_MU62, "anomaliesMU62.csv", row.names = F, sep=";")
