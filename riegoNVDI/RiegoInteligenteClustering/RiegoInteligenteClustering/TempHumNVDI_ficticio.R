#Script para preparación de los datos de humedad y temperatura junto con el NVDI para tener un dataset que relacione estas tres variables y determinar si además la humedad
#y temperatura son anómalas respecto a ciertos valores del NVDI que determinaremos como anómalos (valores de NVDI muy bajos o altos para un parque en concreto)
#usando datos de temperatura y humedad y anomalía ficticios
#Author: Carlos Hernández Hidalgo
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)
################################################
ind = dflt$ind   #índice de medición, en este caso NDVI 
mask = dflt$mask #máscara, en este caso "scl_7_8_9"
agg = dflt$agg #"mean"
serie = dflt$serie #tipo de serie, "Verdor"
pkg = dflt$pkg #paquete usado para clustering, "TSClust"
ini = as.Date("2010-01-13") #dflt$ini #Fecha de inicio para filtrar a partir de ahí los datos sobre los que queremos hacer clustering
fin = as.Date("2024-02-20") #dflt$fin #Fecha de fin para filtrar los datos sobre los que queremos aplicar clustering
aggl = "complete" #usado para ejecutar el clustering
################################################
#Preparamos los contratos
################################################
contratos <- dflt$contratos("ConSeries", ind, mask, agg)
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
ccontrato <-
  listDF2DF(S) %>%
  dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>%
   dplyr::select(Contrato) %>% unique()
################################################
#Ponemos valores aleatorios de temperatura y humedad, ficticios
################################################
set.seed(0717)
Humidity <- rbinom(nrow(Date), 30, 0.5)
Temperature <- rbinom(nrow(Date), 40, 0.5)
TempHumNDVI <- cbind(Date, Humidity, Temperature)
#Para cada uno de los parques vamos añadiendo al dataset la media de NVDI para ese día y si presenta anomalía o no con el formato:
#NVDI_mean_numeroContrato 
#Anomalia_numeroContrato
n_contratos_erroneos=0
for(contrato in contratos) {
 # print(contrato)
  
  
  #Creamos las anomalias de forma aleatoria por ahora
  anomalia <- rbinom(nrow(Date), 1, 0.5)
  #Obtenemos las medias de NVDI de ese parque
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>%
    dplyr::filter(Contrato == contrato) %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  #añadimos las dos nuevas columnas a nuestro dataset
  if(nrow(media)==nrow(Date)){ #Quitando los contratos que tengan las medias de NVDI mal añadidas (algunos tienen menos columnas de las necesarias, sin valores de NA)
    TempHumNDVI$media <- media
    TempHumNDVI$anomalia <- anomalia
    #Para renombrar correctamente las columnas añadidas
    columna_media <- paste("NVDI_mean_",contrato,sep = "")
    columna_anomalia <- paste("Anomaly_", contrato,sep = "")
    #las renombramos
    names(TempHumNDVI)[names(TempHumNDVI) == "media"] <- columna_media
    names(TempHumNDVI)[names(TempHumNDVI) == "anomalia"] <- columna_anomalia
  }
  else{
    n_contratos_erroneos=n_contratos_erroneos+1
  }
  
  
}