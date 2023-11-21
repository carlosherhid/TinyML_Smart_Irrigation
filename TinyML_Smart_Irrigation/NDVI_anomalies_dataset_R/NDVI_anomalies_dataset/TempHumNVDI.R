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
contrato_ca42 <- data.frame(contrato_ca42[[1]],contrato_ca42[[2]], contrato_ca42[[3]]) #We removed the "geometry" column.
colnames(contrato_ca42) <- c("Acometida","Contrato","Estacion") #We changed the names of the columns.
#write.csv(contrato_ca42, file = "contrato_ca42.csv", row.names = TRUE)
contrato_CA91 <- sg%>%
  dplyr::filter(Principal == dflt$eemm[2]) %>% na.omit()
contrato_CA91 <- data.frame(contrato_CA91[[1]],contrato_CA91[[2]], contrato_CA91[[3]]) #We removed the "geometry" column.
colnames(contrato_CA91) <- c("Acometida","Contrato","Estacion") #We changed the names of the columns.
#write.csv(contrato_CA91, file = "contrato_CA91.csv", row.names = TRUE)
contrato_MO12 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[3]) %>% na.omit()
contrato_MO12 <- data.frame(contrato_MO12[[1]],contrato_MO12[[2]], contrato_MO12[[3]]) #We removed the "geometry" column.
colnames(contrato_MO12) <- c("Acometida","Contrato","Estacion") #We changed the names of the columns.
#write.csv(contrato_MO12, file = "contrato_MO12.csv", row.names = TRUE)
contrato_MU21 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[4]) %>% na.omit()
contrato_MU21 <- data.frame(contrato_MU21[[1]],contrato_MU21[[2]], contrato_MU21[[3]]) #We removed the "geometry" column.
colnames(contrato_MU21) <- c("Acometida","Contrato","Estacion") #We changed the names of the columns.
#write.csv(contrato_MU21, file = "contrato_MU21.csv", row.names = TRUE)
contrato_MU62 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[5]) %>% na.omit()
contrato_MU62 <- data.frame(contrato_MU62[[1]],contrato_MU62[[2]], contrato_MU62[[3]]) #Eliminamos la columna "geometry"
colnames(contrato_MU62) <- c("Acometida","Contrato","Estacion") #We changed the names of the columns.
#write.csv(contrato_MU62, file = "contrato_MU62.csv", row.names = TRUE)
################################################
#We prepare S, which will be the initial NDVI series.
#Each park is identified by a contract number.
#For each park, we have the following data:
#   -Fecha: Day on which the NDVI measurements were taken, measurements are taken every 5 days.
#   -NDVI.scl_7_8_9.Min: The minimum NDVI value for that day.
#   -NDVI.scl_7_8_9.Q1: The first quartile of the NDVI values.
#   -NDVI.scl_7_8_9.Median: The median of the NDVI values.
#   -NDVI.scl_7_8_9.Mean: The mean of the NDVI values.
#   -NDVI.scl_7_8_9.Q3: The third quartile of the NDVI values.
#   -NDVI.scl_7_8_9.Max: The maximum NDVI value for that day.
#   -NDVI.scl_7_8_9.n: The number of NDVI measurements taken on that day for that park.
################################################
S <- Verdor$readSumInd(contratos, ind, mask, ini = ini, fin = fin)
################################################
#We obtain the dates.
################################################
Date <-
  listDF2DF(S) %>%
  dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>%
  filter(!is.na(NDVI.scl_7_8_9.Mean)) %>%
  select(Fecha)
#For each of the parks, we add to the dataset the mean NDVI for that day and whether it exhibits an anomaly or not, in the format:
#NVDI_mean_contractNumber
#Anomalie_contractNumber
#We will do this for each of the weather stations. This way, we will have a separate dataset for each station.
#With the collected temperatures and humidities, the contracts, the dates, and a summary of anomalies for all the parks on each date.
#It will be determined whether an NDVI measurement is anomalous or not by comparing it to the mean NDVI of all measurements for each park.
#If it belongs to the 90th or 10th percentile, then it will be considered anomalous.
################################################
# Dataset CA42 Station
################################################
################################################
#Adding the temperature and humidity values from the station, excluding the dates for which we don't have NDVI measurements.
################################################
#We will initially focus on the first 10 measurements (these will be used to compare anomaly indices with the previous 10 measurements for a more realistic assessment).
ant_CA42 <- TH_CA42[1:10,]
TH_CA42 <- subset(TH_CA42, fecha %in% Date$Fecha)
TH_CA42 <- rbind(ant_CA42, TH_CA42)
auxx3 <- TH_CA42 #To use this and prevent accumulation during merging.
id <- 0
for(contrato in contrato_ca42$Contrato) {
  # print(contrato)
  id <- id + 1
  #We obtain the mean NDVI values for that park.
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_CA42$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #We obtain the two values that will indicate whether the NDVI values have been anomalous or not.
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #We also add anomalies for the 20th and 80th percentiles to have a different anomaly index.
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
  #We add the two new columns to our dataset.
  ######################################################################################
  #EVERY PERCENTILE
  ######################################################################################
  # Vector with the desired percentiles
  percentiles <- c(seq(0.98, 0.6, -0.01), seq(0.02, 0.4, 0.01))
  
  # Vector to store the results
  percentile_values <- vector("numeric", length(percentiles))
  
  # Data
  data <- media$NDVI.scl_7_8_9.Mean
  
  # Calculate the percentiles and store the values in the vector
  for (i in 1:length(percentiles)) {
    percentile_values[i] <- quantile(x = data, probs = percentiles[i], na.rm = TRUE)
    
  }
  
  # Split the percentiles into two sets
  percentile_98_to_60 <- percentile_values[percentiles >= 0.6 & percentiles <= 0.98]
  percentile_2_to_40 <- percentile_values[percentiles >= 0.02 & percentiles <= 0.4]
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  anomalias <- matrix(0, ncol = length(percentile_98_to_60), nrow = nrow(x))
  
  for (i in 1:length(percentile_98_to_60)) {
    med <- x$NDVI.scl_7_8_9.Mean
    anomalous <- (med >= percentile_98_to_60[i] | med <= percentile_2_to_40[i])
    
    # Assign values directly to the corresponding column in the matrix
    anomalias[, i] <- ifelse(is.na(med), NA, anomalous)
  }
  
  colnames(anomalias) <- paste("Anomaly_", 1:length(percentile_98_to_60), "_", id, sep = "")
  
  #####################################################################################
  #####################################################################################
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_CA42$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #For each of the NDVI measurements for each contract, we determine whether it is an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia <- append(anomalia,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia <-append(anomalia,0)
      }
    }
  }
  #We obtain the second type of anomaly.
  anomalia2 <- list()
  #For each of the NDVI measurements for each contract, we determine whether it is an anomalous measurement or not (if it belongs to the 80th or 20th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #If it belongs to the 80th or 20th percentile, then it is an anomalous value.
        anomalia2 <- append(anomalia2,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_CA42$anomalia <- unlist(anomalia) #We add the anomalies to the dataset.
  TH_CA42$anomalia2 <- unlist(anomalia2) #We add the anomalies to the dataset.
  #To correctly rename the added columns.
  columna_media <- paste("NDVI_mean_",id,sep = "")
  columna_anomalia <- paste("Anomaly_a_", id,sep = "")
  columna_anomalia2 <- paste("Anomaly_b_", id,sep = "")
  #we rename it.
  names(TH_CA42)[names(TH_CA42) == "media"] <- columna_media
  names(TH_CA42)[names(TH_CA42) == "anomalia"] <- columna_anomalia
  names(TH_CA42)[names(TH_CA42) == "anomalia2"] <- columna_anomalia2
  # Convert the matrix to a data frame
  anomalias_df <- as.data.frame(anomalias)
  
  # Add the columns to the dataset
  TH_CA42 <- cbind(TH_CA42, anomalias_df)
  
}
#We add the previous 10 temperature and humidity measurements as columns for each date.
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
  if(i>10){#The first 10 measurements were only used to retain data.
    #We add the previous 10 humidity measurements.
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
    #We add the previous 10 temperature measurements.
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
#We add the 20 previous columns to the dataframe.
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
#Now we calculate a column containing the mean of temperature differences between a day and its following day over the last ten days (relative to the date in the row).
med_temp <- select(TH_CA42, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences for each pair of columns.
mean_diffs <- rowMeans(abs_diffs)
TH_CA42$mean_temp_last10 <- mean_diffs
#Now we calculate a column containing the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_hum <- select(TH_CA42, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences for each pair of columns.
mean_diffs <- rowMeans(abs_diffs)
TH_CA42$mean_hum_last10 <- mean_diffs
#Calculate a column with the mode of anomalies for each date.
#We select the columns that start with "anomaly".
anomalies_df <- dplyr::select(TH_CA42,contains("Anomaly_a"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of unos
NAS = rowSums(is.na(anomalies_df))          # number of NAs

#TH_CA42$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_CA42$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_CA42$anomaliesAgg2 <-ONES / (ONES+ZEROS)

#Now we calculate the anomaly index for the 80th and 20th percentiles, named "anomaliesAgg3".
anomalies_df <- dplyr::select(TH_CA42,contains("Anomaly_b"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of unos
NAS = rowSums(is.na(anomalies_df))          # number of NAs
TH_CA42$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#We calculated it for all anomalies
anomaly_columns <- colnames(TH_CA42)[grepl("Anomaly_", colnames(TH_CA42))]

for (i in 1:length(percentile_98_to_60)) {
  col_name <- paste("Anomaly_", i, sep = "")
  agg_column_name <- paste(col_name, "Agg", sep = "")
  
  anomalies_df <- dplyr::select(TH_CA42, contains(col_name))
  
  ZEROS <- rowSums(anomalies_df == 0, na.rm = TRUE)
  ONES <- rowSums(anomalies_df, na.rm = TRUE)
  NAS <- rowSums(is.na(anomalies_df))
  
  TH_CA42[[agg_column_name]] <- ONES / (ONES + ZEROS + NAS)
}
#Finally, we obtain the number of NDVI measurements for each date.
TH_CA42$n_measurements <- (ONES+ZEROS)
colnames(TH_CA42)[1] <- "date"
write.table(x = TH_CA42, "anomaliesCA42.csv", row.names = F, sep=";")
################################################
# Dataset CA91 Station
################################################
################################################
#Adding the temperature and humidity values from the station, excluding the dates for which we don't have NDVI measurements.
################################################
ant_CA91 <- TH_CA91[1:10,]
TH_CA91 <- subset(TH_CA91, fecha %in% Date$Fecha)
TH_CA91 <- rbind(ant_CA91, TH_CA91)
auxx3 <- TH_CA91 #To use it and prevent accumulation during the merge.
id <- 0
for(contrato in contrato_CA91$Contrato) {
  # print(contrato)
  id <- id + 1
  #We obtain the mean NDVI values for that park.
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_CA91$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #We obtain the two values that will indicate whether the NDVI values have been anomalous or not.
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #We also add the anomalies for the 20th and 80th percentiles to have a different anomaly index.
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
  #We add the two new columns to our dataset.
  ######################################################################################
  #EVERY PERCENTILE
  ######################################################################################
  # Vector with the desired percentiles
  percentiles <- c(seq(0.98, 0.6, -0.01), seq(0.02, 0.4, 0.01))
  
  # Vector to store the results
  percentile_values <- vector("numeric", length(percentiles))
  
  # Data
  data <- media$NDVI.scl_7_8_9.Mean
  
  # Calculate the percentiles and store the values in the vector
  for (i in 1:length(percentiles)) {
    percentile_values[i] <- quantile(x = data, probs = percentiles[i], na.rm = TRUE)
    
  }
  
  # Split the percentiles into two sets
  percentile_98_to_60 <- percentile_values[percentiles >= 0.6 & percentiles <= 0.98]
  percentile_2_to_40 <- percentile_values[percentiles >= 0.02 & percentiles <= 0.4]
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  anomalias <- matrix(0, ncol = length(percentile_98_to_60), nrow = nrow(x))
  
  for (i in 1:length(percentile_98_to_60)) {
    med <- x$NDVI.scl_7_8_9.Mean
    anomalous <- (med >= percentile_98_to_60[i] | med <= percentile_2_to_40[i])
    
    # Assign values directly to the corresponding column in the matrix
    anomalias[, i] <- ifelse(is.na(med), NA, anomalous)
  }
  
  colnames(anomalias) <- paste("Anomaly_", 1:length(percentile_98_to_60), "_", id, sep = "")
  
  #####################################################################################
  #####################################################################################
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_CA91$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia <- append(anomalia,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia <-append(anomalia,0)
      }
    }
  }
  #We obtain the second type of anomaly.
  anomalia2 <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia2 <- append(anomalia2,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_CA91$anomalia <- unlist(anomalia) #We add the anomalies to the dataset.
  TH_CA91$anomalia2 <- unlist(anomalia2) #We add the anomalies to the dataset.
  #To rename the added columns correctly.
  columna_media <- paste("NDVI_mean_",id,sep = "")
  columna_anomalia <- paste("Anomaly_a_", id,sep = "")
  columna_anomalia2 <- paste("Anomaly_b_", id,sep = "")
  #we rename them
  names(TH_CA91)[names(TH_CA91) == "media"] <- columna_media
  names(TH_CA91)[names(TH_CA91) == "anomalia"] <- columna_anomalia
  names(TH_CA91)[names(TH_CA91) == "anomalia2"] <- columna_anomalia2
  # Convert the matrix to a data frame
  anomalias_df <- as.data.frame(anomalias)
  
  # Add the columns to the dataset
  TH_CA91 <- cbind(TH_CA91, anomalias_df)
  
}
#We add the previous 10 temperature and humidity measurements as columns for each date.
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
  if(i>10){#The first 10 measurements were only used to retain data.
    #We add the previous 10 humidity measurements.
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
    #We add the previous 10 temperature measurements.
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
TH_CA91 <- TH_CA91[11:nrow(TH_CA91),] #We remove the 10 unnecessary rows for which there are no NDVI measurements.
#We add the previous 20 columns to the dataframe.
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
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_temp <- select(TH_CA91, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_CA91$mean_temp_last10 <- mean_diffs
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_hum <- select(TH_CA91, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_CA91$mean_hum_last10 <- mean_diffs
#Calculate column with mode of anomalies for each date.
#Select columns that start with 'anomaly'
anomalies_df <- dplyr::select(TH_CA91,contains("Anomaly_a"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs

#TH_CA91$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_CA91$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_CA91$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_CA91,contains("Anomaly_b"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs
TH_CA91$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#We calculated it for all anomalies
anomaly_columns <- colnames(TH_CA91)[grepl("Anomaly_", colnames(TH_CA91))]

for (i in 1:length(percentile_98_to_60)) {
  col_name <- paste("Anomaly_", i, sep = "")
  agg_column_name <- paste(col_name, "Agg", sep = "")
  
  anomalies_df <- dplyr::select(TH_CA91, contains(col_name))
  
  ZEROS <- rowSums(anomalies_df == 0, na.rm = TRUE)
  ONES <- rowSums(anomalies_df, na.rm = TRUE)
  NAS <- rowSums(is.na(anomalies_df))
  
  TH_CA91[[agg_column_name]] <- ONES / (ONES + ZEROS + NAS)
}
#Finally, we obtain the number of NDVI measurements for each date
TH_CA91$n_measurements <- (ONES+ZEROS)
colnames(TH_CA91)[1] <- "date"
write.table(x = TH_CA91, "anomaliesCA91.csv", row.names = F, sep=";")
################################################
# Dataset Estación MO12
################################################
################################################
#Adding the temperature and humidity values from the station, excluding the dates for which we don't have NDVI measurements.
################################################
ant_MO12 <- TH_MO12[1:10,]
TH_MO12 <- subset(TH_MO12, fecha %in% Date$Fecha)
TH_MO12 <- rbind(ant_MO12, TH_MO12)
auxx3 <- TH_MO12 #To use it and prevent accumulation during the merge.
id <- 0
for(contrato in contrato_MO12$Contrato) {
  # print(contrato)
  id <- id + 1
  #We obtain the mean NDVI values for that park.
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_MO12$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #We obtain the two values that will indicate whether the NDVI values have been anomalous or not.
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #We also add the anomalies for the 20th and 80th percentiles to have a different anomaly index.
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
  #We add the two new columns to our dataset.
  ######################################################################################
  #EVERY PERCENTILE
  ######################################################################################
  # Vector with the desired percentiles
  percentiles <- c(seq(0.98, 0.6, -0.01), seq(0.02, 0.4, 0.01))
  
  # Vector to store the results
  percentile_values <- vector("numeric", length(percentiles))
  
  # Data
  data <- media$NDVI.scl_7_8_9.Mean
  
  # Calculate the percentiles and store the values in the vector
  for (i in 1:length(percentiles)) {
    percentile_values[i] <- quantile(x = data, probs = percentiles[i], na.rm = TRUE)
    
  }
  
  # Split the percentiles into two sets
  percentile_98_to_60 <- percentile_values[percentiles >= 0.6 & percentiles <= 0.98]
  percentile_2_to_40 <- percentile_values[percentiles >= 0.02 & percentiles <= 0.4]
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  anomalias <- matrix(0, ncol = length(percentile_98_to_60), nrow = nrow(x))
  
  for (i in 1:length(percentile_98_to_60)) {
    med <- x$NDVI.scl_7_8_9.Mean
    anomalous <- (med >= percentile_98_to_60[i] | med <= percentile_2_to_40[i])
    
    # Assign values directly to the corresponding column in the matrix
    anomalias[, i] <- ifelse(is.na(med), NA, anomalous)
  }
  
  colnames(anomalias) <- paste("Anomaly_", 1:length(percentile_98_to_60), "_", id, sep = "")
  
  #####################################################################################
  #####################################################################################
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_MO12$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia <- append(anomalia,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia <-append(anomalia,0)
      }
    }
  }
  #We obtain the second type of anomaly.
  anomalia2 <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia2 <- append(anomalia2,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_MO12$anomalia <- unlist(anomalia) #We add the anomalies to the dataset.
  TH_MO12$anomalia2 <- unlist(anomalia2) #We add the anomalies to the dataset.
  #To rename the added columns correctly.
  columna_media <- paste("NDVI_mean_",id,sep = "")
  columna_anomalia <- paste("Anomaly_a_", id,sep = "")
  columna_anomalia2 <- paste("Anomaly_b_", id,sep = "")
  #we rename them
  names(TH_MO12)[names(TH_MO12) == "media"] <- columna_media
  names(TH_MO12)[names(TH_MO12) == "anomalia"] <- columna_anomalia
  names(TH_MO12)[names(TH_MO12) == "anomalia2"] <- columna_anomalia2
  # Convert the matrix to a data frame
  anomalias_df <- as.data.frame(anomalias)
  
  # Add the columns to the dataset
  TH_MO12 <- cbind(TH_MO12, anomalias_df)
}
#We add the previous 10 temperature and humidity measurements as columns for each date.
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
  if(i>10){#The first 10 measurements were only used to retain data.
    #We add the previous 10 humidity measurements.
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
    #We add the previous 10 temperature measurements.
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
TH_MO12 <- TH_MO12[11:nrow(TH_MO12),] #We remove the 10 unnecessary rows for which there are no NDVI measurements.
#We add the previous 20 columns to the dataframe.
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
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_temp <- select(TH_MO12, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_MO12$mean_temp_last10 <- mean_diffs
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_hum <- select(TH_MO12, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_MO12$mean_hum_last10 <- mean_diffs
#Calculate column with mode of anomalies for each date.
#Select columns that start with 'anomaly'
anomalies_df <- dplyr::select(TH_MO12,contains("Anomaly_a"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs

#TH_MO12$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MO12$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MO12$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_MO12,contains("Anomaly_b"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs
TH_MO12$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#We calculated it for all anomalies
anomaly_columns <- colnames(TH_MO12)[grepl("Anomaly_", colnames(TH_MO12))]

for (i in 1:length(percentile_98_to_60)) {
  col_name <- paste("Anomaly_", i, sep = "")
  agg_column_name <- paste(col_name, "Agg", sep = "")
  
  anomalies_df <- dplyr::select(TH_MO12, contains(col_name))
  
  ZEROS <- rowSums(anomalies_df == 0, na.rm = TRUE)
  ONES <- rowSums(anomalies_df, na.rm = TRUE)
  NAS <- rowSums(is.na(anomalies_df))
  
  TH_MO12[[agg_column_name]] <- ONES / (ONES + ZEROS + NAS)
}
#Finally, we obtain the number of NDVI measurements for each date
TH_MO12$n_measurements <- (ONES+ZEROS)
colnames(TH_MO12)[1] <- "date"
write.table(x = TH_MO12, "anomaliesMO12.csv", row.names = F, sep=";")
################################################
# Dataset Estación MU21
################################################
################################################
#Adding the temperature and humidity values from the station, excluding the dates for which we don't have NDVI measurements.
################################################
ant_MU21 <- TH_MU21[1:10,]
TH_MU21 <- subset(TH_MU21, fecha %in% Date$Fecha)
TH_MU21 <- rbind(ant_MU21, TH_MU21)
auxx3 <- TH_MU21 #To use it and prevent accumulation during the merge.
id <- 0
for(contrato in contrato_MU21$Contrato) {
  # print(contrato)
  id <- id + 1
  #We obtain the mean NDVI values for that park.
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_MU21$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #We obtain the two values that will indicate whether the NDVI values have been anomalous or not.
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #We also add the anomalies for the 20th and 80th percentiles to have a different anomaly index.
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
  #We add the two new columns to our dataset.
  ######################################################################################
  #EVERY PERCENTILE
  ######################################################################################
  # Vector with the desired percentiles
  percentiles <- c(seq(0.98, 0.6, -0.01), seq(0.02, 0.4, 0.01))
  
  # Vector to store the results
  percentile_values <- vector("numeric", length(percentiles))
  
  # Data
  data <- media$NDVI.scl_7_8_9.Mean
  
  # Calculate the percentiles and store the values in the vector
  for (i in 1:length(percentiles)) {
    percentile_values[i] <- quantile(x = data, probs = percentiles[i], na.rm = TRUE)
    
  }
  
  # Split the percentiles into two sets
  percentile_98_to_60 <- percentile_values[percentiles >= 0.6 & percentiles <= 0.98]
  percentile_2_to_40 <- percentile_values[percentiles >= 0.02 & percentiles <= 0.4]
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  anomalias <- matrix(0, ncol = length(percentile_98_to_60), nrow = nrow(x))
  
  for (i in 1:length(percentile_98_to_60)) {
    med <- x$NDVI.scl_7_8_9.Mean
    anomalous <- (med >= percentile_98_to_60[i] | med <= percentile_2_to_40[i])
    
    # Assign values directly to the corresponding column in the matrix
    anomalias[, i] <- ifelse(is.na(med), NA, anomalous)
  }
  
  colnames(anomalias) <- paste("Anomaly_", 1:length(percentile_98_to_60), "_", id, sep = "")
  
  #####################################################################################
  #####################################################################################
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_MU21$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia <- append(anomalia,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia <-append(anomalia,0)
      }
    }
  }
  #We obtain the second type of anomaly.
  anomalia2 <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia2 <- append(anomalia2,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_MU21$anomalia <- unlist(anomalia) #We add the anomalies to the dataset.
  TH_MU21$anomalia2 <- unlist(anomalia2) #We add the anomalies to the dataset.
  #To rename the added columns correctly.
  columna_media <- paste("NDVI_mean_",id,sep = "")
  columna_anomalia <- paste("Anomaly_a_", id,sep = "")
  columna_anomalia2 <- paste("Anomaly_b_", id,sep = "")
  #we rename them
  names(TH_MU21)[names(TH_MU21) == "media"] <- columna_media
  names(TH_MU21)[names(TH_MU21) == "anomalia"] <- columna_anomalia
  names(TH_MU21)[names(TH_MU21) == "anomalia2"] <- columna_anomalia2
  # Convert the matrix to a data frame
  anomalias_df <- as.data.frame(anomalias)
  
  # Add the columns to the dataset
  TH_MU21 <- cbind(TH_MU21, anomalias_df)
  
}
#We add the previous 10 temperature and humidity measurements as columns for each date.
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
  if(i>10){#The first 10 measurements were only used to retain data.
    #We add the previous 10 humidity measurements.
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
    #We add the previous 10 temperature measurements.
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
TH_MU21 <- TH_MU21[11:nrow(TH_MU21),] #We remove the 10 unnecessary rows for which there are no NDVI measurements.
#We add the previous 20 columns to the dataframe.
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
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_temp <- select(TH_MU21, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_MU21$mean_temp_last10 <- mean_diffs
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_hum <- select(TH_MU21, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_MU21$mean_hum_last10 <- mean_diffs
#Calculate column with mode of anomalies for each date.
#Select columns that start with 'anomaly'
anomalies_df <- dplyr::select(TH_MU21,contains("Anomaly_a"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs

#TH_MU21$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MU21$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MU21$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_MU21,contains("Anomaly_b"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs
TH_MU21$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#We calculated it for all anomalies
anomaly_columns <- colnames(TH_MO12)[grepl("Anomaly_", colnames(TH_MU21))]

for (i in 1:length(percentile_98_to_60)) {
  col_name <- paste("Anomaly_", i, sep = "")
  agg_column_name <- paste(col_name, "Agg", sep = "")
  
  anomalies_df <- dplyr::select(TH_MU21, contains(col_name))
  
  ZEROS <- rowSums(anomalies_df == 0, na.rm = TRUE)
  ONES <- rowSums(anomalies_df, na.rm = TRUE)
  NAS <- rowSums(is.na(anomalies_df))
  
  TH_MU21[[agg_column_name]] <- ONES / (ONES + ZEROS + NAS)
}
#Finally, we obtain the number of NDVI measurements for each date
TH_MU21$n_measurements <- (ONES+ZEROS)
colnames(TH_MU21)[1] <- "date"
write.table(x = TH_MU21, "anomaliesMU21.csv", row.names = F, sep=";")
################################################
# Dataset Estación MU62
################################################
################################################
#Adding the temperature and humidity values from the station, excluding the dates for which we don't have NDVI measurements.
################################################
ant_MU62 <- TH_MU62[1:10,]
TH_MU62 <- subset(TH_MU62, fecha %in% Date$Fecha)
TH_MU62 <- rbind(ant_MU62, TH_MU62)
auxx3 <- TH_MU62 #To use it and prevent accumulation during the merge.
id <- 0
for(contrato in contrato_MU62$Contrato) {
  # print(contrato)
  id <- id + 1
  #We obtain the mean NDVI values for that park.
  media <- 
    listDF2DF(S) %>%
    dplyr::select(Contrato, Fecha, NDVI.scl_7_8_9.Mean) %>% na.omit() %>%
    dplyr::filter(Contrato == contrato) %>%
    dplyr::filter(Fecha %in% TH_MU62$fecha) %>% dplyr::select(Fecha,NDVI.scl_7_8_9.Mean)
  #We obtain the two values that will indicate whether the NDVI values have been anomalous or not.
  percentil_90 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.9, na.rm = TRUE) 
  percentil_10 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.1, na.rm = TRUE)
  #We also add the anomalies for the 20th and 80th percentiles to have a different anomaly index.
  percentil_80 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.8, na.rm = TRUE) 
  percentil_20 <- quantile(x=media$NDVI.scl_7_8_9.Mean, probs = 0.2, na.rm = TRUE)
  #We add the two new columns to our dataset.
  ######################################################################################
  #EVERY PERCENTILE
  ######################################################################################
  # Vector with the desired percentiles
  percentiles <- c(seq(0.98, 0.6, -0.01), seq(0.02, 0.4, 0.01))
  
  # Vector to store the results
  percentile_values <- vector("numeric", length(percentiles))
  
  # Data
  data <- media$NDVI.scl_7_8_9.Mean
  
  # Calculate the percentiles and store the values in the vector
  for (i in 1:length(percentiles)) {
    percentile_values[i] <- quantile(x = data, probs = percentiles[i], na.rm = TRUE)
    
  }
  
  # Split the percentiles into two sets
  percentile_98_to_60 <- percentile_values[percentiles >= 0.6 & percentiles <= 0.98]
  percentile_2_to_40 <- percentile_values[percentiles >= 0.02 & percentiles <= 0.4]
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  anomalias <- matrix(0, ncol = length(percentile_98_to_60), nrow = nrow(x))
  
  for (i in 1:length(percentile_98_to_60)) {
    med <- x$NDVI.scl_7_8_9.Mean
    anomalous <- (med >= percentile_98_to_60[i] | med <= percentile_2_to_40[i])
    
    # Assign values directly to the corresponding column in the matrix
    anomalias[, i] <- ifelse(is.na(med), NA, anomalous)
  }
  
  colnames(anomalias) <- paste("Anomaly_", 1:length(percentile_98_to_60), "_", id, sep = "")
  
  #####################################################################################
  #####################################################################################
  x<-merge(auxx3, media, by.x = "fecha",by.y = "Fecha", all.x = TRUE) #We fill with NA when there are no NDVI values.
  x <- x %>% dplyr::select(NDVI.scl_7_8_9.Mean)
  TH_MU62$media <- x$NDVI.scl_7_8_9.Mean
  anomalia <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia <- append(anomalia,med)
    }
    else{
      if(med >= percentil_90 || med <= percentil_10 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia <- append(anomalia,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia <-append(anomalia,0)
      }
    }
  }
  #We obtain the second type of anomaly.
  anomalia2 <- list()
  #For each of the NDVI measurements for each contract, we determine whether it's an anomalous measurement or not (if it belongs to the 90th or 10th percentile).
  for(med in x$NDVI.scl_7_8_9.Mean){
    if(is.na(med)){
      anomalia2 <- append(anomalia2,med)
    }
    else{
      if(med >= percentil_80 || med <= percentil_20 ){ #If it belongs to the 90th or 10th percentile, then it is an anomalous value.
        anomalia2 <- append(anomalia2,1)
      }
      else{ #In any other case, it is not an anomalous NDVI value.
        anomalia2 <-append(anomalia2,0)
      }
    }
  }
  TH_MU62$anomalia <- unlist(anomalia) #We add the anomalies to the dataset.
  TH_MU62$anomalia2 <- unlist(anomalia2) #We add the anomalies to the dataset.
  #To rename the added columns correctly.
  columna_media <- paste("NDVI_mean_",id,sep = "")
  columna_anomalia <- paste("Anomaly_a_", id,sep = "")
  columna_anomalia2 <- paste("Anomaly_b_", id,sep = "")
  #we rename them
  names(TH_MU62)[names(TH_MU62) == "media"] <- columna_media
  names(TH_MU62)[names(TH_MU62) == "anomalia"] <- columna_anomalia
  names(TH_MU62)[names(TH_MU62) == "anomalia2"] <- columna_anomalia2
  # Convert the matrix to a data frame
  anomalias_df <- as.data.frame(anomalias)
  
  # Add the columns to the dataset
  TH_MU62 <- cbind(TH_MU62, anomalias_df)
  
}
#We add the previous 10 temperature and humidity measurements as columns for each date.
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
  if(i>10){#The first 10 measurements were only used to retain data.
    #We add the previous 10 humidity measurements.
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
    #We add the previous 10 temperature measurements.
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
TH_MU62 <- TH_MU62[11:nrow(TH_MU62),] #We remove the 10 unnecessary rows for which there are no NDVI measurements.
#We add the previous 20 columns to the dataframe.
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
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_temp <- select(TH_MU62, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, temp10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_temp), ncol = 0))
for (i in 1:(ncol(med_temp)-1)) {
  col_diff <- med_temp[,i] - med_temp[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_MU62$mean_temp_last10 <- mean_diffs
#Now we calculate a column that contains the average temperature difference between a day and its following day over the last ten days (relative to the date in the row).
med_hum <- select(TH_MU62, hum1, hum2, hum3, hum4, hum5, hum6, hum7, hum8, hum9, hum10)
# Calculate the absolute differences between the columns.
diff_df <- data.frame(matrix(nrow = nrow(med_hum), ncol = 0))
for (i in 1:(ncol(med_hum)-1)) {
  col_diff <- med_hum[,i] - med_hum[,i+1]
  diff_df <- cbind(diff_df, col_diff)
}
abs_diffs <- abs(diff_df)
# Calculate the mean of the absolute differences of each pair of columns
mean_diffs <- rowMeans(abs_diffs)
TH_MU62$mean_hum_last10 <- mean_diffs
#Calculate column with mode of anomalies for each date.
#Select columns that start with 'anomaly'
anomalies_df <- dplyr::select(TH_MU62,contains("Anomaly_a"))

ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs

#TH_MU62$anomaliesAgg <-ONES / (ONES + ZEROS)
TH_MU62$anomaliesAgg <-ONES / unique(ONES+ZEROS+NAS)
TH_MU62$anomaliesAgg2 <-ONES / (ONES+ZEROS)
#Calculamos ahora el índice de anomalías del percentil 80 y 20, anomaliesAgg3
anomalies_df <- dplyr::select(TH_MU62,contains("Anomaly_b"))
ZEROS = rowSums(anomalies_df == 0, na.rm = T) # number of ceros
ONES = rowSums(anomalies_df, na.rm=T)        # number of ones
NAS = rowSums(is.na(anomalies_df))          # number of NAs
TH_MU62$anomaliesAgg3 <-ONES / unique(ONES+ZEROS+NAS)
#We calculated it for all anomalies
anomaly_columns <- colnames(TH_MU62)[grepl("Anomaly_", colnames(TH_MU62))]

for (i in 1:length(percentile_98_to_60)) {
  col_name <- paste("Anomaly_", i, sep = "")
  agg_column_name <- paste(col_name, "Agg", sep = "")
  
  anomalies_df <- dplyr::select(TH_MU62, contains(col_name))
  
  ZEROS <- rowSums(anomalies_df == 0, na.rm = TRUE)
  ONES <- rowSums(anomalies_df, na.rm = TRUE)
  NAS <- rowSums(is.na(anomalies_df))
  
  TH_MU62[[agg_column_name]] <- ONES / (ONES + ZEROS + NAS)
}
#Finally, we obtain the number of NDVI measurements for each date
TH_MU62$n_measurements <- (ONES+ZEROS)
colnames(TH_MU62)[1] <- "date"
write.table(x = TH_MU62, "anomaliesMU62.csv", row.names = F, sep=";")
