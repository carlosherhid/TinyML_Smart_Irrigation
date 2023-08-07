#Secondary script used for obtaining the length and latitude of parks and stations.
#Author: Carlos Hernández Hidalgo
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)
library(geosphere)
library(purrr)
acometidas <- DatGeo$leerAcometidas()
tabla <- DatGeo$reunirTablas() %>%
  dplyr::select(Acometida,Abcisa,Norte, Contrato,Localidad) #Each connection with its contract, its location, latitude, and longitude.
estaciones <- DatGeo$leerEstMet() #The weather stations with their locations.
dflt$eemm #The names of the weather stations.
#We obtain the service connection number, contract, and nearest or main weather station.
sg <- DatGeo$asignarEstMet() %>%
  dplyr::select(Acometida, Contrato, Principal) 
ca42 <- sg%>%
  dplyr::filter(Principal == dflt$eemm[1]) 

CA91 <- sg%>%
  dplyr::filter(Principal == dflt$eemm[2]) 
MO12 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[3])
MU21 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[4])
MU62 <-sg%>%
  dplyr::filter(Principal == dflt$eemm[5])