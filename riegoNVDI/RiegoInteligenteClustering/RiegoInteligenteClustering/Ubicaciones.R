#Script para la obtención de la longitud y latitud de parques y estaciones
#Author: Carlos Hernández Hidalgo
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)
library(geosphere)
library(purrr)
acometidas <- DatGeo$leerAcometidas()
tabla <- DatGeo$reunirTablas() %>%
  dplyr::select(Acometida,Abcisa,Norte, Contrato,Localidad) #cada acometida con su contrato, su ubicación y su latitud y longitud
estaciones <- DatGeo$leerEstMet() #Las estaciones meteorológicas con sus ubicaciones
dflt$eemm #Los nombres de las estaciones meteorológicas
#Obtenemos el numero de acometida, contrato y estación meteorologica mas cercana o principal
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