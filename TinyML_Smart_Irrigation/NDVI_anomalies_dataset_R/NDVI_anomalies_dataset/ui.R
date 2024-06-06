#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(leaflet)
library(DT)
source(file.path("code","include.R"), encoding = "UTF-8")
source(file.path("code","DatGeo.R"), encoding = "UTF-8")
source(file.path("code","DatAmb.R"), encoding = "UTF-8")
source(file.path("code","Riego.R"), encoding = "UTF-8")
source(file.path("code","Verdor.R"), encoding = "UTF-8")
source(file.path("code","DatInt.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)

################################################
################################################
dirs$data <<- "datos"
dflt$lan <<- "sp"
dg <- dflt$dg()
de <- DatGeo$leerEstMet() %>%
    sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), crs = DatGeo$epsg$etrs89geo)
lecturasR <- Riego$leerResumenLecturas() %>%
    dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), lubridate::period_to_seconds) %>%
    dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), as.difftime, units = "secs")

units(lecturasR$Tiempo_Min) <- "mins"
units(lecturasR$Tiempo_Mean) <- "hours"
units(lecturasR$Tiempo_Max) <- "days"

lecturasR %<>%
    dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), as.double) %>%
    dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), round, 2)



idvsR <- Verdor$leerResumenSumInd() %>%
    dplyr::rename(N_NDVI = NDVI.scl_7_8_9.Mean_TotalNoNA) %>%
    dplyr::filter(N_NDVI > 0)


datosR <- DatInt$leerResumenDatInt() %>%
    dplyr::rename(N_Obs = Obs_CCs)
ind = dflt$ind   #Measurement index, in this case, NDVI.
mask = dflt$mask #Mask, in this case, "scl_7_8_9".
agg = dflt$agg #"mean"

### Contrato

#contratos <- dflt$contratos("all")
#contratos <- dflt$contratos("ConLecturas")
#contratos <- dflt$contratos("ConImgs")
#contratos <- dflt$contratos("ConSums", mask = NA)
#contratos <- dflt$contratos("ConSums")
#contratos <- dflt$contratos("ConSeries")
contratos <- dflt$contratos("ConSeries", ind, mask, agg)
contratos_replaced <- seq_len(length(contratos))

shinyUI(navbarPage(
    title = "NDVI PARK DATA",
    navbarMenu(
        title = "Geographical Data",
        tabPanel( #Panel que muestra la localizacion de acometidas y zonas verdes
            title = "Parks and water connections",
            selectInput("Tipo", "Choose a type...", c("Green zones(polygons)"="pols","water connections(points)"="ptos")),             #Tipo de dato geografico escogido
            selectInput("Localidad", "Choose location", unique(dg$Localidad)),    #Localidad escogida
            leaflet::leafletOutput("mapa")           #mostramos el mapa
            
        ),
    ),
    navbarMenu(
        title = "Contract data",
        tabPanel(
            title = "By number of contract", 
            selectInput("contrato", "", contratos_replaced, "1"),
            leaflet::leafletOutput("mymap")
        ),
        
    ),
))
