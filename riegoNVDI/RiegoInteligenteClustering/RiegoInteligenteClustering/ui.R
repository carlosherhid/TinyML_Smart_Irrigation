#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#############################################
#Versión que toma los datos del clustering desde local
#############################################

library(shiny)
library(leaflet)
library(DT)
library(elastic)#libreria elastic
library(elasticsearchr) #libreria elastic
source(file.path("code","include.R"), encoding = "UTF-8")
source(file.path("code","DatGeo.R"), encoding = "UTF-8")
source(file.path("code","DatAmb.R"), encoding = "UTF-8")
source(file.path("code","Riego.R"), encoding = "UTF-8")
source(file.path("code","Verdor.R"), encoding = "UTF-8")
source(file.path("code","DatInt.R"), encoding = "UTF-8")
#Script que prueba a realizar clustering a unas series iniciales con un solo tipo de disimilitud y que obtiene en "grp" las series con sus etiquetas, agrupados los contratos por clusters
#Author: Carlos Hernández Hidalgo
library(dplyr)
library(knitr)


dirs$data <<- "datosTFG"
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


### Contrato

contratos <- dflt$contratos("all")
contratos <- dflt$contratos("ConLecturas")
contratos <- dflt$contratos("ConImgs")
contratos <- dflt$contratos("ConSums", mask = NA)
contratos <- dflt$contratos("ConSums")
contratos <- dflt$contratos("ConSeries")



shinyUI(navbarPage(
    title = "Riego Inteligente C",
    navbarMenu(
        title = "Datos Geograficos",
        tabPanel( #Panel que muestra la localizacion de acometidas y zonas verdes
            title = "Acometidas y zonas verdes",
            selectInput("Tipo", "Elige un tipo...", c("Zonas verdes(poligonos)"="pols","Acometidas (puntos)"="ptos")),             #Tipo de dato geografico escogido
            selectInput("Localidad", "Elige Localidad", unique(dg$Localidad)),    #Localidad escogida
            leaflet::leafletOutput("mapa")           #mostramos el mapa
            
        ),
        tabPanel( #Panel que muestra un histograma que representa el numero de poligonos por parque
           title = "Poligonos por parque",
                 DatInt$plotHist(dg, Poligonos,"Numero de poligonos", sup=50)
            
        ),
        tabPanel( #Panel que muestra un histograma con los tamanos totales de cada parque
            title = "Tamanos totales parques",
            DatInt$plotHist(dg, Area, "Area total del parque (m^2)", sup = 12000)
            
        ),
        tabPanel( #Panel que muestra un histograma con la distancia de cada parque a su estacion asociada principal
            title = "Distancias parques a estaciones",
            DatInt$plotHist(dg %>% dplyr::mutate(Distancia = Distancia / 1000), Distancia, 
                            "Distancia del parque a su estacion principal (km)", sup = 20)
            
        ),
        tabPanel( #Panel tabla de recuentos de estaciones meteorológicas y mosaicos Sentinel-2 asociados
            title = "Tabla de recuentos de estaciones meteorológicas y mosaicos Sentinel-2 asociados",
            dataTableOutput("tablaEstMet"), dataTableOutput("tablaEstMet2")
            
        ),
    ),
    navbarMenu(
        title = "Datos Contratos",
        tabPanel(
            title = "Contrato", 
            selectInput("contrato", "", contratos, "6373577"),
            leaflet::leafletOutput("mymap")
        ),
        
    ),
    navbarMenu(
        title = "Disponibilidad de datos",
        tabPanel(
            title = "Datos faltantes por estación meteorológica",
            dataTableOutput("tablaNA")
        ),
        tabPanel(
            title = "Contadores en Lecturas comparados con el de Contratos", 
            dataTableOutput("tablaLC")
        ),
        tabPanel(
            title = "Fecha de primera lectura",
            DatInt$plotHist(lecturasR, Fecha_Min, "primera telectura", inf = as.Date("2017-09-30"), .bw = "quarter")
        ),
        tabPanel(
            title = "Fecha de última lectura",
            
            DatInt$plotHist(lecturasR, Fecha_Max, "última telectura", sup = as.Date("2021-05-01"), .bw = "month")
        ),
        tabPanel(
            title = "Valores no nulos de NDVI enmascarados", 
            
            DatInt$plotHist(idvsR, N_NDVI, "Valores NDVI disponibles")
        ),
        tabPanel(
            title = "Observaciones completas", 
            
            DatInt$plotHist(datosR, N_Obs, "Observaciones completas disponibles")
        ),
    ),
    navbarMenu(
        title = "Datos de verdor",
        tabPanel(
            title = "NDVI",
            plotly::plotlyOutput("viPlot")
        ),
        tabPanel(
            title = "Imagen RGB",
            shiny::plotOutput("rgbImg")
        ),
        tabPanel(
            title = "Mapa SCL",
            shiny::plotOutput("sclImg")
        ),
        tabPanel(
            title = "Imagen NDVI",
            shiny::plotOutput("ndviImg")
        ),
        tabPanel(
            title = "Imagen NDVI enmascarada",
            shiny::plotOutput("ndviMaskImg")
        )
    ),
    tabPanel(
        title = "Balance hidrico",
        plotly::plotlyOutput("bhPlot")
    ),
    tabPanel(
        title = "Clustering",
        plotOutput("distPlot"),
        sliderInput(inputId = "num",     #aquí escogemos el numero de clusters para realizar el cuttree
                    label = "Choose a maximum number of clusters", 
                    value = 5, min = 2, max = 25),
        # para seleccionar la fecha de inicio y de fin para escoger parques para el clustering
        dateInput("inicio", "Introduzca la fecha de inicio para el clustering", "2019-01-01","2017-10-01", Sys.Date(), weekstart = 1, language = "es"),
        
        dateInput("fin", "Introduzca la fecha de fin para el Clustering", "2021-05-17","2017-10-01", Sys.Date(), weekstart = 1, language = "es"),
        
        plotOutput("cuttreeplot") #mostramos los resultados y tenemos agrupaciones para 2 clústers hasta 5 clústers (max_n_clusters) por defecto, si no, el valor escogido
    )
))
