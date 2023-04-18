#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
dg <- dflt$dg()
#Inicialización de parámetros para el Clustering
################################################
ind = dflt$ind   #índice de medición, en este caso NDVI 
mask = dflt$mask #máscara, en este caso "scl_7_8_9"
agg = dflt$agg #"mean"
serie = dflt$serie #tipo de serie, "Verdor"
pkg = dflt$pkg #paquete usado para clustering, "TSClust"
ini = dflt$ini #Fecha de inicio para filtrar a partir de ahí los datos sobre los que queremos hacer clustering
fin = dflt$fin #Fecha de fin para filtrar los datos sobre los que queremos aplicar clustering
aggl = "complete" #usado para ejecutar el clustering
max_n_clusts = 5 #número máximo de clusters, se obtendrá clustering desde 2 hasta max_n_clusts
################################################
#Preparamos los contratos
################################################
contratos <- dflt$contratos("ConSeries", ind, mask, agg)
if (serie == "Riego")
  contratos %<>% intersect(dflt$contratos("ConConsumos"))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$mapa <- leaflet::renderLeaflet({
    if (input$Tipo == "pols")
      DatGeo$plotMapa(dg, "Barrio", "Localidad", input$Localidad)
    else
      DatGeo$plotMapa(sf::st_centroid(dg), "Barrio", "Localidad", input$Localidad)
  })
    output$tablaEstMet <- DT::renderDataTable(expr=DatGeo$consultarDatGeo("tablaEstMet", dg = dg), 
                                              caption = "Recuento de estaciones meteorológicas asignadas a cada jardín")
    
    output$tablaEstMet2 <- DT::renderDataTable(expr=DatGeo$consultarDatGeo("tablaEstMet2", dg = dg),
                                               caption = "Recuento de estaciones principal vs. alternativa 1 asignadas a cada jardín")
    
    output$tablaMosSen2 <- DT::renderDataTable(expr=DatGeo$consultarDatGeo("tablaMosSen2", dg = dg),
                                               colnames = c("Mosaico","Recuento"),
                                               caption = "Recuento de mosaicos de Sentinel-2 en los que se encuentra cada jardín")
    output$tablaNA <- DT::renderDataTable(expr=DatAmb$consultarDatMet("tablaNA"))
    
    output$tablaLC <- DT::renderDataTable(expr=Riego$consultarResumenContadores("tabla"))
    # Estacion
    estacion <- shiny::reactive(DatGeo$consultarDatGeo("listEstMet",input$contrato, dg = dg))
    output$estacion <- shiny::renderText(estacion()[1])
    
    datos <- shiny::reactive(DatInt$leerSeries(input$contrato, ini = "2011-01-01") %>% DatInt$reestructurarSeries())
    
    output$mymap <- leaflet::renderLeaflet({DatGeo$plotMapa(dg, "Contrato", "Contrato", input$contrato)})
    
    output$bhPlot <- plotly::renderPlotly(DatInt$plotTS(datos(), "BH"))
    
    output$viPlot <- plotly::renderPlotly(DatInt$plotTS(datos(), "NDVI.scl_7_8_9.Mean"))
    
    output$rgbImg <- shiny::renderPlot(Verdor$plotImgInd(input$contrato, 'RGB', NA, format(input$dia, "%Y%m%d"), 3, dg))
    
    output$sclImg <- shiny::renderPlot(Verdor$plotImgInd(input$contrato, 'SCL', NA, format(input$dia, "%Y%m%d"), 3, dg))
    
    output$ndviImg <- shiny::renderPlot(Verdor$plotImgInd(input$contrato, 'NDVI', NA, format(input$dia, "%Y%m%d"), 3, dg))
    
    output$ndviMaskImg <- shiny::renderPlot(Verdor$plotImgInd(input$contrato, 'NDVI', 'scl_7_8_9', format(input$dia, "%Y%m%d"), 3, dg))
    #CLUSTERING
    #Datos para el clustering
    ################################################
    #Preparamos S, que serán las series iniciales a las que aplicaremos clustering
    ################################################
    S <-reactive({ switch (serie,
                 "Riego" = {
                   S <- Riego$readConsumos(contratos, ini = input$inicio, fin = input$fin) %>%
                     listDF2DF() %>%
                     dplyr::select(Contrato, Fecha, Consumo) %>%
                     fil2col() %>% dplyr::select(!Fecha)
                   switch (pkg,
                           "pdc" = as.matrix(S),
                           "TSclust" = S %>% dplyr::filter(stats::complete.cases(.)),
                           stop("pkg no vÃ¡lido: ",pkg)
                   )
                 },
                 "Verdor" = {
                   S <- Verdor$readSumInd(contratos, ind, mask, ini = input$inicio, fin = input$fin) %>%  
                     listDF2DF() %>%
                     dplyr::select(Contrato, Fecha, Verdor$getVars(ind, mask, agg)) %>%
                     fil2col() %>% dplyr::select(!Fecha)
                   switch (pkg,
                           "pdc" = as.matrix(S),
                           "TSclust" = S %>% dplyr::filter(stats::complete.cases(.)),
                           stop("pkg no vÃ¡lido: ",pkg)
                   )
                 },
                 "VerdorMulti" = {
                   switch (pkg,
                           "pdc" = {
                             idvs <- Verdor$readSumIndElastic(contratos, ind, mask, ini = input$inicio, fin = input$fin)
                             froms <- sapply(idvs, function(idv) min(idv$Fecha))
                             from  <- lubridate::as_date(min(froms))
                             tos   <- sapply(idvs, function(idv) max(idv$Fecha))
                             to    <- lubridate::as_date(max(tos))
                             S <- idvs %>%
                               lapply(completarFechas, from, to, "5 days") %>%
                               listDF2DF() %>%
                               dplyr::select(Verdor$getVars(ind, mask, Verdor$funs[1:6]))
                             # 2 dim:
                             # dim 1: obs ordenado por Contrato, Fecha
                             # dim 2: variables
                             S <- as.matrix(S)
                             # 3 dim:
                             # dim 1: Fecha
                             # dim 2: Contrato
                             # dim 3: variables
                             dim(S) <- c(as.integer(to-from)/5L + 1L, length(idvs), ncol(S))
                             dimnames(S) <- list(seq(from, to, "5 days"), names(idvs),
                                                 Verdor$getVars(ind, mask, Verdor$funs[1:6]))
                             S
                           },
                           "TSclust" = stop("El paquete TSclust no permite agrupar series multidimensionales"),
                           stop("pkg no vÃ¡lido: ",pkg)
                   )
                   
                 },
                 stop("serie no vÃ¡lida: ",serie)
    )})
    ################################################
    #Preparamos D, que son las distancias 2 a 2 de los contratos, necesario para el clustering
    ################################################
    D <- reactive({TSclust::diss(S(), "COR", beta = NULL)}) #Aplicamos en este caso la disimilitud "COR.1" del paquete TSClust
    ################################################
    #Se realiza el clustering
    ################################################
    hcl <- reactive({stats::hclust(D(), aggl)}) #despues habrá que hacer un cuttree para obtener las series finales agrupadas por clusters, es decir, con el clustering ya aplicado
    ################################################
    #Aplicamos a los datos anteriores cuttree
    ################################################
    grp <- reactive({stats::cutree(hcl(), k = input$num:input$num)#, order_clusters_as_data = F  se obtienen las etiquetas con el número de clústers escogido
    })
   output$distPlot <- renderPlot({
      plot(hcl()) # Con este plot Vemos que están ya agrupados los parques en jerarquía pero que no tienen aún sus etiquetas, por ello hay que realizar cuttree, para saber por dónde agrupar
    })
   output$cuttreeplot <- renderPlot({
     hist(grp() , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="") #mostramos los resultados y tenemos agrupaciones para (max_n_clusters)
    })
})
