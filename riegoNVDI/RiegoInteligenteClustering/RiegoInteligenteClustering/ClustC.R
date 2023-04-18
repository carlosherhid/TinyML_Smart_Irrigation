#Script que prueba a realizar clustering a unas series iniciales con un solo tipo de disimilitud y que obtiene en "grp" las series con sus etiquetas, agrupados los contratos por clusters
#Author: Carlos Hernández Hidalgo
source(file.path("code","include.R"), encoding = "UTF-8")
library(dplyr)
library(knitr)
library(elastic)#libreria elastic
#library(elasticsearchr) #libreria elastic
#Inicialización de parámetros para el Clustering
################################################
ind = dflt$ind   #índice de medición, en este caso NDVI 
mask = dflt$mask #máscara, en este caso "scl_7_8_9"
agg = dflt$agg #"mean"
serie = dflt$serie #tipo de serie, "Verdor"
pkg = dflt$pkg #paquete usado para clustering, "TSClust"
ini = as.Date("2020-01-13") #dflt$ini #Fecha de inicio para filtrar a partir de ahí los datos sobre los que queremos hacer clustering
fin = as.Date("2021-02-20") #dflt$fin #Fecha de fin para filtrar los datos sobre los que queremos aplicar clustering
aggl = "complete" #usado para ejecutar el clustering
max_n_clusts = 5 #número máximo de clusters, se obtendrá clustering desde 2 hasta max_n_clusts
################################################
#Preparamos los contratos
################################################
table(X$NDVI.scl_7_8_9.Mean)
X <- S$`6351960`
S$`6351960
S[1]
S <- Verdor$readSumInd(contratos, ind, mask, ini = ini, fin = fin)
contratos <- dflt$contratos("ConSeries", ind, mask, agg)
if (serie == "Riego")
  contratos %<>% intersect(dflt$contratos("ConConsumos"))
################################################
#Preparamos S, que serán las series iniciales a las que aplicaremos clustering
################################################
S <- switch (serie,
             "Riego" = {
               S <- Riego$readConsumos(contratos, ini = ini, fin = fin) %>%
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
               S <- Verdor$readSumInd(contratos, ind, mask, ini = ini, fin = fin) %>%
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
                         idvs <- Verdor$readSumIndElastic(contratos, ind, mask, ini = ini, fin = fin)
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
)
################################################
#Preparamos D, que son las distancias 2 a 2 de los contratos, necesario para el clustering
################################################
D <- TSclust::diss(S, "COR", beta = NULL) #Aplicamos en este caso la disimilitud "COR.1" del paquete TSClust
################################################
#Se realiza el clustering
################################################
hcl <- stats::hclust(D, aggl) #despues habrá que hacer un cuttree para obtener las series finales agrupadas por clusters, es decir, con el clustering ya aplicado
plot(hcl) #Vemos que están ya agrupados pero que no tienen aún sus etiquetas, por ello hay que realizar cuttree, para saber por dónde agrupar
################################################
#Aplicamos a los datos anteriores cuttree
################################################
grp <- stats::cutree(hcl, k = max_n_clusts:max_n_clusts)#, order_clusters_as_data = F)  se obtienen las etiquetas
grp
hcl
hist(grp , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="") #mostramos los resultados y tenemos agrupaciones para 2 clústers hasta 5 clústers (max_n_clusters)
