library(magrittr, warn.conflicts = F, verbose = F, quietly = T) # %>%, %<>%
# otros paquetes: base, utils, stats
# ggplot2, scales, --units
# stringr, lubridate
# tibble, data.table, readxl, zoo, xts
# reshape2, tidyr, purrr, dplyr, plyr
# weathermetrics, ET.PenmanMonteith, sf, sen2r
# RCurl, ssh

dirs <- list()
files <- list()
paths <- list()

dflt <- list()


msg <- function(..., fich = NULL) {
  if (!is.null(fich)) {
    c <- file(fich, "a")
    on.exit(close(c), add = T, after = F)
    sink(c, , type = "message")
    on.exit(sink(type = "message"), add = T, after = F)
  }
  message("[",strftime(Sys.time(), format = ""),"] ",...)
}

print_msg <- function(s, out = NULL, err = NULL) {
  if (!is.null(out)) {
    co <- file(out, "a")
    on.exit(close(co), add = T, after = F)
    sink(co, type = "output")
    on.exit(sink(type = "output"), add = T, after = F)
  }
  if (!is.null(err)) {
    ce <- file(err, "a")
    on.exit(close(ce), add = T, after = F)
    sink(ce, type = "message")
    on.exit(sink(type = "message"), add = T, after = F)
  }
  if (is.list(s))
    print(s)
  else
    cat(s, fill = 1L) # stdout
  message(s) # stderr
}

newFile <- function(path, file_name, ext = NULL, header = NULL) {
  fsep <- if (dflt$node %in% c("dibulibu","juno")) "/" else .Platform$file.sep
  if (!is.null(ext)) file_name %<>% paste0('.',ext)
  file_path <- file.path(path, file_name, fsep = fsep)
  if (dflt$node == Sys.info()["nodename"] &&
      !file.exists(file_path) && !is.null(header)) {
    cat(header, file = file_path)
    msg("New file ", file_path)
  }
  return(file_path)
}

newDir <- function(path, dir_name) {
  fsep <- if (dflt$node %in% c("dibulibu","juno")) "/" else .Platform$file.sep
  dir_path <- file.path(path, dir_name, fsep = fsep)
  if (dflt$node == Sys.info()["nodename"] && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = T)
    msg("Create dir ", dir_path)
  }
  return(dir_path)
}


# http://gauss.inf.um.es:8080/parques/

# Global
{
  dflt$node <- Sys.info()["nodename"]
  paths$home <- function() switch(dflt$node,
    "dibulibu" = "/home/aurora/RiegoInteligente",
    "juno" = "/home/gardens/RiegoInteligente",
    ".")

  dirs$data <- "datos"
  paths$data <- function() newDir(paths$home(), dirs$data)

  dirs$code <- "code"
  files$code <- {c(
    include = "include.R",
    dg = "DatGeo.R",
    da = "DatAmb.R",
    dr = "Riego.R",
    dv = "Verdor.R",
    di = "DatInt.R",
    cl = "Clust.R",
    ml = "ML.R",
    lstm = "lstm.R",
    run = "run.R",
    sh = "run.sh",
    shr = "run_dibulibu.sh",
    shv = "run_juno.sh"
  )}
  paths$code <- function(id = NULL) {
    if (is.null(id))
      return(newDir(paths$home(), dirs$code))
    return(newFile(paths$code(), files$code[id]))
  }

  lapply(c("dg","da","dr","dv","di","cl","ml","lstm"), function(id) {
    f <- paths$code(id)
    if (file.exists(f))
      source(f, encoding = "UTF-8")
  })

  dflt$ini <- as.Date("2019-01-01")
  dflt$fin <- as.Date("2021-05-17")
  # dflt$ini <- as.Date("2017-09-27")
  # dflt$fin <- Sys.Date()

  dflt$eps <- 0
  dflt$faltantesC <- 5L
  dflt$faltantesL <- "9 days"
  dflt$noActuales <- dflt$fin - lubridate::as.period(dflt$faltantesL)
  dflt$tardias <- dflt$ini + lubridate::as.period("6 months")
  dflt$vegetacion <- 0.3

  dflt$contratos  <- function(id = "all", ind = dflt$ind, mask = dflt$mask,
                              agg = dflt$agg, crop = dflt$crop, int = dflt$int) {

    v <- switch (id,
      "all" = dflt$dg() %>% dplyr::filter(!is.na(Contrato)) %>% dplyr::pull(Contrato),

      "conLecturas" = paths$lects() %>% list.files() %>% basename_sans_ext(),
      # "ConLecturas" = DatInt$leerResumenJardines(maxCD = "LecturasNoActuales",
      #                                            version = 1)$Contrato,
      "ConLecturas" = DatInt$leerResumenJardines(maxCD = "VariosContratos",
                                                 version = 1)$Contrato,

      "conConsumos" = paths$conss() %>% list.files() %>% basename_sans_ext(),
      # "ConConsumos" = DatInt$leerResumenJardines(maxCD = "LecturasNoActuales", version = 1) %>%
      "ConConsumos" = DatInt$leerResumenJardines(maxCD = "VariosContratos", version = 1) %>%
        dplyr::filter(!(abs(Consumo_Min) <= dflt$eps & abs(Consumo_Max) <= dflt$eps)) %>%
        dplyr::pull(Contrato),

      # "conImgs" = paths$imgs() %>% list.dirs(full.names = F, recursive = F),
      "conImgs" = Verdor$consultarDirsImgs("conImagenes")$Contrato,
      "ConImgs" = DatInt$leerResumenJardines(maxCDV = "SinImagenes")$Contrato,

      "conSums" = paths$sums(ind, mask) %>% list.files() %>% basename_sans_ext(),
      "ConSumsNA"  = DatInt$leerResumenJardines(maxCDV = "SinSerieNA",
                                                ind = ind, mask = mask, agg = agg)$Contrato,
      "ConSumsSCL" = DatInt$leerResumenJardines(maxCDV = "SinSerieSCL",
                                                ind = ind, mask = mask, agg = agg)$Contrato,
      "ConSums" = if (is.na(mask)) dflt$contratos("ConSumsNA", ind, dflt$mask, agg) else
        dflt$contratos("ConSumsSCL", ind, mask, agg),


      "conSerie" = intersect(dflt$contratos("conConsumos"),
                             dflt$contratos("conImgs")),
      "conSeries" = intersect(dflt$contratos("conConsumos"),
                              dflt$contratos("conSums", ind, mask)),
      # "ConSerie"    = DatInt$leerResumenJardines(maxCDR = "LecturasNoActuales",
      #                                            maxCDV = "SinImagenes")$Contrato,
      "ConSerie"    = intersect(dflt$contratos("ConLecturas"),
                                dflt$contratos("ConImgs")),
      # "ConSeriesNA"  = DatInt$leerResumenJardines(maxCDR = "LecturasNoActuales",
      #                                             maxCDV = "SinSerieNA",
      #                                             ind = ind, mask = mask, agg = agg)$Contrato,
      "ConSeriesNA"  = intersect(dflt$contratos("ConLecturas"),
                                 dflt$contratos("ConSumsNA", ind, mask, agg)),
      # "ConSeriesSCL" = DatInt$leerResumenJardines(maxCDR = "LecturasNoActuales",
      #                                             maxCDV = "SinSerieSCL",
      #                                             ind = ind, mask = mask, agg = agg)$Contrato,
      "ConSeriesSCL" = intersect(dflt$contratos("ConLecturas"),
                                 dflt$contratos("ConSumsSCL", ind, mask, agg)),
      "ConSeries"    = intersect(dflt$contratos("ConLecturas"),
                                 dflt$contratos("ConSums", ind, mask, agg)),

      "conDatInt" = paths$int(int, crop, ind, mask, agg) %>% list.files() %>% basename_sans_ext(),

      "Seleccionados" = DatInt$leerResumenJardines(maxDkm = 10, minAPm2 = 30,
                                                   maxCDR = "LecturasTardias",
                                                   maxCDV = "SinVegetacion")$Contrato,
      stop("id no válido: ",id)
    )
    attr(v,"id") <- switch(id,
      "conSums"    = , "conSeries"    =
        c(id = id, ind = ind, mask = mask),
      "ConSumsNA"  = , "ConSeriesNA"  = ,
      "ConSumsSCL" = , "ConSeriesSCL" =
        c(id = id, ind = ind, mask = mask, agg = agg),
      "conDatInt" =
        c(id = id, crop = crop, ind = ind, mask = mask, agg = agg, int = int),
      c(id = id)
    )
    return(v)
  }
}

# Datos geograficos
{
  dirs$geo <- "geo"
  files$geo <- {c(
    contratos = "CONTRATOS_AGUA_PARQUES_Y_JARDINES.csv",
    relaciones = "Contratos de alta en AQUACIS parques y jardines.xlsx",
    acometidas = "Acometidas.csv",
    # zonas = "zonasVerdesAP.csv",
    zonas = "Zonas Verdes Agua Potable",
    zonasR = "zonasVerdesAP.geojson",
    jardi = "jardines.geojson",
    # Aquí, https://datosabiertos.regiondemurcia.es/carm/catalogo/medio-ambiente/estaciones-meteorologicas-de-la-red-siam-carm,
    # te puedes descargar un CSV (o un JSON) con las estaciones meteorológicas y datos sobre ellas.
    estaciones = "IMIDA_estaciones.csv"
  )}
  paths$geo <- function(id = NULL) {
    if (is.null(id))
      return(newDir(paths$data(), dirs$geo))
    if (id == "zonas")
      return(newDir(paths$geo(), files$geo[id]))
    return(newFile(paths$geo(), files$geo[id]))
  }

  dflt$dg <- function() DatGeo$leerJardines()


  dflt$acometidas <- function() dflt$dg()$Acometida
  dflt$estaciones <- function() DatGeo$consultarDatGeo("setEstMet")

  # dflt$eemm <- DatAmb$consultarEstMet()
  dflt$eemm <- c("CA42","CA91","MO12","MU21","MU62")
}

# Datos ambientales
{
  dirs$amb <- "amb"
  paths$amb <- function() newDir(paths$data(), dirs$amb)

  dirs$met <- "met"
  paths$met <- function(estacion = NULL) {
    if (is.null(estacion))
      return(newDir(paths$amb(), dirs$met))
    return(newFile(paths$met(), estacion, "csv"))
  }

  # install.packages("devtools")
  # devtools::install_github("VicenteYago/ET.PenmanMonteith")
  # https://github.com/VicenteYago/ET.PenmanMonteith
  dirs$et0 <- "et0"
  paths$et0 <- function(crop = NULL, estacion = NULL) {
    if (is.null(crop))
      return(newDir(paths$amb(), dirs$et0))
    if (is.null(estacion))
      return(newDir(paths$et0(), crop))
    return(newFile(paths$et0(crop), estacion, "csv"))
  }


  dflt$crops <- c("short","tall")
  dflt$crop <- dflt$crops[2]

  dflt$dms  <- function() listDF2DF(DatAmb$readDatMet(dflt$eemm), "Estacion")
  dflt$et0s <- function() listDF2DF(DatAmb$readET0(dflt$eemm), "Estacion")
  dflt$das  <- function() listDF2DF(DatAmb$readDatAmb(dflt$eemm), "Estacion")
}

# Datos de riego
{
  dirs$riego <- "riego"
  paths$riego <- function() newDir(paths$data(), dirs$riego)

  dirs$agua <- "agua"
  paths$agua <- function(dias = NULL) {
    if (is.null(dias)) {
      parent <- if (dflt$node == "dibulibu") "/home/aurora" else paths$riego()
      return(newDir(parent, dirs$agua))
    }
    if (dflt$node == Riego$nodename && Sys.info()["nodename"] != Riego$nodename) {
      file_names <- Riego$consultarFilesAgua("dias",dias)$Fichero
      return(newFile(paths$agua(), file_names))
    }
    return(list.files(paths$agua(), Riego$pattern(dias), full.names = T))
  }

  dirs$lects <- "lecturas"
  paths$lects <- function(contrato = NULL, create = F) {
    if (is.null(contrato))
      return(newDir(paths$riego(), dirs$lects))
    cabecera <- if (!create) NULL else
      paste0(paste0(names(Riego$tiposL),collapse = ";"),"\n")
    return(newFile(paths$lects(), contrato, "csv", cabecera))
  }
  dirs$conss <- "consumos"
  paths$conss <- function(contrato = NULL, create = F) {
    if (is.null(contrato))
      return(newDir(paths$riego(), dirs$conss))
    cabecera <- if (!create) NULL else
      paste0(paste0(names(Riego$tiposC),collapse = ";"),"\n")
    return(newFile(paths$conss(), contrato, "csv", cabecera))
  }

  paths$aguaLS <- function() newFile(paths$riego(), dirs$agua, "txt")
  # fichero con los zip procesados
  files$proc <- "processed.txt"
  paths$proc <- function() newFile(paths$riego(), files$proc, NULL, "Processed\n")

  files$contR <- "contadores.csv"
  paths$contR <- function() newFile(paths$riego(), files$contR)
  paths$lectR <- function() newFile(paths$riego(), dirs$lects, "csv")
  paths$consR <- function() newFile(paths$riego(), dirs$conss, "csv")


  dflt$lecturas <- function() listDF2DF(Riego$readLecturas(expl2 = T))
  dflt$consumos <- function() listDF2DF(Riego$readConsumos())
}

# Datos de verdor
{
  # CARPETA DE TRABAJO DE sen2r
  dirs$s2r <- "sen2r"
  files$s2r <- {c(
    times = "times.txt",
    # CARPETA CON LAS PARCELAS DE ENTRADA DE sen2r
    parcelas = "jardines",
    #
    lists = "lists",
    # CARPETA CON POSIBLES JSON CON PARAMETROS PARA sen2r
    jsons = "input_jsons",
    ####
    # JSON CON PARAMETROS PARA sen2r
    params = "sen2r_prod_online.json"
    # params = "sen2r_prod_sen2cor.json"
    ####
  )}
  paths$s2r <- function(id = NULL) {
    if (is.null(id))
      return(newDir(paths$code(), dirs$s2r))
    switch (id,
      "times" = newFile(paths$s2r(), files$s2r[id]),
      "parcelas" = , "lists" = , "jsons" = newDir(paths$s2r(), files$s2r[id]),
      "params" = newFile(paths$s2r("jsons"), files$s2r[id]),
      "new_list" = newFile(paths$s2r("lists"),
                           format(Sys.time(),"%Y%m%d_%H%M%S_sen2_list.csv")),
      "last_list" = paths$s2r("lists") %>%
        # list.files("[0-9]{8}_[0-9]{6}_sen2_list.csv",full.names=T) %>%
        list.files(full.names=T) %>% utils::tail(n=1),
      "proc_par" = {
        stopifnot(dflt$node == Sys.info()["nodename"])
        sen2r::load_binpaths() %>%
          attr("path") %>% dirname() %>%
          file.path("proc_par")
      },
      stop(id," no soportado")
    )
  }

  # PARA CADA CUENTA...
  # ...CARPETA DE TRABAJO
  dirs$acc <- "account"
  files$acc <- {c(
    # ...CARPETA DE ARCHIVOS SAFES
    safes = "safes",
    # ...FICHEROS CON LAS CREDENCIALES
    apihub = "apihub.txt",
    # ...FICHEROS LOGS CON LAS SALIDAS ESTANDAR/DE ERROR
    logOUT = "logOUT.txt",
    logERR = "logERR.txt",
    # ...FICHEROS CON LOS TIEMPOS/RESUMENES DE EJECUCION
    times = "times.csv",
    status = "status.csv"
  )}
  paths$acc <- function(n_acc = 1L, id = NULL) {
    if (is.null(id))
      return(newDir(paths$s2r(), paste0(dirs$acc,n_acc)))
    if (id == "safes")
      return(newDir(paths$acc(n_acc), files$acc[id]))
    return(newFile(paths$acc(n_acc), files$acc[id]))
  }


  dirs$verdor <- "verdor"
  paths$verdor <- function() newDir(paths$data(), dirs$verdor)

  # CARPETA CON LAS SALIDAS DE sen2r
  dirs$imgs <- "images"
  paths$imgs <- function(contrato = NULL, ind = NULL, mask = NULL, dias = NULL) {
    if (is.null(contrato))
      return(newDir(paths$verdor(), dirs$imgs))
    if (is.null(ind))
      return(newDir(paths$imgs(), contrato))
    if (ind == "RGB") ind = "RGB432B"
    if (is.null(mask) || (is.na(mask) && is.null(dias)))
      return(newDir(paths$imgs(contrato), ind))
    if (!is.na(mask) && is.null(dias))
      return(newDir(paths$imgs(contrato, ind), mask))
    # !is.null(dias)
    if (dflt$node == Verdor$nodename && Sys.info()["nodename"] != Verdor$nodename) {
      stop("# TODO")
    }
    carpeta <- if (is.na(mask))
      paths$imgs(contrato,ind) else
        paths$imgs(contrato,ind,mask)
    patron <- Verdor$pattern(ind,contrato,dias)
    return(list.files(carpeta, patron, full.names = T))
  }
  dirs$sums <- "summaries"
  paths$sums <- function(ind = NULL, mask = NULL, contrato = NULL, create = F) {
    if (is.null(ind))
      return(newDir(paths$verdor(), dirs$sums))
    if (is.null(mask))
      return(newDir(paths$sums(), ind))
    if (is.null(contrato))
      return(newDir(paths$sums(ind), mask))
    cabecera <- if (!create) NULL else {
      vars <- c("Fecha", Verdor$getVars(ind, mask, Verdor$funs))
      paste0(paste0(vars,collapse = ";"),"\n")
    }
    return(newFile(paths$sums(ind, mask), contrato, "csv", cabecera))
  }

  paths$imgsLS <- function(ind = NULL) {
    file_name <- if (is.null(ind))
      dirs$imgs else
        paste(dirs$imgs,ind,sep = "_")
    newFile(paths$verdor(), file_name, "txt")
  }
  paths$imgsR <- function(ind = "BOA")
    newFile(paths$verdor(), paste(dirs$imgs,ind, sep = "_"), "csv")
  paths$sumsR <- function(ind = dflt$ind, mask = dflt$mask, agg = dflt$agg)
    newFile(paths$verdor(), paste(dirs$sums,ind,mask,agg, sep = "_"), "csv")


  # Doc ESA generación del SCL y significado:
  # https://sentinels.copernicus.eu/web/sentinel/technical-guides/sentinel-2-msi/level-2a/algorithm
  # Doc sen2r máscaras disponibles por defecto:
  # https://sen2r.ranghetti.info/reference/s2_mask.html
  dflt$inds <- c("NDVI", "SAVI", "EVI", "ARVI", "NDMI")
  dflt$ind <- dflt$inds[1]
  dflt$masks <- {c(
    NA,                   #1
    "nomask",             #2 nada? los bordes por el tema de diferentes resoluciones?
    "nodata",             #3 [No data/0/Negro] + [Saturated or defective/1/Rojo]
    "cloud_high_proba",   #4 "nodata" + [Cloud (high probability)/9/Blanco]
    "cloud_medium_proba", #5 "cloud_high_proba" + [Cloud (medium probability)/8/Gris claro]
    "cloud_and_shadow",   #6 "cloud_medium_proba" + [Cloud shadow/3/Marrón]
    "clear_sky",          #7 "cloud_and_shadow" + [Unclassified/7/Gris] + [Thin cirrus/10/Azul claro]
    "land",               #8 "clear_sky" + [Dark area/2/Gris oscuro] + [Water/6/Azul] + [Snow/11/Rosa]
    # Sólo quedan [Vegetation/4/Verde] + [Bare soil/5/Amarillo]
    "scl_7_8_9")}
  dflt$mask <- dflt$masks[9]
  dflt$aggs <- c("Mean", "Median")
  dflt$agg <- dflt$aggs[1]

  dflt$idvs <- function() listDF2DF(Verdor$readSumInd())
}

# Series de datos y resumenes integrados
{
  dirs$int <- "int"
  paths$int <- function(int = NULL, crop = NULL,
                        ind = NULL, mask = NULL, agg = NULL, contrato = NULL) {
    if (any(vapply(list(int,crop,ind,mask,agg),is.null,F)))
      return(newDir(paths$data(), dirs$int))
    if (is.null(contrato))
      return(newDir(paths$int(), paste(int,crop,ind,mask,agg,sep="_")))
    return(newFile(paths$int(int, crop, ind, mask, agg), contrato, "csv"))
  }

  paths$intR <- function(int = dflt$int, crop = dflt$crop,
                         ind = dflt$ind, mask = dflt$mask, agg = dflt$agg)
    newFile(paths$int(), paste(int,crop,ind,mask,agg, sep="_"), "csv")


  dflt$ints <- c("redimensionar", "agregar")
  dflt$ints <- c("redim", "agg")
  dflt$int <- dflt$ints[1]


  dflt$datos <- function() listDF2DF(DatInt$readDatInt(NULL))


  # FICHERO CON LAS PARCELAS DE ENTRADA DE sen2r
  paths$jardiR <- function()
    newFile(paths$data(), paste("jardines","riego", sep="_"), "geojson")

  # paths$jardiR2 <- function() {
  #   file_name <- paste("jardines","riego","verdor", sep="_")
  #   newFile(paths$data(), file_name, "geojson")
  #   # newFile(paths$data(), file_name, "csv")
  # }

  paths$jardiR2 <- function(ind = dflt$ind, mask = dflt$mask, agg = dflt$agg)
    newFile(paths$data(), paste("jardines","riego",ind,mask,agg, sep="_"), "geojson")


  dflt$jardines <- function() DatInt$leerResumenJardines(version = 2)
  # PARCELAS DE ENTRADA DE sen2r
  dflt$jardinesSen2 <- function()
    # DatInt$leerResumenJardines(maxCD = "LecturasNoActuales", version = 1)
    DatInt$leerResumenJardines(maxCD = "VariosContratos", version = 1)
}

# Clusters y Modelos de aprendizaje
{
  dirs$clu <- "clust"
  paths$clu <- function(cjto = NULL, serie = NULL, pkg = NULL, diss = NULL) {
    if (is.null(cjto))
      return(newDir(paths$data(), dirs$clu))
    if (is.null(serie) || is.null(pkg))
      return(newDir(paths$clu(), paste0(cjto,collapse = "_")))
    if (pkg == "TSclust" && is.null(diss))
      stop("Indicar una medida de distancia para TSclust")
    file_name <- switch (pkg,
      "pdc" = paste(pkg,serie,sep = "_"),
      "TSclust" = paste(pkg,diss,serie,sep = "_"),
      stop("pkg no válido: ",pkg)
    )
    return(newFile(paths$clu(cjto), file_name, "csv"))
  }

  # dflt$cjtos <- c("ConSeriesNA","ConSeriesSCL","Seleccionados")
  # dflt$cjto <- dflt$cjtos[2]

  dflt$series <- c("Riego","Verdor","VerdorMulti")
  dflt$serie <- dflt$series[2]

  dflt$pkgs <- c("pdc", "TSclust")
  dflt$pkg <- dflt$pkgs[2]

  dflt$disss = {c(
    # MODEL FREE APPROACHES

    # Based on raw data
    "EUCL",         # "CORT", k = 0, deltamethod = "Euclid"
    "FRECHET",      # "CORT", k = 0, deltamethod = "Frechet"
    "DTWARP",       # "CORT", k = 0, deltamethod = "DTW"
    # Warning messages: #   1: In dtw(x, y, ...) :
    #   Argument dist.method does not usually make a difference with single-variate timeseries

    # Covering both proximity on values and on behavior # k > 0
    "CORT.EUCL", "CORT.FRECHET", "CORT.DTW",
    # Warning messages: #   1: In dtw(x, y, ...) :
    #   Argument dist.method does not usually make a difference with single-variate timeseries

    # Based on correlations # beta > 0
    "COR.1", "COR.2.0", "COR.2.1", "COR.2.2",

    # Based on simple and partial autocorrelations # 0 < p < 1, ¿lag.max?
    "ACFU", "ACFG", "PACFU", "PACFG",

    # Based on periodograms
    "PER", "NORM.PER", "LOG.NORM.PER",
    "INT.PER", "NORM.INT.PER",

    # Based on nonparametric spectral estimators # 0 < alpha < 1 # ¿n?
    "SPEC.WLS", "SPEC.WLK", "SPEC.GLK", "SPEC.ISD",

    # Based on the discrete wavelet transform
    # "DWT" # DWT Distance Temporarily not supported

    # Based on symbolic representation # w in N and < n # alpha
    "MINDIST.SAX",

    # MODEL BASED APPROACHES # ¿order?, ¿permissive?, ¿seasonal?
    "AR.PIC", "AR.MAH", "AR.MAHext", "AR.LPC.CEPS",
    # Warning messages: #   1: In cepstral(y, k, order.y, seasonal.y, permissive) :
    #   Cepstral distance, error on the selection of the AR order, 0 by AIC, forcing 1

    # COMPLEXITY BASED APPROACHES  # type = "gzip", "bzip2" or "xz"
    # Since the compression methods are character-based, a symbolic representation can be used, see details for an example using SAX as the symbolic representation
    "NCD", "NCDsym", "CDM", "CDMsym",
    "PDC", "PDCext", "CID"

    # PREDICTION BASED APPROACHES
    # "PRED"
  )}
  dflt$diss <- dflt$disss[1]

  dirs$res <- "result"
  paths$res <- function(int = NULL, crop = NULL, cjto = NULL,
                        model = NULL, pkg = NULL, serie = NULL,
                        diss = NULL) {
    if (any(vapply(list(int,crop,cjto),is.null,F)))
      return(newDir(paths$data(), dirs$res))
    if (is.null(model))
      return(newDir(paths$res(), paste0(c(int,crop,cjto),collapse = "_")))
    if (is.null(pkg))
      return(newDir(paths$res(int, crop, cjto), model))
    if (is.na(pkg))
      return(newFile(paths$res(int, crop, cjto), model, "csv"))
    if (pkg != "None" && is.null(serie))
      stop("Indicar una serie para el clustering")
    if (pkg == "TSclust" && is.null(diss))
      stop("Indicar una medida de distancia para TSclust")
    file_name <- switch(pkg,
      "None" = paste(pkg, sep = "_"),
      "pdc" = paste(pkg,serie, sep = "_"),
      "TSclust" = paste(pkg,diss,serie, sep = "_"),
      stop("pkg no válido: ",pkg)
    )
    return(newFile(paths$res(int, crop, cjto, model), file_name, "csv"))
  }

  dflt$models = c("glm","rpart","knn","svmRadial","gbm","rf")
  dflt$model = dflt$models[6]

}

# Tables & Plots
{
  dirs$saves <- "saves"
  paths$saves <- function(lan = dflt$lan) {
    if (is.null(lan))
      return(newDir(paths$home(), dirs$saves))
    return(newDir(paths$saves(NULL), lan))
  }

  dirs$ramps <- "colorRamps"
  paths$ramps <- function(ind = dflt$ind) {
    if (is.null(ind))
      return(newDir(paths$home(), dirs$ramps))
    return(newFile(paths$ramps(NULL), ind, "txt"))
  }

  dirs$params <- "paramsTS"
  paths$params <- function(lan = dflt$lan, param = "Ele") {
    if (is.null(lan))
      return(newDir(paths$home(), dirs$params))
    if (is.null(param))
      return(newDir(paths$params(NULL, NULL), lan))
    return(newFile(paths$params(lan, NULL), param, "txt"))
  }

  dflt$lan <- "en"

  dflt$cols <- {list(
    red = scales::hue_pal()(1)[1],
    green = scales::hue_pal()(3)[2],
    blue = scales::hue_pal()(3)[3],
    purple = scales::hue_pal()(4)[4],
    magent = scales::hue_pal()(6)[6],
    pink = scales::hue_pal()(8)[8],
    dirt = scales::hue_pal()(9)[2]
  )}

  dflt$units <- {list(
    time = c("sec","min","hour"),
    date = c("day","week","month","quarter","year"),
    datetime = c("sec","min","hour","day","week","month","quarter","year"),
    sp = list(
      time = c("Segundo","Minuto","Hora"),
      date = c("Día","Semana","Mes","Trimestre","Año"),
      datetime = c("Segundo","Minuto","Hora","Día","Semana","Mes","Trimestre","Año")
    ),
    en = list(
      time = c("Second","Minute","Hour"),
      date = c("Day","Week","Month","Quarter","Year"),
      datetime = c("Second","Minute","Hour","Day","Week","Month","Quarter","Year")
    )
  )}
}



is.subset <- function(x, y) {
  # setequal(union(x,y), y)
  # setequal(intersect(x,y), x)
  all(x %in% y)
}

basename_sans_ext <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

freadDateTimeSerie <- function(fichero, ini = NULL, fin = NULL, ...) {

  obj.df <- data.table::fread(fichero, sep = ";", ...)
  obj.df$Fecha %<>% lubridate::as_datetime()
  if (!is.null(ini)) obj.df %<>% dplyr::filter(as.POSIXct(ini) <= Fecha)
  if (!is.null(fin)) obj.df %<>% dplyr::filter(Fecha < as.POSIXct(fin))
  return(obj.df)
}

freadDateSerie <- function(fichero, ini = NULL, fin = NULL, ...) {

  obj.df <- data.table::fread(fichero, sep = ";", ...)
  obj.df$Fecha %<>% lubridate::as_date()
  if (!is.null(ini)) obj.df %<>% dplyr::filter(ini <= Fecha)
  if (!is.null(fin)) obj.df %<>% dplyr::filter(Fecha < fin)
  obj.df %<>% dplyr::distinct() %>% dplyr::arrange(Fecha)
  if (length(obj.df$Fecha) != dplyr::n_distinct(obj.df$Fecha))
    warning("Fechas duplicadas: ", fichero)
  return(obj.df)
}

freadResumen <- function(fichero) {
  data.table::fread(fichero, sep = ";") %>%
    dplyr::mutate_at("Contrato", as.character) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Fecha_")), as.Date) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Tiempo_")), lubridate::seconds_to_period)
}

streadGeoJSON <- function(fichero,
                          estaciones = dflt$eemm) {
  eemm <- c("Principal","Alternativa")
  ids <- c("Acometida","Contrato","Contador","Direccion")
  sf::st_read(fichero, drivers = "GeoJSON", quiet = T, stringsAsFactors = F) %>%
    dplyr::rename(Geometry = geometry) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(eemm)),
                     factor, levels = estaciones) %>%
    dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with(ids)), as.character) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Fecha_")), as.Date) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Tiempo_")), lubridate::seconds_to_period)
}

xts2dt <- function(obj.xts) {
  data.table::as.data.table(obj.xts, keep.rownames = "Fecha")
}

completarFechas <- function(obj.df, from = min(obj.df$Fecha),
                            to = max(obj.df$Fecha),
                            by = if (inherits(to, "POSIXt")) "hour" else "day",
                            version = 1) {
  todas <- seq(from, to, by)
  switch (version,
          {
            as <- if (inherits(to, "POSIXt")) lubridate::as_datetime else lubridate::as_date
            faltan <- setdiff(todas, obj.df$Fecha) %>%
              as() %>%
              data.table::as.data.table() %>%
              # data.table::setDT() %>%
              dplyr::rename(Fecha = 1)
            obj.df %>% rbind(faltan, fill=T) %>%
              dplyr::arrange(Fecha)
          },
          obj.df %>% merge(data.table::data.table(Fecha = todas), all.y = T),
          obj.df %>% xts::as.xts() %>% merge(todas, join = "right") %>% xts2dt()
  )
}

casiIguales <- function(c1, c2) {
  stringr::str_detect(c1, c2) | stringr::str_detect(c2, c1)
}

#' @export
listDF2DF <- function(obj.list.df, id = c("Contrato","Estacion")[1]) {
  obj.df <- obj.list.df %>% plyr::ldply(.id = id) %>%
    dplyr::mutate_at(id, as.character) %>%
    dplyr::mutate_if(is.factor, droplevels) %>%
    # data.table::as.data.table()
    data.table::setDT()
  attr(obj.df, "param") <- attr(obj.list.df, "param")
  return(obj.df)
}

#' @export
fil2col <- function(obj.df, id = c("Contrato","Estacion")[1], vr = NA) {
  if (!is.na(vr))
    obj.df %<>% dplyr::select(tidyselect::all_of(c(id,"Fecha",vr)))
  obj.df %>% tidyr::pivot_wider(names_prefix = if (!is.na(vr)) paste0(vr,'_'),
                                names_from = !!rlang::sym(id),
                                values_from = !c(!!rlang::sym(id),Fecha)) %>%
    # data.table::as.data.table()
    data.table::setDT()
}

col2fil <- function(obj.df, id = c("Contrato","Estacion")[1], vr = NA) {
  obj.df %>% tidyr::pivot_longer(cols = !Fecha,
                                 names_sep = if(is.na(vr)) "_(?![^_]*_)",
                                 names_to = c(if(is.na(vr))".value",id),
                                 values_to = if (!is.na(vr)) vr) %>%
    # data.table::as.data.table()
    data.table::setDT()
}

agregarPorFecha <- function(obj.df) {

  variables <- colnames(obj.df)

  obj.df %<>% dplyr::group_by(Fecha)
  mins <- obj.df %>% dplyr::summarise(across(ends_with("min"),
                                             ~suppressWarnings(min(.x, na.rm = T))))
  maxs <- obj.df %>% dplyr::summarise(across(ends_with("max"),
                                             ~suppressWarnings(max(.x, na.rm = T))))
  meds <- obj.df %>% dplyr::summarise(across(ends_with("med"),
                                             ~mean(.x, na.rm = T)))
  sums <- obj.df %>% dplyr::summarise(across(!ends_with(c("min", "max", "med")),
                                             ~sum(.x, na.rm = T)))
  obj.df <- list(mins, maxs, meds, sums) %>%
    purrr::reduce(merge) %>%
    # data.table::as.data.table()
    data.table::setDT()

  if (all(c("Tmed", "HRmed", "PtoRocio") %in% variables))
    obj.df %<>% dplyr::mutate(
      PtoRocio = weathermetrics::humidity.to.dewpoint(HRmed, Tmed, "celsius"))

  obj.df %>% dplyr::select(dplyr::all_of(variables))
}

allNA <- function(v) all(is.na(v))
nallNA <- function(v) !allNA(v)
nanyNA <- function(v) !anyNA(v)
numNA <- function(v) sum(is.na(v))
numNoNA <- function(v) sum(!is.na(v))
num0 <- function(v) sum(v == 0, na.rm = T)
numP <- function(v) sum(v > 0, na.rm = T)
numN <- function(v) sum(v < 0, na.rm = T)

resumir <- function(obj.df, vr = "Fecha",
                    n = switch(vr,
                      "Fecha" = c("Min","Max"),
                      "Tiempo" = c("Min","Mean","Max"),
                      c("Min","Mean","Max","TotalNA","MaxNA")),
                    gb = T) {

  if (gb) obj.df %<>% dplyr::group_by(Contrato)

  fns <- list(
    "Min" = ~min(.x, na.rm = T),
    "Q1" = ~stats::quantile(.x, 0.25, na.rm = T),
    "Mean" = ~mean(.x, na.rm = T),
    "Median" = ~stats::median(.x, na.rm = T),
    "Q3" = ~stats::quantile(.x, 0.75, na.rm = T),
    "Max" = ~max(.x, na.rm = T),
    "TotalNoNA" = ~sum(!is.na(.x)),
    "TotalNA" = ~sum(is.na(.x)),
    "MaxNA" = ~ifelse(sum(is.na(.x)) == 0, 0L,
                      with(rle(is.na(.x)), max(lengths[values]))),
    "Total0" = ~sum(.x == 0, na.rm = T),
    "TotalP" = ~sum(.x > 0, na.rm = T),
    "TotalN" = ~sum(.x < 0, na.rm = T),
    "TotalD"  = ~dplyr::n_distinct(.x, na.rm = T),
    "Total"    = ~dplyr::n()
  )

  obj.df %>% dplyr::summarise(across(all_of(vr), fns[n])) %>%
    dplyr::arrange(as.character(Contrato)) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Fecha_")), lubridate::date) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Tiempo_")), as.integer)
}

n.digits <- function(num) floor(log10(abs(num))) + 1

# trunc <- function(x) ifelse(x >= 0, floor(x), ceiling(x))
cnurt <- function(x) ifelse(x >= 0, ceiling(x), floor(x))

my.floor   <- function(x, digits = 0)   floor(x*10^digits)*10^(-digits)
my.ceiling <- function(x, digits = 0) ceiling(x*10^digits)*10^(-digits)
my.trunc   <- function(x, digits = 0)   trunc(x*10^digits)*10^(-digits)
# my.trunc   <- function(x, digits = 0) ifelse(x >= 0, my.floor(x, digits), my.ceiling(x, digits))
my.cnurt   <- function(x, digits = 0)   cnurt(x*10^digits)*10^(-digits)
# my.cnurt   <- function(x, digits = 0) ifelse(x >= 0, my.ceiling(x, digits), my.floor(x, digits))

floor.instant   <- function(x, units = c("sec","min","hour","day","week","month","quarter","year")[1]) {
  units %<>% factor(levels = dflt$units$datetime, ordered = T)
  if (units >= "min")     lubridate::second(x) <- 0
  if (units >= "hour")    lubridate::minute(x) <- 0
  if (units >= "day")     lubridate::hour(x)   <- 0
  if (units == "week")    lubridate::day(x) <- lubridate::day(x) - lubridate::wday(x, week_start = 1) + 1
  if (units >= "month")   lubridate::day(x) <- 1
  if (units >= "quarter") lubridate::month(x) <- (lubridate::month(x)-1) %/% 3 * 3 + 1
  if (units >= "year")    lubridate::month(x) <- 1
  return(x)
}
ceiling.instant <- function(x, units = c("sec","min","hour","day","week","month","quarter","year")[1]) {
  x %<>% floor.instant(units)
  switch (units,
    "sec"     = {},
    "min"     = lubridate::minute(x) <- lubridate::minute(x) + 1,
    "hour"    = lubridate::hour(x)   <- lubridate::hour(x)   + 1,
    "day"     = lubridate::day(x)    <- lubridate::day(x)    + 1,
    "week"    = lubridate::day(x)    <- lubridate::day(x)    + 7,
    "month"   = lubridate::month(x)  <- lubridate::month(x)  + 1,
    "quarter" = lubridate::month(x)  <- lubridate::month(x)  + 3,
    "year"    = lubridate::year(x)   <- lubridate::year(x)   + 1,
    stop("units no válido: ",units)
  )
  return(x)
}

#' @export
escribirTablaLatex <- function(tabla, nombre_fichero, row.names = T,
                               lan = dflt$lan, carpeta = paths$saves(lan)) {
  fichero <- newFile(carpeta, nombre_fichero, "txt")
  msg("Guardando tabla ",fichero)
  write.table(tabla, fichero, quote = F, sep = " & ", eol = " \\\\\n", dec = ",",
              row.names = row.names, col.names = if (row.names) NA else T)
}

#####

# hibrido sapply y mclapply
mcsapply <- function(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE,
                     mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE,
                     mc.cores = if (Sys.info()["sysname"] == "Windows") 1L else
                       getOption("mc.cores", 2L),
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE, affinity.list = NULL) {
  FUN <- match.fun(FUN)
  answer <- parallel::mclapply(X, FUN, ...,
                               mc.preschedule = mc.preschedule,
                               mc.set.seed = mc.set.seed,
                               mc.silent = mc.silent,
                               mc.cores = mc.cores,
                               mc.cleanup = mc.cleanup,
                               mc.allow.recursive = mc.allow.recursive,
                               affinity.list = affinity.list)
  if (USE.NAMES && is.character(X) && is.null(names(answer)))
    names(answer) <- X
  if (!isFALSE(simplify) && length(answer))
    simplify2array(answer, higher = (simplify == "array"))
  else answer
}

dflt$cores <- ceiling(parallel::detectCores()/2)
dflt$mc.cores <- function()
  if (Sys.info()["sysname"] == "Windows") 1L else dflt$cores

my.lapply <- function(X, FUN, ...) {
  # cl <- parallel::makePSOCKcluster(dflt$cores)
  # on.exit(parallel::stopCluster(cl))
  # Opcion 1
  # parallel::clusterEvalQ(cl, {
  #   wd <- if (Sys.info()["sysname"] == "Windows") "." else
  #     file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  #   source(file.path(wd,"code","include.R"), encoding = "UTF-8")
  # })
  # Opcion 2
  # parallel::clusterEvalQ(cl, {
  #   library(magrittr, warn.conflicts = F, verbose = F, quietly = T) # %>%, %<>%
  # })
  # parallel::clusterExport(cl, ls(.GlobalEnv))

  # parallel::clusterApplyLB(cl, X, FUN, ...)
  parallel::mclapply(X, FUN, ..., mc.cores = dflt$mc.cores())
}

my.sapply <- function(X, FUN, ...,
                      simplify = TRUE, USE.NAMES = TRUE) {
  mcsapply(X, FUN, ..., simplify = simplify, USE.NAMES = USE.NAMES,
           mc.cores = dflt$mc.cores())
}

my.mapply <- function(FUN, ..., MoreArgs = NULL,
                      SIMPLIFY = TRUE, USE.NAMES = TRUE) {
  parallel::mcmapply(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = SIMPLIFY,
                     USE.NAMES = USE.NAMES, mc.cores = dflt$mc.cores())
}



