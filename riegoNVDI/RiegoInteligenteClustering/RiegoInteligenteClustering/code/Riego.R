if (!exists("paths") || is.null(paths)) {
  wd <- if (Sys.info()["sysname"] == "Windows") "." else
    file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  source(file.path(wd,"code","include.R"), encoding = "UTF-8")
}



#' Datos de riego
#'
#' @param host
#' @param pwd
#' @param nodename
#' @param contrato(s)
#'   (Vector de) identificador(es) (`strings`) de contrato(s) de interés.
#' @param ini,fin
#' @name riego
Riego <- list(
  nodename = "dibulibu",
  host = "aurora@155.54.204.34",
  pwd = "aauroralegustaelagua",
  ini = as.Date("2017-09-29"),
  fin = Sys.Date(),
  iniT = as.POSIXct("2017-09-29", "UTC"),
  finT = Sys.time(),
  frecL = "hour",
  frecC = "day",
  # ref = lubridate::as_datetime(0),
  ref = as.POSIXct("2017-01-01", "UTC"),
  version = 2,
  tiposL = c(Expl2="character",Contador="character",
             Fecha="POSIXct",Lectura="integer"),
  tiposC = c(Fecha="Date",ConsumoTotal="numeric",Consumo="numeric"),
  pattern = function(dias = '[0-9]{8}') {
    dias %<>% paste0(collapse = "|")
    paste0('Lecturas_(',dias,')[_0-9]+\\.csv(_big)?\\.zip')
  }
)

###############################

#' @export
#' @rdname riego
Riego$conectarServRiego <- function(host = Riego$host,
                                    pwd = Riego$pwd) {
  ssh::ssh_connect(host, passwd = pwd)
}

####
Riego$parseDays <- function(filesAgua) {
  filesAgua %>% stringr::str_extract("(?<=Lecturas_)[0-9]{8}")
}
Riego$parseDates <- function(filesAgua) {
  filesAgua %>% Riego$parseDays() %>% lubridate::ymd()
}

#'
#' @return Nada
#' @rdname riego
Riego$lsFilesAgua <- function(host = Riego$host,
                              pwd = Riego$pwd,
                              nodename = Riego$nodename) {
  msg("Listando ficheros de lecturas comprimidas...")

  if (Sys.info()["nodename"] != nodename) {
    session <- ssh::ssh_connect(host, passwd = pwd)
    on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  }
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)
  dflt$node <<- nodename; ruta <- paths$agua()
  comando <- paste0('ls -sh ',ruta,' | tail -n +2')
  listado <- if (Sys.info()["nodename"] == nodename)
      system(comando, intern = T) else
        capture.output(status <- ssh::ssh_exec_wait(session, comando))
        # sys::as_text(ssh::ssh_exec_internal(session, comando)$stdout)

  listado %<>% stringr::str_squish() %>%
    stringr::str_split(" ", simplify = T) %>%
    data.table::data.table() %>%
    dplyr::rename(Tamano = 1, Fichero = 2)
  msg("Guardando listado de ficheros de lecturas comprimidas...")
  dflt$node <<- old_node; fichero <- paths$aguaLS()
  data.table::fwrite(listado, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
Riego$getFilesAgua <- function(update = F) {

  fichero = paths$aguaLS()
  if (!file.exists(fichero) || update)
    Riego$lsFilesAgua()

  data.table::fread(fichero, sep = ";") %>%
    dplyr::mutate(Fecha = Riego$parseDates(Fichero)) %>%
    completarFechas()
}

#' @param consulta Una de `c("fechasRepe", "fechasFalt", "fechasVacias", "fechas")`
#' @export
Riego$consultarFilesAgua <- function(consulta = "fechasFalt",
                                     valor = switch (consulta,
                                      "fechasVacias" = "4,0K",
                                      "dias" = "202010"
                                     ),
                                     listado = Riego$getFilesAgua()) {
  # File 'Lecturas_20210908070711.csv' has size 0.
  switch(consulta,
    "fechasRepe" = {
      fechas <- listado %>% dplyr::count(Fecha) %>% dplyr::filter(n > 1)
      listado %>% dplyr::filter(Fecha %in% fechas$Fecha)
    },
    "fechasFalt" =
      listado %>% dplyr::filter(is.na(Fichero)),
    "fechasVacias" =
      listado %>% dplyr::filter(Tamano == valor),
    "dias" =
      listado %>% dplyr::filter(stringr::str_starts(Fichero,paste0("Lecturas_",valor))),
    stop("Consulta no válida: ", consulta)
  )
}

#'
#' @return Nada
#' @export
#' @rdname riego
Riego$downloadAgua <- function(dias = "202010",
                               host = Riego$host,
                               pwd = Riego$pwd,
                               nodename = Riego$nodename) {

  session <- ssh::ssh_connect(host, passwd = pwd)
  on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)

  msg("Descargando lecturas comprimidas...")
  if (is.null(dias)) {
    dflt$node <<- old_node; dst <- paths$riego()
    dflt$node <<- nodename; src <- paths$agua()
    out <- capture.output(
      status <- ssh::scp_download(session, src, dst)
    )
  } else {
    dflt$node <<- old_node; dst  <- paths$agua()
    dflt$node <<- nodename; srcs <- paths$agua(dias)
    out <- sapply(srcs, function(src) {
      capture.output(
        status <- ssh::scp_download(session, src, dst)
      )
    }, USE.NAMES = F)
  }
  msg(sum(tools::file_ext(out) != "")," ficheros descargados")

  invisible(out)
}

#' @return Un `data.table` con...
#' @export
#' @rdname riego
Riego$leerAgua <- function(name.zip) {
  name.csv <- name.zip %>% stringr::str_extract(".*\\.csv")

  unzip(file.path(paths$agua(),name.zip))
  agua <- data.table::fread(name.csv, sep = ";", encoding = 'UTF-8')
  # aurorax, por un warning he decidio hacer install.packages("bit64")
  file.remove(name.csv)

  if (!plyr::empty(agua)) {

    colnames(agua) <- c("Expl1", "Expl2", "Contador", "Contrato",
                        "Fecha", "Lectura", "Sobra")

    agua$Fecha %<>% lubridate::dmy_hms()

    # Sobra se genera por un fallo de codificacion
    agua$Sobra <- NULL
    # Expl1 no hace falta (siempre vale EMUASA-MURCIA para nuestros contratos)
    agua$Expl1 <- NULL
  }

  return(agua)
}

###############################
# Fichero 3.1. Lectura

#'
#' @return los contratos para los que ha escrito nuevas lecturas
#' @rdname riego
Riego$extraerLecturas <- function(name.zip, contratos = dflt$contratos(), expl2 = F) {

  agua <- Riego$leerAgua(name.zip)

  if (plyr::empty(agua))
    return(c())

  if (!expl2) agua$Expl2 <- NULL
  agua %<>% dplyr::filter(Contrato %in% contratos)

  ###### añadimos las lecturas de cada contador en el fichero correspondiente
  lecturas <- split(agua, by = "Contrato", keep.by = F)
  contratos <- names(lecturas)
  ficheros <- paths$lects(contratos)
  mapply(data.table::fwrite, lecturas, ficheros, append = T, sep = ";", na = "NA")

  return(contratos)
}

#'
#' @return los contratos para los que ha escrito nuevas lecturas
#' @rdname riego
Riego$updateLecturas <- function(contratos = dflt$contratos(),
                                 initRes = F) {

  msg("Descomprimiendo, extrayendo y guardando NUEVAS lecturas...")
  # guardamos los contratos que tienen nuevas lecturas
  actualizados <- c()
  # todos los zip, los procesados y los nuevos
  allFiles <- list.files(paths$agua())
  fichero = paths$proc()
  processedFiles <- data.table::fread(fichero)$Processed
  newFiles <- setdiff(allFiles, processedFiles)
  if (length(newFiles) > 0) {
    msg("Procesando ",length(newFiles)," ficheros comprimidos...")
    actualizados <- my.lapply(newFiles, function(name.zip) {
      # msg("#######     Processing",name.zip,"     #######")
      Riego$extraerLecturas(name.zip, contratos, T)
    })
    actualizados %<>% purrr::reduce(union)
    msg("Lecturas de ",length(actualizados)," contratos actualizadas...")
    data.table::fwrite(data.frame(newFiles), fichero, append = T)
  }

  if (initRes) {
    msg("Leyendo y resumiendo lecturas de contadores...")
    lecturas <- listDF2DF(Riego$readLecturas(expl2 = T))
    Riego$resumirContadores(lecturas = lecturas)
    Riego$resumirLecturas(lecturas = lecturas)
  }

  return(actualizados)
}

#'
#' @return Nada
#' @rdname riego
Riego$cleanLecturas <- function(contratos = dflt$contratos("conLecturas"),
                                rmDup = T, rmErr = F, rmRed = F) {

  msg("Limpiando lecturas de ",length(contratos)," contratos...")
  my.lapply(contratos, function(contrato) {
    fichero <- paths$lects(contrato)
    lectura <- Riego$leerLecturas(contrato, T, debug = T,
                                  rmDup = rmDup, rmErr = rmErr, rmRed = rmRed)
    if (plyr::empty(lectura)) {
      msg(contrato,": eliminando fichero lecturas vacio...")
      file.remove(fichero)
    } else {
      lectura %<>% dplyr::select(Expl2, Contador, Fecha, Lectura)
      data.table::fwrite(lectura, fichero, sep = ";", na = "NA")
    }
  }) %>% invisible()
}

#'
#' @return Nada
#' @rdname riego
Riego$downloadLecturas <- function(contratos = NULL,
                                   host = Riego$host,
                                   pwd = Riego$pwd,
                                   nodename = Riego$nodename,
                                   downloadRes = T) {

  session <- ssh::ssh_connect(host, passwd = pwd)
  on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)

  msg("Descargando lecturas...")
  if (is.null(contratos)) {
    dflt$node <<- old_node; dst <- paths$riego()
    dflt$node <<- nodename; src <- paths$lects()
    out <- capture.output(
      status <- ssh::scp_download(session, src, dst)
    )
  } else {
    dflt$node <<- old_node; dst  <- paths$lects()
    dflt$node <<- nodename; srcs <- paths$lects(contratos)
    out <- vapply(srcs, function(src) {
      capture.output(
        status <- ssh::scp_download(session, src, dst)
      )
    }, "", USE.NAMES = F)
  }
  msg(sum(tools::file_ext(out) != "")," ficheros descargados")

  if (downloadRes) {
    msg("Descargando resumenes de contadores y lecturas...")
    dflt$node <<- old_node; dst <- paths$riego()
    dflt$node <<- nodename; src <- paths$contR()
    out %<>% capture.output(
      status <- ssh::scp_download(session, src, dst)
    )
    dflt$node <<- nodename; src <- paths$lectR()
    out %<>% c(capture.output(
      status <- ssh::scp_download(session, src, dst)
    ))
  }

  invisible(out)
}

#' @return Un `data.table` con...
#' @export
#' @rdname riego
Riego$leerLecturas <- function(contrato, expl2 = F, contadores = T,
                               rmDup = T, rmErr = T, rmRed = F,
                               ultimoContador = F,
                               ini = NULL, fin = NULL,
                               debug = F) {

  fichero <- paths$lects(contrato, T)
  lectura <- freadDateTimeSerie(fichero, ini, fin, colClasses = Riego$tiposL)

  if (ultimoContador) {
    contador <- lectura %>% dplyr::slice_max(Fecha) %>% dplyr::pull(Contador)
    lectura %<>% dplyr::filter(casiIguales(Contador, contador))
  }

  if (rmDup) {
    n1 <- nrow(lectura)
    # debemos ordenar las fechas y eliminar duplicados
    lectura %<>% dplyr::distinct() %>% dplyr::arrange(Fecha)
    n2 <- nrow(lectura)
    if (debug && n1 > n2)
      msg(contrato,": ",n1-n2," lecturas duplicadas")

    if (rmErr) {
      # a veces tenemos 2 lecturas distintas para el mismo instante
      n1 <- nrow(lectura)
      lectura %<>% dplyr::mutate(
        Tiempo = lubridate::as.duration(dplyr::lead(Fecha) - Fecha)
      ) %>% dplyr::distinct(Fecha, .keep_all = T) %>%
        dplyr::filter(is.na(Tiempo) | Tiempo != 0)
      n2 <- nrow(lectura)
      if (debug && n1 > n2)
        msg(contrato,": ",n1-n2," lecturas erroneas")
    }

    if (rmRed) {
      #### Eliminando lecturas redundantes
      n1 <- nrow(lectura)
      lectura %<>% dplyr::filter(
        tidyr::replace_na(dplyr::lead(Lectura) != dplyr::lag(Lectura), T)
      )
      n2 <- nrow(lectura)
      if (debug && n1 > n2)
        msg(contrato,": ",n1-n2," lecturas redundantes")
    }
  }

  # if (debug)
  #   msg(contrato,": ",nrow(lectura)," lecturas")


  if (expl2 && contadores)
    lectura$Expl2 %<>% as.factor()
  else if (expl2 && !contadores)
    lectura$Expl2 <- NULL

  if (contadores)
    lectura %<>% dplyr::mutate(
      CambiaContador = !casiIguales(dplyr::lead(Contador), Contador),
    ) %>% dplyr::mutate_at(c("Contador"), as.character)

  lectura %>% dplyr::mutate(
    # PASAMOS DE LITROS A METROS CUBICOS
    # Lectura = Lectura / 1000,
    Tiempo = lubridate::as.duration(dplyr::lead(Fecha) - Fecha),
    Consumo = dplyr::lead(Lectura) - Lectura
  )
}

#' @return Una `list` de `data.table` con...
#' @export
#' @rdname riego
Riego$readLecturas <- function(contratos = dflt$contratos("conLecturas"), ...) {

  if (is.null(contratos))
    contratos <- dflt$contratos("conLecturas")
  else
    contratos %<>% intersect(dflt$contratos("conLecturas"))
  lecturas <- my.lapply(contratos, Riego$leerLecturas, ...)
  names(lecturas) <- contratos
  lecturas
}

#'
#' @return Nada
#' @rdname riego
Riego$resumirContadores <- function(fin = dflt$fin,
                                    # dg = DatGeo$leerRelaciones(objetivo = "consumo"),
                                    dg = dflt$dg() %>% sf::st_drop_geometry() %>%
                                      dplyr::filter(!is.na(Contrato)),
                                    lecturas = dflt$lecturas()) {
  msg("Inicializando resumen de contadores...")
  lecturas %<>% dplyr::filter(Fecha < fin)

  lect <- lecturas %>% dplyr::select(Contrato,Contador) %>% dplyr::distinct()
  falta3 <- dg %>% dplyr::anti_join(lect, by = "Contrato")
  variosContadores <- lect %>% dplyr::count(Contrato) %>% dplyr::filter(n > 1)

  lect.last <- lecturas %>% dplyr::group_by(Contrato) %>%
    dplyr::slice_tail() %>% dplyr::select(Contrato,Contador) %>% dplyr::ungroup()
  dt3.last <- dg %>%
    merge(lect.last, by = "Contrato", suffixes = c(".dg", ".lect")) %>%
    dplyr::select(Contrato, Contador.dg, Contador.lect) %>%
    dplyr::mutate(Iguales = Contador.dg == Contador.lect,
                  CasiIguales = casiIguales(Contador.lect, Contador.dg))

  contadoresR <- dplyr::bind_rows(
    "Ninguno" = falta3 %>%
      dplyr::rename(Contador.dg = Contador) %>%
      dplyr::select(Contrato, Contador.dg),
    "Uno" = dt3.last %>%
      dplyr::filter(!(Contrato %in% variosContadores$Contrato)),
    "Varios" = dt3.last %>%
      dplyr::filter(Contrato %in% variosContadores$Contrato),
    .id = "TieneContadorEnLect") %>%
    dplyr::mutate(
      Distintos = !CasiIguales,
      CasiIguales = CasiIguales & !Iguales) %>%
    dplyr::mutate(
      Contadores = apply(matrix(c(Iguales,CasiIguales,Distintos),nrow=dplyr::n()),
                         MARGIN = 1, FUN = function(v) {
                           if (allNA(v)) NA
                           else which(v)
                         }),
      .before = Contrato) %>%
    dplyr::select(!c(Iguales,CasiIguales,Distintos)) %>%
    dplyr::mutate_at(c("TieneContadorEnLect","Contadores"), as.factor)
  levels(contadoresR$Contadores) <- c("Iguales","CasiIguales","Distintos")

  msg("Guardando resumen contadores...")
  fichero = paths$contR()
  data.table::fwrite(contadoresR, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
Riego$leerResumenContadores <- function(update = F) {

  fichero = paths$contR()
  if (!file.exists(fichero) || update)
    Riego$resumirContadores()

  freadResumen(fichero) %>%
    dplyr::mutate_at(c("TieneContadorEnLect","Contadores"), as.factor)
}

####
#' @param consulta Una de `c("tabla", "sinLecturas", "variosContadores")`
#' @export
Riego$consultarResumenContadores <- function(consulta = "tabla",
                                             res = Riego$leerResumenContadores()) {
  switch(consulta,
    "tabla" = {
      tabla <- table(res$TieneContadorEnLect, res$Contadores, useNA = "ifany")
      Total <- apply(tabla, 1, sum)
      tabla %<>% cbind(Total)
      tabla[,c((ncol(tabla)-1):1,ncol(tabla))]
    },
    "sinLecturas" =
      res %>% dplyr::filter(TieneContadorEnLect == "Ninguno"),
    "conLecturas" =
      res %>% dplyr::filter(TieneContadorEnLect != "Ninguno"),
    "variosContadores" =
      res %>% dplyr::filter(TieneContadorEnLect == "Varios"),
    "unContador" =
      res %>% dplyr::filter(TieneContadorEnLect == "Uno"),
    stop("Consulta no válida: ", consulta)
  )
}

#'
#' @return Nada
#' @rdname riego
Riego$resumirLecturas <- function(ini = dflt$ini, fin = dflt$fin,
                                  lecturas = dflt$lecturas()) {
  msg("Inicializando resumen de lecturas...")

  lecturasRfechas <- lecturas %>%
    resumir("Fecha")
  lecturasRtiempos0 <- lecturas %>%
    dplyr::filter(Fecha >= ini, Fecha < fin, Consumo != 0) %>%
    resumir("Tiempo")
  lecturasRconsumo <- lecturas %>%
    # dplyr::filter(Fecha >= ini, Fecha < fin) %>%
    dplyr::filter(Fecha < fin) %>%
    resumir("Consumo",  c("Min","Max"))

  lecturasRls <- list(lecturasRfechas, lecturasRtiempos0, lecturasRconsumo)
  lecturasR <- purrr::reduce(lecturasRls, merge, all=T)
  msg("Guardando resumen lecturas...")
  fichero = paths$lectR()
  data.table::fwrite(lecturasR, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
Riego$leerResumenLecturas <- function(update = F) {

  fichero = paths$lectR()
  if (!file.exists(fichero) || update)
    Riego$resumirLecturas()

  freadResumen(fichero)
}

####
#' @param consulta Una de `c("tardias", "noActuales", "faltantes", "negativo", "cero")`
#' @export
Riego$consultarResumenLecturas <- function(consulta, valor = switch(consulta,
                                            "noActuales" = , "actuales" = dflt$noActuales,
                                            "tardias" = , "noTardias" = dflt$tardias,
                                            "faltantes" = ,"noFaltantes" = dflt$faltantesL,
                                            "negativo" = , "noNegativo" = ,
                                            "cero" = , "noCero" = dflt$eps),
                                           res = Riego$leerResumenLecturas()) {
  switch(consulta,
    "noActuales" =
      res %>% dplyr::filter(Fecha_Max < valor) %>% dplyr::arrange(Fecha_Max),
    "actuales" =
      res %>% dplyr::filter(Fecha_Max >= valor) %>% dplyr::arrange(Fecha_Max),
    "tardias" =
      res %>% dplyr::filter(Fecha_Min >= valor) %>% dplyr::arrange(Fecha_Min),
    "noTardias" =
      res %>% dplyr::filter(Fecha_Min < valor) %>% dplyr::arrange(Fecha_Min),
    "faltantes" =
      res %>% dplyr::filter(Tiempo_Max >= valor) %>% dplyr::arrange(Tiempo_Max),
    "noFaltantes" =
      res %>% dplyr::filter(Tiempo_Max < valor) %>% dplyr::arrange(Tiempo_Max),
    "negativo" =
      res %>% dplyr::filter(Consumo_Min < valor) %>% dplyr::arrange(Consumo_Min),
    "noNegativo" =
      res %>% dplyr::filter(Consumo_Min >= valor) %>% dplyr::arrange(Consumo_Min),
    "cero" =
      res %>% dplyr::filter(abs(Consumo_Min) <= valor & abs(Consumo_Max) <= valor),
    "noCero" =
      res %>% dplyr::filter(abs(Consumo_Min) > valor | abs(Consumo_Max) > valor),
    stop("Consulta no válida: ", consulta)
  )
}

###############################
# Fichero 3.2. Consumo

#' @return Un `data.table` con...
Riego$deLecturasAConsumos <- function(lect.dt, frec = Riego$frecC, ref = Riego$ref,
                                      version = Riego$version) {
  frec.secs <- frec %>% lubridate::period() %>% lubridate::period_to_seconds()

  if (plyr::empty(lect.dt))
    return(lect.dt %>% dplyr::rename(Consumo = Lectura) %>%
             dplyr::select(Fecha,Consumo))

  floor.instant.ref.frec <- function(fecha) {
    fecha.secs <- as.double(fecha-ref, units = "secs")
    if (fecha.secs %% frec.secs != 0)
      ref + (fecha.secs %/% frec.secs)*frec.secs
    else
      fecha
  }
  ceiling.instant.ref.frec <- function(fecha) {
    fecha.secs <- as.double(fecha-ref, units = "secs")
    if (fecha.secs %% frec.secs != 0)
      ref + (fecha.secs %/% frec.secs + 1)*frec.secs
    else
      fecha
  }

  from <- ceiling.instant.ref.frec(min(lect.dt$Fecha))
  # to   <- floor.instant.ref.frec(max(lect.dt$Fecha))
  to   <- max(lect.dt$Fecha)
  tiempo.total <- to - from
  if (as.double(tiempo.total, "secs") < frec.secs)
    return(lect.dt %>% dplyr::rename(Consumo = Lectura) %>%
             dplyr::slice_sample(n = 0) %>% dplyr::select(Fecha,Consumo))

  lect2cons <- function(lect.dt) {
    lect.dt %<>% dplyr::select(Fecha, Lectura)
    lect.xts <- xts::as.xts(lect.dt)

    #### Establecer la frecuencia de las lecturas (a partir de frec y ref)
    from <- lect.xts %>% xts::first() %>% zoo::index()
    to <- lect.xts %>% xts::last() %>% zoo::index()
    if (!grepl("year|month",frec)) {
      from %<>% ceiling.instant.ref.frec()
      # to   %<>% floor.instant.ref.frec()
      fechas <- seq.POSIXt(from, to, frec)
    } else {
      fechas <- seq.POSIXt(ref, to, frec)
      fechas <- fechas[fechas >= from]
    }
    lect.xts %<>% merge(fechas) %>% zoo::na.approx()
    lect.xts <- lect.xts[fechas]

    #### Pasando de lecturas a consumos
    cons.xts <- lect.xts %>% zoo::as.zoo() %>% diff(lag = -1) %>% xts::as.xts()
    names(cons.xts) <- "Consumo"
    switch(version,
           cons.xts,
           xts2dt(merge(lect.xts, cons.xts))
    )
  }

  cons.dt <- switch(version,
    {
      cons.xts <- lect2cons(lect.dt)
      if ("Contador" %in% colnames(lect.dt)) {
        lect.dt %<>% dplyr::mutate(
          CambiaContador = !casiIguales(dplyr::lead(Contador), Contador)
        )
        if (any(lect.dt$CambiaContador)) {
          ini <- lect.dt %>% dplyr::filter(CambiaContador) %>%
            dplyr::pull(Fecha) %>% as.Date()
          fin <- lect.dt %>% dplyr::filter(dplyr::lag(CambiaContador)) %>%
            dplyr::pull(Fecha) %>% as.Date()
          fechas <- paste0(ini,"/",fin)
          msg(paste0(fechas,collapse = ", ")," datos desconocidos")
          cons.xts[fechas] <- NA
        }
      }
      xts2dt(cons.xts)
    },
    {
      if ("Contador" %in% colnames(lect.dt)) {
        lect.dt.ls <- split(lect.dt, by = "Contador")
      } else {
        lect.dt.ls <- list(lect.dt)
      }
      cons.dt.ls <- lapply(lect.dt.ls, lect2cons)
      if (length(lect.dt.ls) > 1) {
        lect.dt.ls.aux <- lapply(1:(length(lect.dt.ls)-1), function(i) {
          lect.dt.aux <- rbind(tail(cons.dt.ls[[i]],n=1),
                               head(cons.dt.ls[[i+1]],n=1))
          lect.dt.aux$Lectura[2] <- tail(lect.dt.ls[[i]],n=1)$Lectura +
            lect.dt.aux$Lectura[2]
          lect.dt.aux %>% dplyr::select(Fecha, Lectura)
        })
        cons.dt.ls.aux <- lapply(lect.dt.ls.aux, lect2cons)
        cons.dt.ls %<>% append(cons.dt.ls.aux)
      }
      cons.dt.ls %<>% lapply(head, -1)
      dplyr::bind_rows(cons.dt.ls)
    }
  )
  return(cons.dt %>% dplyr::select(Fecha, Consumo) %>% dplyr::arrange(Fecha))
}

#'
#' @return Nada
#' @rdname riego
Riego$calcularConsumos <- function(contrato, area = 1, expl2 = F,
                                   frec = Riego$frecC, ref = Riego$ref,
                                   version = Riego$version) {

  lectura <- Riego$leerLecturas(contrato, expl2, debug = T)

  consumo <- lectura %>% Riego$deLecturasAConsumos(frec, ref, version) %>%
    dplyr::mutate_at("Fecha", lubridate::date) %>%
    dplyr::rename(ConsumoTotal = Consumo) %>%
    dplyr::mutate(Consumo = ConsumoTotal / area)

  fichero <- paths$conss(contrato)
  data.table::fwrite(consumo, fichero, sep = ";", na = "NA")
}

#'
#' @return Nada
#' @rdname riego
Riego$initConsumos <- function(contratos = dflt$contratos("conLecturas"),
                               frec = Riego$frecC, ref = Riego$ref,
                               version = Riego$version,
                               dg = dflt$dg() %>% sf::st_drop_geometry(),
                               initRes = F) {

  msg("Inicializando consumos de ",length(contratos)," contratos...")
  my.lapply(contratos, function(contrato) {
    area <- dg %>% dplyr::filter(Contrato == contrato) %>% dplyr::pull(Area)
    if (length(area) != 1) {
      area = 1
      msg(contrato,": area desconocida")
    }
    Riego$calcularConsumos(contrato, area, T, frec, ref, version)
  })

  if (initRes) {
    msg("Leyendo y resumiendo consumos de agua...")
    consumos <- listDF2DF(Riego$readConsumos())
    Riego$resumirConsumos(consumos = consumos)
  }
}

#' Establecer la frecuencia para un periodo
#' @return Un `data.table` con...
Riego$deLecturasAConsumosPeriodo <- function(lect.dt, ini = iniT, fin = finT,
                                             frec = Riego$frecC, ref = Riego$ref,
                                             version = Riego$version) {
  if (plyr::empty(lect.dt))
    return(lect.dt %>% dplyr::rename(Consumo = Lectura) %>%
             dplyr::select(Fecha,Consumo))

  frec.d <- frec %>% lubridate::duration()
  res <- lect.dt %>% dplyr::filter(ini-frec.d <= Fecha & Fecha <= fin+frec.d)

  anteriores <- lect.dt %>% dplyr::filter(Fecha <= ini-frec.d)
  if (plyr::empty(anteriores))
    anterior <- lect.dt %>% dplyr::slice_head() %>%
    dplyr::mutate(Fecha = ini-frec.d)
  else
    anterior <- anteriores %>% dplyr::slice_tail()
  res %<>% dplyr::add_row(anterior, .before = 1)

  posteriores <- lect.dt %>% dplyr::filter(fin+frec.d <= Fecha)
  if (plyr::empty(posteriores))
    posterior <- lect.dt %>% dplyr::slice_tail() %>%
    dplyr::mutate(Fecha = fin+frec.d)
  else
    posterior <- posteriores %>% dplyr::slice_head()
  res %<>% dplyr::add_row(posterior)

  res %>% Riego$deLecturasAConsumos(frec, ref, version) %>%
    dplyr::filter(ini <= Fecha & Fecha < fin)
}

#'
#' @return Nada
Riego$calcularConsumos2 <- function(contrato, area = 1, expl2 = F,
                                    frec = Riego$frecC, ref = Riego$ref,
                                    version = Riego$version) {

  lectura <- Riego$leerLecturas(contrato, expl2, debug = T)

  consumo <- Riego$leerConsumos(contrato)
  if (plyr::empty(consumo))
    return(Riego$calcularConsumos(contrato, area, expl2, frec, ref, version))

  # allDates <- lectura$Fecha %>% lubridate::date() %>% unique()
  # processedDates <- consumo$Fecha
  # newDates <- setdiff(allDates, processedDates) %>% sort() %>% lubridate::as_date()

  nextDate <- max(consumo$Fecha) + lubridate::period(frec)
  # if (is.na(nextDate)) {
  #   return(calcularConsumos(contrato, area, expl2, frec, ref, version))
  # }
  newDate <- lubridate::date(max(lectura$Fecha))

  # if (length(newDates) > 0) {
  if (nextDate < newDate) {

    # ini <- newDates %>% head(1)
    # fin <- newDates %>% tail(1)
    consumo <- lectura %>%
      # Riego$deLecturasAConsumosPeriodo(ini, fin, frec, ref, version) %>%
      Riego$deLecturasAConsumosPeriodo(nextDate, newDate, frec, ref, version) %>%
      dplyr::mutate_at("Fecha", lubridate::date) %>%
      dplyr::rename(ConsumoTotal = Consumo) %>%
      dplyr::mutate(Consumo = ConsumoTotal / area)

    fichero <- paths$conss(contrato)
    data.table::fwrite(consumo, fichero, append = T, sep = ";", na = "NA")

  }
}

#'
#' @return Nada
Riego$updateConsumos <- function(contratos = dflt$contratos("conLecturas"),
                                 frec = Riego$frecC, ref = Riego$ref,
                                 version = Riego$version,
                                 dg = dflt$dg() %>% sf::st_drop_geometry(),
                                 initRes = F) {

  msg("Actualizando consumos de ",length(contratos)," contratos...")
  my.lapply(contratos, function(contrato) {
    area <- dg %>% dplyr::filter(Contrato == contrato) %>% dplyr::pull(Area)
    if (length(area) != 1) {
      area = 1
      msg(contrato,": area desconocida")
    }
    Riego$calcularConsumos2(contrato, area, T, frec, ref, version)
  })

  if (initRes) {
    msg("Leyendo y resumiendo consumos de agua...")
    consumos <- listDF2DF(Riego$readConsumos())
    Riego$resumirConsumos(consumos = consumos)
  }
}

#'
#' @return Nada
#' @rdname riego
Riego$cleanConsumos <- function(contratos = dflt$contratos("conConsumos"),
                                dg = dflt$dg() %>% sf::st_drop_geometry()) {

  msg("Limpiando consumos de ",length(contratos)," contratos...")
  my.lapply(contratos, function(contrato) {
    fichero <- paths$conss(contrato)
    consumo <- Riego$leerConsumos(contrato)
    if (plyr::empty(consumo)) {
      msg(contrato,": eliminando fichero consumos vacio...")
      file.remove(fichero)
    } else {
      area <- dg %>% dplyr::filter(Contrato == contrato) %>% dplyr::pull(Area)
      if (length(area) != 1) {
        area = 1
        msg(contrato,": area desconocida")
      }
      consumo %<>% dplyr::select(Fecha, ConsumoTotal, Consumo) %>%
        dplyr::mutate(Consumo = ConsumoTotal / area)
      data.table::fwrite(consumo, fichero, sep = ";", na = "NA")
    }
  }) %>% invisible()
}

#'
#' @return Nada
#' @rdname riego
Riego$downloadConsumos <- function(contratos = NULL,
                                   host = Riego$host,
                                   pwd = Riego$pwd,
                                   nodename = Riego$nodename,
                                   downloadRes = T) {

  session <- ssh::ssh_connect(host, passwd = pwd)
  on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)
  # status <- ssh::ssh_exec_wait(s, "Rscript initConsumos.R > salida.txt 2>&1")

  msg("Descargando consumos...")
  if (is.null(contratos)) {
    dflt$node <<- old_node; dst <- paths$riego()
    dflt$node <<- nodename; src <- paths$conss()
    out <- capture.output(
      status <- ssh::scp_download(session, src, dst)
    )
  } else {
    dflt$node <<- old_node; dst  <- paths$conss()
    dflt$node <<- nodename; srcs <- paths$conss(contratos)
    out <- vapply(srcs, function(src) {
      capture.output(
        status <- ssh::scp_download(session, src, dst)
      )
    }, "", USE.NAMES = F)
  }
  msg(sum(tools::file_ext(out) != "")," ficheros descargados")

  if (downloadRes) {
    msg("Descargando resumen de consumos...")
    dflt$node <<- old_node; dst <- paths$riego()
    dflt$node <<- nodename; src <- paths$consR()
    out %<>% c(capture.output(
      status <- ssh::scp_download(session, src, dst)
    ))
  }

  invisible(out)
}

#' @return Un `data.table` con...
#' @export
#' @rdname riego
Riego$leerConsumos <- function(contrato,
                               ini = NULL, fin = NULL) {

  fichero <- paths$conss(contrato, T)
  consumo <- freadDateSerie(fichero, ini, fin, colClasses = Riego$tiposC)

  # PASAMOS DE LITROS A METROS CUBICOS
  # consumo %<>% dplyr::mutate(Consumo = Consumo / 1000)

  return(consumo)
}

#' @return Una `list` de `data.table` con...
#' @export
#' @rdname riego
Riego$readConsumos <- function(contratos = dflt$contratos("conConsumos"), ...) {

  if (is.null(contratos))
    contratos <- dflt$contratos("conConsumos")
  else
    contratos %<>% intersect(dflt$contratos("conConsumos"))
  consumos <- lapply(contratos, Riego$leerConsumos, ...)
  names(consumos) <- contratos
  consumos
}

#'
#' @return Nada
#' @rdname riego
Riego$resumirConsumos <- function(ini = dflt$ini, fin = dflt$fin,
                                  consumos = dflt$consumos()) {
  msg("Inicializando resumen de consumos...")

  consumosR <- consumos %>%
    dplyr::filter(Fecha >= ini, Fecha < fin) %>%
    resumir("Consumo")

  msg("Guardando resumen consumos...")
  fichero = paths$consR()
  data.table::fwrite(consumosR, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
Riego$leerResumenConsumos <- function(update = F) {

  fichero = paths$consR()
  if (!file.exists(fichero) || update)
    Riego$resumirConsumos()

  freadResumen(fichero)
}

####
#' @param consulta Una de `c("faltantes", "negativo", "cero")`
#' @export
Riego$consultarResumenConsumos <- function(consulta, valor = switch(consulta,
                                            "faltantes" = , "noFaltantes" = dflt$faltantesC,
                                            "negativo" = , "noNegativo" = ,
                                            "cero" = , "noCero" = dflt$eps),
                                           res = Riego$leerResumenConsumos()) {
  switch(consulta,
    "faltantes" =
      res %>% dplyr::filter(Consumo_TotalNA > valor) %>% dplyr::arrange(Consumo_TotalNA),
    "noFaltantes" =
      res %>% dplyr::filter(Consumo_TotalNA <= valor) %>% dplyr::arrange(Consumo_TotalNA),
    "negativo" =
      res %>% dplyr::filter(Consumo_Min < valor) %>% dplyr::arrange(Consumo_Min),
    "noNegativo" =
      res %>% dplyr::filter(Consumo_Min < valor) %>% dplyr::arrange(Consumo_Min),
    "cero" =
      res %>% dplyr::filter(abs(Consumo_Min) <= valor & abs(Consumo_Max) <= valor),
    "noCero" =
      res %>% dplyr::filter(abs(Consumo_Min) <= valor & abs(Consumo_Max) <= valor),
    stop("Consulta no válida: ", consulta)
  )
}

###############################

#'
#' @return Nada
#' @export
#' @rdname riego
Riego$initRiego <- function(contratos = dflt$contratos(),
                            resetLecturas = F,
                            initRes = F) {
  if (resetLecturas) {
    unlink(paths$proc())
    unlink(file.path(paths$lects(),'*'))
  }
  contratos <- Riego$updateLecturas(contratos, initRes = initRes)
  # unlink(file.path(paths$conss(),'*'))
  Riego$initConsumos(sort(contratos), initRes = initRes)
}

#'
#' @return Nada
#' @rdname riego
Riego$updateRiego <- function(contratos = dflt$contratos(),
                              initRes = F) {
  contratos <- Riego$updateLecturas(contratos, initRes = initRes)
  Riego$updateConsumos(sort(contratos), initRes = initRes)
}

#'
#' @return Nada
#' @export
#' @rdname riego
Riego$cleanRiego <- function() {
  Riego$cleanLecturas()
  Riego$cleanConsumos()
}

#'
#' @return Nada
#' @export
#' @rdname riego
Riego$downloadRiego <- function(contratos = NULL) {

  invisible(c(
    # Riego$downloadFilesAgua(NULL),
    Riego$downloadLecturas(contratos),
    Riego$downloadConsumos(contratos)
  ))
}



