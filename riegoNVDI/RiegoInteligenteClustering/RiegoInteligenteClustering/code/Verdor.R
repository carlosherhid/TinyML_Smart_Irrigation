if (!exists("paths") || is.null(paths)) {
  wd <- if (Sys.info()["sysname"] == "Windows") "." else
    file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  source(file.path(wd,"code","include.R"), encoding = "UTF-8")
}



#' Datos de verdor
#'
#' @param host
#' @param pwd
#' @param nodename
#' @param contrato(s)
#'   (Vector de) identificador(es) (`strings`) de contrato(s) de interés.
#' @param ind(s)
#'   (Vector de) índice(s) de vegetación.
#'   Posibles valores: `r dflt$inds`.
#' @param mask(s)
#'   (Vector de) máscara(s) de nubes.
#'   Posibles valores: `r dflt$masks`.
#' @param agg(s)
#'   (Vector de) método(s) de agregación de imágenes.
#'   Posibles valores: `r dflt$aggs`.
#' @param ini,fin
#' @name verdor
Verdor <- list(
  nodename = "juno",
  host = "gardens@juno.inf.um.es",
  pwd = "riego_inteligente",
  ini = as.Date("2017-09-29"),
  fin = Sys.Date(),
  frec = "5 days",
  fechasNoSen2 = as.Date(c(#"2021-07-04",#https://scihub.copernicus.eu/news/News00902
                           "2021-05-10",#https://scihub.copernicus.eu/news/News00872
                           # "2021-03-31",
                           "2020-10-22",#https://scihub.copernicus.eu/news/News00761
                           "2019-09-23",#https://scihub.copernicus.eu/news/News00597
                           "2019-05-21",#https://scihub.copernicus.eu/news/News00522
                           # "2018-12-22",
                           "2018-07-25",# solo esta a L1C en el LTA
                           # "2018-06-20",
                           "2018-02-25",# solo esta a L1C en el LTA
                           "2017-12-07",# solo esta a L1C en el LTA
                           "2017-11-27",# solo esta a L1C en el LTA
                           "2017-11-17",# solo esta a L1C en el LTA
                           "2017-11-07",# solo esta a L1C en el LTA
                           "2017-10-28",# solo esta a L1C en el LTA
                           "2017-10-18",# solo esta a L1C en el LTA
                           "2017-10-08",# solo esta a L1C en el LTA
                           "2017-09-28")# solo esta a L1C en el LTA
  ),
  pattern = function(ind = '[a-zA-Z0-9-+ ]+', contrato = '[0-9]{7}', dias = '[0-9]{8}') {
    dias %<>% paste0(collapse = "|")
    paste0('S2[AB]2A_(',dias,')_051_',contrato,'_',ind,'_10\\.tif')
  },
  funs = c('Min','Q1','Median','Mean','Q3','Max','n'),
  getVars = function(ind = dflt$ind, mask = dflt$mask, aggs = dflt$agg) {
    # aggs
    # paste(ind,mask,aggs,sep='_')
    paste(ind,mask,aggs,sep='.')
  },
  getVarsSym = function(ind = dflt$ind, mask = dflt$mask, aggs = dflt$agg) {
    as.symbol(Verdor$getVars(ind, mask, aggs))
  }
)

###############################

#' @export
#' @rdname verdor
Verdor$conectarServVerdor <- function(host = Verdor$host,
                                      pwd = Verdor$pwd) {
  ssh::ssh_connect(host, passwd = pwd)
}

####
Verdor$parseDays <- function(filesImgs) {
  filesImgs %>% stringr::str_extract("(?<=S2[AB]2A_)[0-9]{8}(?=_)")
}
Verdor$parseDates <- function(filesImgs) {
  filesImgs %>% stringr::str_extract("(?<=S2[AB]2A_)[0-9]{8}(?=_)") %>% lubridate::ymd()
}

#'
#' @return Nada
#' @rdname verdor
Verdor$lsDirsImgs <- function(host = Verdor$host,
                              pwd = Verdor$pwd,
                              nodename = Verdor$nodename) {
  msg("Listando carpetas (contratos) de imagenes...")

  if (Sys.info()["nodename"] != nodename) {
    session <- ssh::ssh_connect(host, passwd = pwd)
    on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  }
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)
  dflt$node <<- nodename; ruta <- paths$imgs()
  comando <- paste0('ls ',ruta)
  conCarpeta <- if (Sys.info()["nodename"] == nodename)
    system(comando, intern = T) else
      capture.output(status <- ssh::ssh_exec_wait(session, comando))
      # sys::as_text(ssh::ssh_exec_internal(session, comando)$stdout)
  dflt$node <<- old_node
  jardines <- dflt$jardinesSen2() %>%
    sf::st_drop_geometry() %>%
    dplyr::select(Contrato)
  conCarpeta %<>% intersect(jardines$Contrato)

  listado <- vapply(conCarpeta, function(contrato) {
    dflt$node <<- nodename; ruta <- paths$imgs(contrato,"BOA")
    comando <- paste0('ls ',ruta,' | wc -l')
    if (Sys.info()["nodename"] == nodename)
      system(comando, intern = T) else
        capture.output(status <- ssh::ssh_exec_wait(session, comando))
        # sys::as_text(ssh::ssh_exec_internal(session, comando)$stdout)
  }, "")

  listado %<>%
    data.table::as.data.table(keep.rownames = T) %>%
    # data.table::setDT(keep.rownames = T) %>%
    dplyr::rename(Contrato = 1, Total = 2) %>%
    dplyr::mutate_at("Total", as.integer) %>%
    merge(jardines, by = "Contrato", all = T)
  msg("Guardando listado de carpetas (contratos) de imagenes...")
  dflt$node <<- old_node; fichero = paths$imgsLS()
  data.table::fwrite(listado, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
Verdor$getDirsImgs <- function(update = F) {

  fichero = paths$imgsLS()
  if (!file.exists(fichero) || update)
    Verdor$lsDirsImgs()

  freadResumen(fichero)
}

#' @param consulta Una de `c("sinCarpeta", "conCarpeta", "sinImagenes", "conImagenes")`
#' @export
Verdor$consultarDirsImgs <- function(consulta = "conImagenes",
                                     listado = Verdor$getDirsImgs()) {
  switch(consulta,
    "sinCarpeta"  = listado %>% dplyr::filter(is.na(Total)),
    "conCarpeta"  = listado %>% dplyr::filter(!is.na(Total)),
    "sinImagenes" = listado %>% dplyr::filter(!is.na(Total) & Total == 0),
    "conImagenes" = listado %>% dplyr::filter(!is.na(Total) & Total != 0),
    stop("Consulta no válida: ", consulta)
  )
}

#'
#' @return Nada
#' @rdname verdor
Verdor$lsFilesImgsInd <- function(contratos = dflt$contratos("conImgs"),
                                  ind = "BOA",
                                  host = Verdor$host,
                                  pwd = Verdor$pwd,
                                  nodename = Verdor$nodename) {
  msg("Listando ficheros de imagenes ",ind,"...")

  if (Sys.info()["nodename"] != nodename) {
    session <- ssh::ssh_connect(host, passwd = pwd)
    on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  }
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)
  listado <- lapply(contratos, function(contrato) {
    dflt$node <<- nodename; ruta <- paths$imgs(contrato,ind)
    comando <- paste0('ls ',ruta)
    listado <- if (Sys.info()["nodename"] == nodename)
      system(comando, intern = T) else
        capture.output(status <- ssh::ssh_exec_wait(session, comando))
        # sys::as_text(ssh::ssh_exec_internal(session, comando)$stdout)
    listado %>%
      data.table::as.data.table() %>%
      # data.table::setDT() %>%
      dplyr::rename(Fichero = 1) %>%
      dplyr::arrange(Verdor$parseDays(Fichero))
  })
  names(listado) <- contratos
  listado %<>% listDF2DF()

  msg("Guardando listado de ficheros de imagenes ",ind,"...")
  dflt$node <<- old_node; fichero = paths$imgsLS(ind)
  data.table::fwrite(listado, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
Verdor$getFilesImgsInd <- function(ini = dflt$ini, fin = dflt$fin,
                                   ind = "BOA", update = F) {

  fichero = paths$imgsLS(ind)
  if (!file.exists(fichero) || update)
    Verdor$lsFilesImgsInd(ind = ind)

  listado <- freadResumen(fichero) %>%
    dplyr::mutate(Fecha = Verdor$parseDates(Fichero))
  from <- min(listado$Fecha)
  to <- max(listado$Fecha)
  listado %<>% split(by = "Contrato", keep.by = F) %>%
    lapply(completarFechas, from, to, Verdor$frec) %>%
    listDF2DF()
  if (!is.null(ini)) listado %<>% dplyr::filter(ini <= Fecha)
  if (!is.null(fin)) listado %<>% dplyr::filter(Fecha < fin)
  listado
}

#' @param consulta Una de `c("fechasFalt", "tilesFalt", "total")`
#' @export
Verdor$consultarFilesImgsInd <- function(consulta = "tilesFalt",
                                         contratos = NULL,
                                         fechas = NULL,
                                         fechasNo = Verdor$fechasNoSen2,
                                         ini = dflt$ini, fin = dflt$fin, ind = "BOA",
                                         listado = Verdor$getFilesImgsInd(ini, fin, ind)) {
  if (!is.null(contratos)) listado %<>% dplyr::filter(Contrato %in% contratos)
  if (!is.null(fechas)) listado %<>% dplyr::filter(Fecha %in% fechas)
  if (!is.null(fechasNo)) listado %<>% dplyr::filter(!Fecha %in% fechasNo)
  switch(consulta,
    "fechasFalt"  = {
      listado %>% dplyr::filter(is.na(Fichero)) %>%
        dplyr::count(Fecha, name = "Numero jardines") %>%
        dplyr::arrange(dplyr::desc(Fecha))
    },
    "tilesFalt"  = {
      jardines <- dflt$jardinesSen2() %>%
        dplyr::mutate(Mosaicos = ifelse(Mosaico==Mosaico2,
                                        as.character(Mosaico),"AMBOS")) %>%
        dplyr::select(Contrato,Mosaicos)
      listado %>% dplyr::filter(is.na(Fichero)) %>%
        merge(jardines, by = "Contrato") %>%
        dplyr::count(Fecha, Mosaicos, name = "Numero jardines") %>%
        dplyr::arrange(dplyr::desc(Fecha))
    },
    "total" = {
      fmin <- min(listado$Fecha)
      fmax <- max(listado$Fecha)
      as.integer(fmax-fmin)/5L + 1L
    },
    stop("Consulta no válida: ", consulta)
  )
}

#'
#' @return Nada
Verdor$resumirFilesImgsInd <- function(ini = dflt$ini, fin = dflt$fin, ind = "BOA",
                                       listado = Verdor$getFilesImgsInd(ini, fin, ind)) {
  msg("Inicializando resumen de imagenes ",ind,"...")
  if (!is.null(ini)) listado %<>% dplyr::filter(ini <= Fecha)
  if (!is.null(fin)) listado %<>% dplyr::filter(Fecha < fin)

  listadoRfechas <- listado %>% dplyr::filter(!is.na(Fichero)) %>% resumir("Fecha")
  listadoRnas <- listado %>% resumir("Fichero", c("TotalNA","MaxNA")) %>%
    dplyr::rename(Faltan = 2, Faltan_Max = 3)

  listadoRls <- list(listadoRfechas, listadoRnas)
  listadoR <- purrr::reduce(listadoRls, merge, all=T)
  msg("Guardando resumen de imagenes ",ind,"...")
  fichero = paths$imgsR(ind)
  data.table::fwrite(listadoR, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
Verdor$leerResumenFilesImgsInd <- function(ind = "BOA", update = F) {

  fichero = paths$imgsR(ind)
  if (!file.exists(fichero) || update)
    Verdor$resumirFilesImgsInd(ind = ind)

  freadResumen(fichero)
}

###############################
# Fichero 5.1. Indice de verdor

#' @export
Verdor$checkAvailabilityDatesTiles <- function(faltan = Verdor$consultarFilesImgsInd("tilesFalt",ind="BOA"),
                                               level = c("L2A","L1C","auto")[1],
                                               availability = "check") {
  faltan %<>% dplyr::filter(Mosaicos != "AMBOS")
  sl <- mapply(function(f,t) sen2r::s2_list(
    level = level, tile = t, orbit = c("051"),
    time_interval = f, availability = availability
  ), faltan$Fecha, faltan$Mosaicos, SIMPLIFY = F)
  lapply(sl, data.table::as.data.table) %>%
    dplyr::bind_rows()
}

# Usos:
# Consultar si hay imagenes para un periodo: `fecha` de longitud 2
# > Verdor$checkAvailabilityDates(c("2021-04-14","2021-04-28"))
# Consultar si hay imagenes para un dia: `fecha` de longitud 1, `mes = F`
# > Verdor$checkAvailabilityDates("2021-03-31")
# Consultar si hay imagenes para un mes: `fecha` de longitud 1, primer dia del mes, `mes = T`
# > Verdor$checkAvailabilityDates("2021-02-01", T)
#' @export
Verdor$checkAvailabilityDates <- function(fecha, mes = F,
                                          level = c("L2A","L1C","auto")[1],
                                          availability = "online") {
  fecha %<>% as.Date()
  if (length(fecha) == 1 & mes)
    fecha <- c(fecha, lubridate::rollforward(fecha))
  # print(system.time(
    sl <- sen2r::s2_list(level = level,
                         tile = c("30SXG","30SXH"), orbit = c("051"),
                         time_interval = fecha, availability = availability)
  # ))
  data.table::as.data.table(sl)
}

#' @export
Verdor$cleanResponseAvailability <- function(tl) {
  tl %>% dplyr::mutate(date = as.Date(sensing_datetime),
                       n = 1:dplyr::n(),
                       clouds = redondea2(clouds)) %>%
    dplyr::select(n, clouds, date, id_tile, online, level, mission, id_orbit) %>%
    dplyr::arrange(date, id_tile)
}

#' @export
Verdor$orderLTA <- function(tl, n = 1:nrow(tl), path = NULL, account = c(1,2,3)[1]) {
  if (is.null(path)) {
    tl %<>% dplyr::filter(!online)
    sl <- as(tl[n,], "safelist")
    # print(system.time(
      so <- sen2r::s2_order(sl, apihub = paths$acc(account,"apihub"), reorder = F)
    # ))
  } else {
    # print(system.time(
      so <- sen2r::s2_order(path, apihub = paths$acc(account,"apihub"), reorder = F)
    # ))
  }
  return(so)
}

Verdor$getPathsImgInd <- function(contrato, ind = 'BOA', mask = NA, dias = '[0-9]{8}') {

  tryCatch(
    {
      files.boa <- paths$imgs(contrato,"BOA",NA,dias)
      if (ind == "BOA") return(files.boa)
      files.scl <- paths$imgs(contrato,"SCL",NA,dias)
      if (ind == "SCL") return(files.scl)

      carpeta = paths$imgs()

      if (ind == "RGB") {
        path.ind <- file.path(carpeta, contrato, "RGB432B")
        files.ind <- suppressMessages(
          sen2r::s2_rgb(files.boa, outdir = path.ind)
        )
      } else {
        path.ind <- normalizePath(file.path(carpeta, contrato, ind))
        files.ind <- suppressMessages(
          sen2r::s2_calcindices(files.boa, ind, outdir = path.ind, dataType = "Float32")
        )
      }

      if (is.na(mask)) {
        files.mask <- files.ind
      } else {
        path.mask <- file.path(".",mask)
        files.mask <- suppressMessages(
          sen2r::s2_mask(files.ind, files.scl, mask, outdir = path.mask)
        )
      }
      return(files.mask)
    },
    error = function(cond) {
      msg("Hubo en error en la ejecucion de s2_? para ",
          contrato,"_",ind,"_",mask," en ", dias)
      msg("Here's the original error message:")
      msg(cond)
      msg("")
    }
  )
}

Verdor$downloadSentinel2 <- function(account, s2list, contratos) {

  reduce.out.paths <- function(out.paths, .id) {
    out.paths <- out.paths[!is.na(out.paths)]
    if (length(out.paths) > 0) {
      return(structure(
        purrr::reduce(out.paths, c),
        names = rep(names(out.paths),times=vapply(out.paths,length,0L)),
        status = sapply(out.paths, attr, "status", simplify = F) %>%
          listDF2DF(.id)
      ))
    } else
      return(NA)
  }

  # Borramos los ficheros logs anteriores
  logs <- c(paths$acc(account,"logOUT"), paths$acc(account,"logERR"))
  file.remove(logs[file.exists(logs)])
  # Para redirigir la salida estandar/error
  acc_print_msg <- function(s) {
    print_msg(s, paths$acc(account,"logOUT"), paths$acc(account,"logERR"))
  }

  on.exit(acc_print_msg(warnings()))
  acc_print_msg("------------------ BEGIN sen2r ------------------")

  # LISTAMOS LAS PARCELAS DE ENTRADA
  inputs <- list.files(paths$s2r("parcelas"), full.names = T)
  ids <- basename_sans_ext(inputs)
  inputs <- inputs[ids %in% contratos]
  ids <- ids[ids %in% contratos]

  # FECHAS PARA PROCESAR
  dates <- unique(s2list$date)

  acc_print_msg(paste("Processing",length(dates),"dates and",length(inputs),"gardens..."))

  #for (batch in dates) {
  for (i in 1:length(dates)) {
    batch <- dates[i]
    s_batch <- paste(batch, collapse = " / ")
    acc_print_msg(paste("\tProcessing batch:", s_batch))

    # Solo llamamos a sen2r para los parques que le falten BOA o SCL para batch
    dia <- format.Date(batch,"%Y%m%d")
    files.boa <- sapply(ids, paths$imgs, "BOA", NA, dia)
    files.scl <- sapply(ids, paths$imgs, "SCL", NA, dia)
    idx <- vapply(files.boa, rlang::is_empty, T) | vapply(files.scl, rlang::is_empty, T)
    time <- system.time(
      out_paths_inputs <- sapply(inputs[idx], function(input) {
        id <- basename_sans_ext(input)
        jardin <- sf::st_read(input, drivers = "GeoJSON", quiet = T, stringsAsFactors = F)
        s2tiles <- unique(c(jardin$Mosaico,jardin$Mosaico2))
        out_paths_tiles <- sapply(s2tiles, function(s2tile) {
          if (!plyr::empty(s2list %>% dplyr::filter(id_tile == s2tile, date == batch)))
            tryCatch(
              {
                acc_print_msg(paste("\t\tProcessing garden", id,
                                    ", dates", s_batch, "and tile", s2tile))
                out_paths <- sen2r::sen2r(
                  param_list = paths$s2r("params"),
                  apihub = paths$acc(account,"apihub"),
                  timewindow = batch,
                  s2tiles_selected = s2tile,
                  extent = input,
                  extent_name = id,
                  # path_l1c = paths$acc(account,"safes"),
                  path_l2a = paths$acc(account,"safes"),
                  path_out = paths$imgs(id),
                  log = paths$acc(account,"logERR")
                  # log = nullfile()
                )
                # file.remove(attr(out_paths,"procpath"))
                return(out_paths)
              },
              error = function(cond) {
                # Cuando falla, sen2r::sen2r(log=...) no redirige las salidas "message" ni cierra la conexion
                sink(type = "message")
                showConnections(all = TRUE) %>%
                  as.data.frame %>%
                  tibble::rownames_to_column('con_id') %>%
                  # dplyr::filter(description == "nul:") %>% #Windows
                  # dplyr::filter(description == "/dev/null") %>% #Unix
                  dplyr::filter(description == paths$acc(account,"logERR")) %>%
                  dplyr::pull(con_id) %>%
                  as.integer %>%
                  lapply(getConnection) %>%
                  lapply(close)

                acc_print_msg("Hubo en error en la ejecucion de sen2r")
                acc_print_msg("Here's the original error message:")
                acc_print_msg(cond)
                acc_print_msg("")

                return(NA)
              },
              finally = {
                acc_print_msg(paste("\t\t\tProcessed garden", id,
                                    ", dates", s_batch, "and tile", s2tile))
              }
            )
          else
            return(NA)
        }, simplify = F)
        return(reduce.out.paths(out_paths_tiles, "Tile"))
      }, simplify = F)
    )
    # Guardamos un resumen del proceso
    names(out_paths_inputs) %<>% basename_sans_ext()
    out_paths_inputs %<>% reduce.out.paths("Contrato")
    if (!anyNA(out_paths_inputs)) {
      status <- attr(out_paths_inputs,"status")
      status %<>% dplyr::mutate(Date = batch) %>% dplyr::relocate(Date)
      fichero <- paths$acc(account,"status")
      data.table::fwrite(status, fichero, append = T, sep = ";", na = "NA")
    }
    # Guardamos el tiempo de ejecucion
    times <- data.table::data.table(NumContratos = sum(idx),
                                    Fecha = batch,
                                    user = summary(time)["user"],
                                    system = summary(time)["system"],
                                    elapsed = summary(time)["elapsed"])
    fichero <- paths$acc(account,"times")
    data.table::fwrite(times, fichero, append = T, sep = ";", na = "NA")
    # Borramos el/los archivo/s SAFE descargados
    unlink(file.path(paths$acc(account,"safes"),"*"), recursive = T)
    # Borramos los ficheros proc_par generados
    unlink(file.path(paths$s2r("proc_par"),"*"))

    acc_print_msg(paste("\tProcessed batch:", s_batch))
  }

  acc_print_msg("------------------ END sen2r ------------------")
}

#'
#' @return Nada
#' @rdname verdor
Verdor$updateImgInd <- function(from = Sys.Date()-365, to = Sys.Date(),
                                n_accounts = 3,
                                contratos = dflt$jardinesSen2()$Contrato) {

  msg("------------------ BEGIN sen2r master ------------------")
  msg("Getting available dates/tiles...")
  # OBTENEMOS LAS FECHAS Y TILES DISPONIBLES
  s2list <- Verdor$checkAvailabilityDates(c(from,to), level = "auto", availability = "check")
  s2list %<>% dplyr::select(mission,level,id_tile,id_orbit,sensing_datetime,clouds,online)
  msg("Saving available dates/tiles...")
  fichero <- paths$s2r("new_list")
  data.table::fwrite(s2list, fichero, sep = ";", na = "NA")
  ####
  # s2list %<>% dplyr::filter(online)
  s2list %<>% dplyr::filter(online, level %in% c("2A","2Ap"))
  # s2list %<>% dplyr::filter(online, level == "1C")
  ####
  s2list %<>% dplyr::mutate(date = as.Date(sensing_datetime))
  s2list %<>% dplyr::arrange(date, id_tile)

  msg("Splitting ",nrow(s2list)," dates/tiles between ",n_accounts," accounts...")
  # LAS REPARTIMOS ENTRE --accounts
  stopifnot(nrow(s2list) > 0)
  s2list %<>% dplyr::mutate(n_date = as.integer(as.factor(date)))
  if (max(s2list$n_date) < n_accounts) n_accounts <- max(s2list$n_date)
  s2list %<>% dplyr::mutate(account = (n_date-1L) %% n_accounts)
  s2list %<>% split(by = "account")

  msg("Running Sentinel-2 download for ",length(contratos)," gardens in ",n_accounts," accounts...")
  # LANZAMOS EL SCRIPT --accounts VECES, CON LOS PARAMETROS PERTINENETES
  old_cores <- dflt$cores
  on.exit(dflt$cores <<- old_cores, add = T, after = F)
  dflt$cores <- n_accounts
  my.mapply(Verdor$downloadSentinel2, 1:n_accounts, s2list, MoreArgs = list(contratos))
  msg("------------------ END sen2r master ------------------")

  # Verdor$lsDirsImgs()
  # Verdor$lsFilesImgsInd(ind = "BOA")
  # Verdor$resumirFilesImgsInd(ind = "BOA")
}

#'
#' @return Nada
#' @export
#' @rdname verdor
Verdor$cleanImgInd <- function(contratos = dflt$contratos("conImgs"),
                               inds = dflt$ind,
                               # masks = unique(c(NA, dflt$mask))) {
                               masks = setdiff(NA, dflt$mask)) {

  lapply(inds, function(ind) {
    lapply(masks, function(mask) {
      cs <- intersect(contratos, dflt$contratos("conSums", ind, mask))
      msg("Limpiando imagenes de ",ind,".",mask," de ",length(cs)," contratos...")
      my.lapply(cs, function(contrato) {
        resumen <- Verdor$leerSumInd(contrato, ind, mask)
        ficheros <- Verdor$getFilesImgInd(contrato, ind, mask)

        allDates <- Verdor$parseDates(ficheros)
        processedDates <- resumen$Fecha
        oldDates <- intersect(allDates, processedDates)

        if (length(oldDates) > 0) {
          msg(contrato,"_",ind,"_",mask,": eliminando ",length(oldDates)," imagenes...")
          file.remove(ficheros[allDates %in% oldDates])
        }
      })
    })
  }) %>% invisible()
}

#'
#' @return Nada
#' @export
#' @rdname verdor
Verdor$downloadImgInd <- function(contratos, inds = dflt$ind, fechas = NULL,
                                  host = Verdor$host,
                                  pwd = Verdor$pwd,
                                  nodename = Verdor$nodename) {

  session <- ssh::ssh_connect(host, passwd = pwd)
  on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)

  msg("Descargando imagenes...")
  if (is.null(contratos)) {
    dflt$node <<- old_node; dst <- paths$verdor()
    dflt$node <<- nodename; src <- paths$imgs()
    out <- capture.output(
      status <- ssh::scp_download(session, src, dst)
    )
  } else {
    if (is.null(inds)) {
      dflt$node <<- old_node; dst  <- paths$imgs()
      dflt$node <<- nodename; srcs <- paths$imgs(contratos)
      out <- sapply(srcs, function(src) {
        capture.output(
          status <- ssh::scp_download(session, src, dst)
        )
      }, USE.NAMES = F)
    } else {
      out <- c()
      for (contrato in contratos) {
        # if (is.null(fechas)) {
          dflt$node <<- old_node; dst  <- paths$imgs(contrato)
          dflt$node <<- nodename; srcs <- paths$imgs(contrato, inds)
          out %<>% c(sapply(srcs, function(src) {
            capture.output(
              status <- ssh::scp_download(session, src, dst)
            )
          }, USE.NAMES = F))
        # } else {
        #   for (ind in inds) {
        #     dflt$node <<- old_node; dst  <- paths$imgs(contrato, ind)
        #     dflt$node <<- nodename; srcs <- paths$imgs(contrato, ind, fechas)
        #     out %<>% c(vapply(srcs, function(src) {
        #       capture.output(
        #         status <- ssh::scp_download(session, src, dst)
        #       )
        #     }, "", USE.NAMES = F))
        #   }
        # }
      }
    }
  }
  msg(sum(tools::file_ext(out) != "")," ficheros descargados")

  invisible(out)
}

Verdor$readImg <- function(fileImg) {
  raster::as.data.frame(raster::raster(fileImg), xy = T)
}

#' @return Un `stack` con...
#' @export
#' @rdname verdor
Verdor$leerImgInd <- function(contrato, ind = dflt$ind, mask = NA, dia = '20210515') {
  fichero <- paths$imgs(contrato, ind, mask, dia)
  if (length(fichero) == 0)
    fichero <- Verdor$getPathsImgInd(contrato, ind, mask, dia)
  # raster::raster(fichero)
  raster::stack(fichero)
}

###############################
# Tarea 6.1. Mapas e imágenes

#' @return Un `data.table` con...
Verdor$leerColorRamp <- function(ind = dflt$ind) {
  fichero <- paths$ramps(ind)
  data.table::fread(fichero, sep = c(","),
                    col.names = c("values","R","G","B")) %>%
    dplyr::mutate(svalues = scales::rescale(values),
                  hcolors = grDevices::rgb(R, G, B, 255, maxColorValue = 255))

}
#' @export
#' @rdname verdor
Verdor$plotImgInd <- function(contrato, ind = dflt$ind, mask = NA, dia = '20210515',
                              sfg = 3, obj_sf = dflt$dg(), lan = dflt$lan,
                              carpeta = paths$saves(lan), f_save = NULL) {

  obj_raster <- Verdor$leerImgInd(contrato, ind, mask, dia)
  obj_df <- raster::as.data.frame(obj_raster, xy = T)
  # info <- stringr::str_split(colnames(obj_df)[3],"_")[[1]]
  if (ind != "RGB" && sfg == 3)
    # paleta <- Verdor$leerColorRamp(info[5])
    paleta <- Verdor$leerColorRamp(ind)
  obj_sf %<>% sf::st_transform(crs = sf::st_crs(obj_raster)) %>%
    # dplyr::filter(Contrato == info[4])
    dplyr::filter(Contrato == contrato)

  gg <- ggplot2::ggplot(obj_df, ggplot2::aes(x, y))
  if (ind == "RGB") {
    obj_df[[3]][is.na(obj_df[[3]])] <- 245
    obj_df[[4]][is.na(obj_df[[4]])] <- 245
    obj_df[[5]][is.na(obj_df[[5]])] <- 220
    gg <- gg +
      ggplot2::geom_raster(ggplot2::aes(fill = grDevices::rgb(obj_df[[3]],obj_df[[4]],obj_df[[5]],
                                                              maxColorValue = 255))) +
      ggplot2::scale_fill_identity()
  } else {
    gg <- gg +
      ggplot2::geom_raster(ggplot2::aes(fill = obj_df[[3]])) +
      switch(sfg,
             ggplot2::scale_fill_gradient(low = "brown", high = "green",
                                          na.value = "beige"),
             ggplot2::scale_fill_gradient2(low = "blue", mid = "brown", high = "green",
                                           limits = c(-1,1), na.value = "beige"),
             ggplot2::scale_fill_gradientn(colours = paleta$hcolors,
                                           values = paleta$svalues,
                                           limits = range(paleta$values),
                                           na.value = "beige")
      )
  }
  gg <- gg +
    # ggplot2::coord_equal() +
    ggplot2::geom_sf(data = obj_sf, inherit.aes = F, fill = NA, colour = "black") +
    # ggplot2::labs(#title = paste('Contrato:',info[4]),
    #               #subtitle = paste('Fecha(YYYYMMDD):',info[2]),
    #               fill = info[5])
    # ggplot2::scale_x_continuous("long", breaks = NULL, minor_breaks = NULL,
    #                             n.breaks = 0, labels = NULL) +
    ggplot2::scale_x_continuous("long") +
    # ggplot2::scale_y_continuous("lat", breaks = NULL, minor_breaks = NULL,
    #                             n.breaks = 0, labels = NULL) +
    ggplot2::scale_y_continuous("lat") +
    ggplot2::labs(#title = paste('Contrato:',contrato),
                  #subtitle = paste('Fecha(YYYYMMDD):',dia),
                  fill = ind)

  if (!is.null(f_save)) {
    fichero <- paste0("Img_",ind,"_",mask,".",f_save)
    msg("Guardando ",fichero)
    ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                    width = 7, height = 5, units = "in")
  }

  gg
}

#' @export
#' @rdname verdor
Verdor$plotHistImgInd <- function(contrato, ind = dflt$ind, mask = NA, dia = '20210515',
                                  bw = 0.05, lan = dflt$lan,
                                  carpeta = paths$saves(lan), f_save = NULL) {

  obj_raster <- Verdor$leerImgInd(contrato, ind, mask, dia)
  obj_df <- raster::as.data.frame(obj_raster, xy = T)
  # info <- stringr::str_split(colnames(obj_df)[3],"_")[[1]]

  gg <- ggplot2::ggplot(obj_df, ggplot2::aes(obj_df[[3]])) +
    ggplot2::geom_histogram(binwidth = bw, boundary = 0) +
    # ggplot2::labs(#title = paste('Contrato:',info[4]),
    #               #subtitle = paste('Fecha(YYYYMMDD):',info[2]),
    #               x = info[5])
    ggplot2::labs(#title = paste('Contrato:',contrato),
                  #subtitle = paste('Fecha(YYYYMMDD):',dia),
                  x = ind)

  if (!is.null(f_save)) {
    fichero <- paste0("ImgHist_",ind,"_",mask,".",f_save)
    msg("Guardando ",fichero)
    ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                    width = 7, height = 5, units = "in")
  }

  gg
}

###############################
# Fichero 5.2. Indice de verdor

Verdor$numNoNA <- function(fileImg) {
  df <- Verdor$readImg(fileImg)
  sum(!is.na(df[[3]]))
}

Verdor$resumirImgInd <- function(fileImg) {
  df <- Verdor$readImg(fileImg)
  r <- summary(df[[3]])
  n <- sum(!is.na(df[[3]]))
  r[7] <- n
  names(r)[7] <- "n"
  r
}

#'
#' @return Nada
#' @rdname verdor
Verdor$calcularSumInd <- function(contrato, ind = dflt$ind, mask = NA) {

  resumen <- Verdor$leerSumInd(contrato, ind, mask)
  ficheros <- paths$imgs(contrato, "BOA", NA)

  allDays <- Verdor$parseDays(ficheros)
  allDates <- lubridate::ymd(allDays)
  processedDates <- resumen$Fecha
  newDates <- setdiff(allDates, processedDates) %>% sort() %>% lubridate::as_date()

  if (length(newDates) > 0) {

    idx <- allDates %in% newDates
    fechas <- allDates[idx]
    dias <- allDays[idx]
    ficheros <- Verdor$getPathsImgInd(contrato, ind, mask, dias)
    dias <- Verdor$parseDays(ficheros)
    fechas <- lubridate::ymd(dias)
    resumen <- sapply(ficheros, Verdor$resumirImgInd)

    resumen <- data.frame(fechas, t(resumen))
    colnames(resumen) <- c("Fecha", Verdor$getVars(ind, mask, Verdor$funs))
    fichero <- paths$sums(ind, mask, contrato)
    data.table::fwrite(resumen, fichero, append = T, sep = ";", na = "NA")

  }

}

#'
#' @return Nada
#' @rdname verdor
Verdor$updateSumInd <- function(contratos = dflt$contratos("conImgs"),
                                inds = dflt$ind,
                                masks = unique(c(NA, dflt$mask)),
                                aggs = dflt$agg,
                                initRes = F) {

  fichero <- paths$s2r("times")
  lapply(inds, function(ind) {
    lapply(masks, function(mask) {
      msg("Actualizando summaries de ",ind,".",mask," de ",length(contratos)," contratos...")
      my.lapply(contratos, function(contrato) {
        time <- summary(system.time(tryCatch(
          Verdor$calcularSumInd(contrato, ind, mask),
          error = function(cond) {
            msg("Hubo en error en la ejecucion para ",contrato,"_",ind,"_",mask)
            msg("Here's the original error msg:")
            msg(cond)
            msg("")
          }
        )))
        time %<>% matrix(ncol = length(time), dimnames = list(NULL, names(time)))
        time %<>% data.table::data.table()
        time <- data.table::data.table(Contrato = contrato,
                                       Indice = ind,
                                       Mascara = mask,
                                       time)
        data.table::fwrite(time, fichero, append = T, sep = ";", na = "NA")
      })
      if (initRes) {
        msg("Leyendo y resumiendo summaries de ",ind,".",mask,"...")
        idvs = listDF2DF(Verdor$readSumInd(NULL, ind, mask))
        lapply(aggs, function(agg) {
          Verdor$resumirSumInd(idvs = idvs, agg = agg)
        })
      }
    })
  }) %>% invisible()
}

#'
#' @return Nada
#' @export
#' @rdname verdor
Verdor$cleanSumInd <- function(contratos = dflt$contratos("conImgs"),
                               inds = dflt$ind,
                               masks = unique(c(NA, dflt$mask))) {

  # msg("Limpiando tiempos summaries...")
  # fichero <- paths$s2r("times")
  # times <- data.table::fread(fichero, sep = ";")
  # times %<>% dplyr::arrange(Indice,Mascara,Contrato,dplyr::desc(elapsed)) %>%
  #   # dplyr::distinct(Indice,Mascara,Contrato,.keep_all=T)
  #   dplyr::group_by(Indice,Mascara,Contrato) %>%
  #   dplyr::summarise_if(is.numeric, sum)
  # data.table::fwrite(times, fichero, sep = ";", na = "NA")

  lapply(inds, function(ind) {
    lapply(masks, function(mask) {
      cs <- intersect(contratos, dflt$contratos("conSums", ind, mask))
      msg("Limpiando summaries de ",ind,".",mask," de ",length(cs)," contratos...")
      my.lapply(cs, function(contrato) {
        fichero <- paths$sums(ind, mask, contrato)
        idvs <- Verdor$leerSumInd(contrato, ind, mask)
        if (plyr::empty(idvs)) {
          msg(contrato,"_",ind,"_",mask,": eliminando fichero summaries vacio...")
          file.remove(fichero)
        } else {
          data.table::fwrite(idvs, fichero, sep = ";", na = "NA")
        }
      })
    })
  }) %>% invisible()
}

#'
#' @return Nada
#' @export
#' @rdname verdor
Verdor$downloadSumInd <- function(inds = dflt$ind,
                                  masks = unique(c(NA,dflt$mask)),
                                  contratos = NULL,
                                  aggs = dflt$agg,
                                  host = Verdor$host,
                                  pwd = Verdor$pwd,
                                  nodename = Verdor$nodename,
                                  downloadRes = T) {

  session <- ssh::ssh_connect(host, passwd = pwd)
  on.exit(ssh::ssh_disconnect(session), add = T, after = F)
  old_node <- dflt$node
  on.exit(dflt$node <<- old_node, add = T, after = F)

  msg("Descargando summaries de imagenes...")
  if (is.null(inds)) {
    dflt$node <<- old_node; dst <- paths$verdor()
    dflt$node <<- nodename; src <- paths$sums()
    out <- capture.output(
      status <- ssh::scp_download(session, src, dst)
    )
  } else {
    if (is.null(masks)) {
      dflt$node <<- old_node; dst  <- paths$sums()
      dflt$node <<- nodename; srcs <- paths$sums(inds)
      out <- sapply(srcs, function(src) {
        capture.output(
          status <- ssh::scp_download(session, src, dst)
        )
      }, USE.NAMES = F)
    } else {
      out <- c()
      for (ind in inds) {
        if (is.null(contratos)) {
          dflt$node <<- old_node; dst  <- paths$sums(ind)
          dflt$node <<- nodename; srcs <- paths$sums(ind, masks)
          out %<>% c(sapply(srcs, function(src) {
            capture.output(
              status <- ssh::scp_download(session, src, dst)
            )
          }, USE.NAMES = F))
        } else {
          for (mask in masks) {
            dflt$node <<- old_node; dst  <- paths$sums(ind, mask)
            dflt$node <<- nodename; srcs <- paths$sums(ind, mask, contratos)
            out %<>% c(vapply(srcs, function(src) {
              capture.output(
                status <- ssh::scp_download(session, src, dst)
              )
            }, "", USE.NAMES = F))
          }
        }
      }
    }
  }
  msg(sum(tools::file_ext(out) != "")," ficheros descargados")

  if (downloadRes) {
    msg("Descargando resumen de summaries de imagenes...")
    dflt$node <<- old_node; dst <- paths$verdor()
    for (agg in aggs)
      for (ind in inds)
        for (mask in masks) {
          dflt$node <<- nodename; src <- paths$sumsR(ind, mask, agg)
          out %<>% c(capture.output(
            status <- ssh::scp_download(session, src, dst)
          ))
        }
  }

  invisible(out)
}

#' @return Un `data.table` con...
#' @export
#' @rdname verdor
Verdor$leerSumInd <- function(contrato, ind = dflt$ind, mask = dflt$mask,
                             ini = NULL, fin = NULL) {

  fichero <- paths$sums(ind, mask, contrato, T)
  idv <- freadDateSerie(fichero, ini, fin)

  # idv %<>% dplyr::mutate(Tiempo = lubridate::as.duration(dplyr::lead(Fecha) - Fecha))

  attr(idv, "param") <- list(ind = ind, mask = mask)
  idv
}

#' @return Una `list` de `data.table` con...
#' @export
#' @rdname verdor
Verdor$readSumInd <- function(contratos = dflt$contratos("conImgs"),
                              ind = dflt$ind, mask = dflt$mask, ...) {

  if (is.null(contratos))
    contratos <- dflt$contratos("conSums", ind, mask)
  else
    contratos %<>% intersect(dflt$contratos("conSums", ind, mask))
  idvs <- my.lapply(contratos, Verdor$leerSumInd, ind, mask, ...)
  names(idvs) <- contratos
  attr(idvs, "param") <- list(ind = ind, mask = mask)
  idvs
}
#########################################################
#Funcion leerSumInd utilizando elasticSearch
#########################################################
Verdor$leerSumIndElastic <- function(contrato, ind = dflt$ind, mask = dflt$mask,
                                     ini = NULL, fin = NULL) {
  print(contrato)
  # Levantamos la conexion con el servidor elastic
  conn <- connect(host = "155.54.95.97")
  # Preparamos la query
  for_everything <- query('{
    "match_all": {}
  }') 
  
  # Obtenemos el archivo deseado
  obj.df <- elastic("http://155.54.95.97:9200/", contrato, "data") %search% for_everything
  
  # Procesamos el archivo
  obj.df$fecha %<>% lubridate::as_date()
  if (!is.null(ini)) obj.df %<>% dplyr::filter(ini <= fecha)
  if (!is.null(fin)) obj.df %<>% dplyr::filter(fecha < fin)
  obj.df %<>% dplyr::distinct() %>% dplyr::arrange(fecha)
  if (length(obj.df$fecha) != dplyr::n_distinct(obj.df$fecha))
    warning("Fechas duplicadas: ", fichero)
  idv <- obj.df
  
  attr(idv, "param") <- list(ind = ind, mask = mask)
  idv
}
Verdor$readSumIndElastic <- function(contratos = dflt$contratos("conImgs"),
                                     ind = dflt$ind, mask = dflt$mask, ...) {
  
  if (is.null(contratos))
    contratos <- dflt$contratos("conSums", ind, mask)
  else
    contratos %<>% intersect(dflt$contratos("conSums", ind, mask))
  idvs <- my.lapply(contratos, Verdor$leerSumIndElastic, ind, mask, ...)
  names(idvs) <- contratos
  attr(idvs, "param") <- list(ind = ind, mask = mask)
  idvs
}

#'
#' @return Nada
#' @rdname verdor
Verdor$resumirSumInd <- function(ini = dflt$ini, fin = dflt$fin,
                                 agg = dflt$agg, idvs = dflt$idvs()) {
  ind <- attr(idvs, "param")$ind
  mask <- attr(idvs, "param")$mask
  msg("Inicializando resumen de ",ind,".",mask,".",agg," ...")

  idvsRfechas <- idvs %>%
    resumir("Fecha")
  # idvsRtiempos <- idvs %>%
  #   dplyr::filter(Fecha >= ini, Fecha < fin) %>%
  #   resumir("Tiempo")

  from <- min(idvs$Fecha)
  to <- max(idvs$Fecha)
  idvs %<>% split(by = "Contrato", keep.by = F) %>%
    lapply(completarFechas, from, to, Verdor$frec) %>%
    listDF2DF()

  idvsRns <- idvs %>%
    dplyr::filter(Fecha >= ini, Fecha < fin) %>%
    resumir(Verdor$getVars(ind,mask,"n"), c("TotalD","TotalNA","MaxNA"))
  suppressWarnings(
    idvsRaggs <- idvs %>%
      dplyr::filter(Fecha >= ini, Fecha < fin) %>%
      resumir(Verdor$getVars(ind,mask,agg), c("Min","Mean","Max","TotalNoNA","TotalNA")))

  idvsRls <- list(idvsRfechas,# idvsRtiempos,
                  idvsRns, idvsRaggs)
  idvsR <- purrr::reduce(idvsRls, merge, all=T)
  msg("Guardando resumen de ",ind,".",mask,".",agg," ...")
  fichero <- paths$sumsR(ind, mask, agg)
  data.table::fwrite(idvsR, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
#' @rdname verdor
Verdor$leerResumenSumInd <- function(ind = dflt$ind, mask = dflt$mask, agg = dflt$agg) {

  fichero <- paths$sumsR(ind, mask, agg)
  if (!file.exists(fichero)) {
    idvs <- listDF2DF(Verdor$readSumInd(NULL, ind, mask), "Contrato")
    Verdor$resumirSumInd(agg = agg, idvs = idvs)
  }

  res <- freadResumen(fichero)
  # fmin <- min(res$Fecha_Min, na.rm = T)
  # fmax <- max(res$Fecha_Max, na.rm = T)
  # n_Total <- as.numeric(fmax - fmin)/5 + 1
  # v <- DatInt$getVarsSym("TotalNA",ind,mask,agg)
  # n <- DatInt$getVars("TotalNoNA",ind,mask,agg)
  # res %>% dplyr::mutate("{n}" := n_Total - !!v)
  res
}

####
#' @param consulta Una de `c("sinSerie", "sinVegetacion", "muchaVegetacion")`
#' @export
#' @rdname verdor
Verdor$consultarResumenSumInd <- function(consulta, valor = switch(consulta,
                                            "sinVegetacion" = ,
                                            "muchaVegetacion" = dflt$vegetacion),
                                          ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                                          res = Verdor$leerResumenSumInd(ind, mask, agg)) {
  switch(consulta,
    "sinSerie" = {
      v <- DatInt$getVarsSym("Mean",ind,mask,agg)
      res %>% dplyr::filter(is.na(!!v))
    },
    "conSerie" = {
      v <- DatInt$getVarsSym("Mean",ind,mask,agg)
      res %>% dplyr::filter(!is.na(!!v))
    },
    "sinVegetacion" = {
      v <- DatInt$getVarsSym("Max",ind,mask,agg)
      res %>% dplyr::filter(-Inf < !!v, !!v < valor) %>% dplyr::arrange(!!v)
    },
    "conVegetacion" = {
      v <- DatInt$getVarsSym("Max",ind,mask,agg)
      res %>% dplyr::filter(valor <= !!v, !!v < Inf) %>% dplyr::arrange(!!v)
    },
    "muchaVegetacion" = {
      v <- DatInt$getVarsSym("Min",ind,mask,agg)
      res %>% dplyr::filter(valor < !!v, !!v < Inf) %>% dplyr::arrange(!!v)
    },
    stop("Consulta no válida: ", consulta)
  )
}

###############################

#'
#' @return Nada
#' @rdname verdor
Verdor$updateVerdor <- function(from = Sys.Date()-365, to = Sys.Date(),
                                n_accounts = 3,
                                contratos = dflt$contratos("conImgs"),
                                inds = dflt$ind,
                                masks = unique(c(NA, dflt$mask)),
                                aggs = dflt$agg,
                                initRes = F) {

  Verdor$updateImgInd(from, to, n_accounts)
  # Verdor$updateSumInd(contratos, inds, masks, aggs, initRes)
}

#'
#' @return Nada
#' @rdname verdor
Verdor$cleanVerdor <- function(contratos = dflt$contratos("conImgs"),
                               inds = dflt$ind,
                               masks = unique(c(NA, dflt$mask))) {
  # Verdor$cleanImgInd(contratos, inds, masks)
  Verdor$cleanSumInd(contratos, inds, masks)
}

#'
#' @return Nada
#' @rdname verdor
Verdor$downloadVerdor <- function(inds = dflt$ind,
                                  masks = unique(c(NA,dflt$mask)),
                                  contratos = NULL,
                                  aggs = dflt$agg) {

  invisible(c(
    # # POR FAVOR USAR rsync...
    # Verdor$downloadImgInd(contratos, inds, fechas = NULL),
    # mejor usar rsync
    Verdor$downloadSumInd(inds, masks, contratos, aggs)
  ))
}



