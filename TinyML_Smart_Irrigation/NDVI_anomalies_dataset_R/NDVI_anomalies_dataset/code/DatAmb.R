if (!exists("paths") || is.null(paths)) {
  wd <- if (Sys.info()["sysname"] == "Windows") "." else
    file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  source(file.path(wd,"code","include.R"), encoding = "UTF-8")
}



#' Datos ambientales
#'
#' @param url
#' @param userpwd
#' @param e(e)m(m)
#'   (Vector de) identificador(es) (`string`) de estacion(es) de
#'   interés (debe(n) tener datos disponibles, ver [DatAmb$consultarEstMet()]).
#' @param estacion(es)
#'   (Lista de) vector(es) de identificadores (`strings`) de las estaciones de
#'   interés, siendo el primer elemento la estación principal y el resto las
#'   alternativas.
#' @param crop(s)
#'   (Vector de) cultivo(s) de referencia para la `et0`.
#'   Posibles valores: `r dflt$crops`.
#' @param ini,fin
#' @name amb
DatAmb <- list(
  url = "ftp://idei.imida.es/",
  userpwd = "um:ImidA_Siam2",
  # ini = as.Date("2011-01-01"),
  ini = as.Date("2015-11-25"),
  fin = Sys.Date(),
  frecM = "hour",
  frecE = "day",
  vars = c("Prec","Tmed","HRmed","VVmed","RADmed")
)

###############################
# Fichero 4.1. Datos meteorológicos

#' @return Un vector de identificadores (`strings`) de las estaciones con datos
#'   disponibles.
#' @export
#' @rdname amb
DatAmb$consultarEstMet <- function(url = DatAmb$url,
                                   userpwd = DatAmb$userpwd) {

  RCurl::getURL(url = url,
                userpwd = userpwd,
                ftp.use.epsv = F,
                dirlistonly = T) %>%
    stringr::str_split("\\r\\n") %>%
    purrr::reduce(c) %>%
    stringr::str_extract(".*(?=\\.txt)") %>%
    sort() %>%
    stringr::str_subset("^[A-Z]{2}[0-9]{2}$")
}

#'
#' @return Nada
#' @rdname amb
DatAmb$downloadDatMet <- function(eemm = dflt$eemm,
                                  url = DatAmb$url,
                                  userpwd = DatAmb$userpwd) {
  msg("Descargando datos meteorologicos...")

  # urls <- paste0(url,eemm,"_hist.txt")
  # histrawdatas <- RCurl::getURL(url = urls, userpwd = userpwd)
  # histdms <- my.lapply(histrawdatas, function(hrd)
  #   if (hrd == "") data.table::data.table()
  #   else data.table::fread(text = hrd))

  urls <- paste0(url,eemm,".txt")
  rawdatas <- RCurl::getURL(url = urls, userpwd = userpwd)
  dms <- my.lapply(rawdatas, data.table::fread)

  # dms <- my.mapply(rbind, histdms, dms, SIMPLIFY = F)
  # names(dms) <- eemm
  dms %<>% my.lapply(function(dm) {
    dm %>%
      dplyr::rename_all(stringr::str_to_title) %>%
      dplyr::rename_all(stringr::str_replace,
                        "(.*)(?=(med|max|min))", stringr::str_to_upper) %>%
      dplyr::rename(PtoRocio = Dewpt, DPV = Dpv) %>%

      dplyr::mutate(Fecha = lubridate::dmy_hms(paste(Fecha,Hora)),
                    .keep = "unused") %>%
      dplyr::distinct() %>%# dplyr::arrange(Fecha) %>%

      dplyr::mutate_at("HRmax", ~ifelse(.x == "#####", 100, .x)) %>%
      dplyr::mutate_if(is.character, ~ifelse(.x == "None", NA_character_, .x)) %>%
      dplyr::mutate_if(is.character, as.numeric) %>%
      completarFechas(by = DatAmb$frecM)
  })
  # names(dms) <- eemm

  ficheros <- paths$met(eemm)
  invisible(my.mapply(data.table::fwrite, dms, ficheros, sep = ";", na = "NA"))
}

#'
#' @param vars
#' @return Un `string` con la subcadena de estacion utilizada
#' @rdname amb
DatAmb$completarDatMet <- function(estacion = c("MU62","MU21"),
                                   vars = DatAmb$vars) {

  ficheros <- paths$met(estacion)
  # if (any(!file.exists(ficheros)))
  #   DatAmb$downloadDatMet(estacion)
  dms <- lapply(ficheros, data.table::fread, sep = ";")
  # names(dms) <- estacion

  from <- min(dms[[1]]$Fecha)
  to <- max(dms[[1]]$Fecha)
  dms %<>% lapply(function(dm) {
    dm %>% dplyr::filter(from <= Fecha & Fecha <= to) %>%
      dplyr::distinct() %>%# dplyr::arrange(Fecha) %>%
      completarFechas(from, to, DatAmb$frecM)
  })
  # names(dms) <- estacion

  na.replace <- function(y, x) {
    idx_pred <- is.na(y) & !is.na(x)
    idx_train <- !is.na(y) & !is.na(x)
    if (sum(idx_pred) > 0 && sum(idx_train) > 0) {
      model <- stats::lm(y ~ x, data.frame(x = x[idx_train],y = y[idx_train]))
      y[idx_pred] <- stats::predict(model, data.frame(x = x[idx_pred]))
    }
    y
  }

  k <- 1
  while (k < length(dms) &
         !all(stats::complete.cases(dms[[1]] %>% dplyr::select(dplyr::all_of(vars))))) {
    k <- k+1
    for (j in which(vapply(dms[[1]], anyNA, FALSE))) {
      dms[[1]][[j]] %<>% na.replace(dms[[k]][[j]])
    }
  }

  abnormal.replace <- function(v, vmin = -Inf, vmax = Inf) {
    v[v < vmin] <- vmin
    v[v > vmax] <- vmax
    v
  }

  dms[[1]] %<>%
    dplyr::mutate_at(c("Tmed","Tmax","Tmin","PtoRocio"),
                     abnormal.replace, -50, 100) %>%
    dplyr::mutate_at(c("HRmed","HRmax","HRmin"),
                     abnormal.replace, 0, 100) %>%
    dplyr::mutate_at(c("RADmed","RADmax","Prec",
                       "VVmed","VVmax","DVmed"),
                     abnormal.replace, 0)

  estacion <- paste0(estacion[1:k],collapse="_")
  fichero <- paths$met(estacion)
  data.table::fwrite(dms[[1]], fichero, sep = ";", na = "NA")
  estacion
}

#' @param crudo Indica si considera el fichero de la estacion principal, es decir,
#'   con posibles datos incompletos.
#' @return Un `string` con el fichero de datos más completo (con más estaciones)
#'   que encuentre, o `NULL` si no encuentra ninguno.
#' @rdname amb
DatAmb$getPathDatMet <- function(estacion = c("MU62","MU21"),
                                 crudo = T) {

  posibilidades <- rev(purrr::accumulate(estacion, paste, sep = "_"))
  if (!crudo) posibilidades %<>% head(n = -1)
  ficheros <- paths$met(posibilidades)
  if (!any(file.exists(ficheros)))
    return(NULL)
  ficheros[which.max(file.exists(ficheros))]
}

#'
#' @return Nada
#' @rdname amb
DatAmb$initDatMet <- function(estaciones = dflt$estaciones()) {

  unlink(file.path(paths$met(),'*'))
  DatAmb$downloadDatMet()
  msg("Completando datos meteorologicos...")
  # mc.preschedule = F,
  my.lapply(estaciones, function(estacion) {
    fichero <- DatAmb$getPathDatMet(estacion, F)
    if (is.null(fichero))
      msg("#######     ",DatAmb$completarDatMet(estacion),"     #######")
  }) %>% invisible()
}

#' @return Un `data.table` con...
#' @rdname amb
DatAmb$leerDatMet <- function(estacion = c("MU62","MU21"),
                              ini = NULL, fin = NULL) {

  if (length(estacion) > 1) {
    fichero <- DatAmb$getPathDatMet(estacion, F)
    if (is.null(fichero)) {
      # msg("Completando datos meteorologicos...")
      # msg("#######     ",DatAmb$completarDatMet(estacion),"     #######")
      DatAmb$completarDatMet(estacion)
    }
  }

  fichero <- DatAmb$getPathDatMet(estacion, T)
  dm <- freadDateTimeSerie(fichero, ini, fin)
  dm
}

#' @param ...
#' @return Una `list` de `data.table` con...
#' @rdname amb
DatAmb$readDatMet <- function(estaciones = dflt$estaciones(), ...) {

  dms <- my.lapply(estaciones, DatAmb$leerDatMet, ...)
  names(dms) <- basename_sans_ext(vapply(estaciones, DatAmb$getPathDatMet, ""))
  dms
}

#' @param princ Un identificador (`string`) o vector de identificadores (`strings`)
#' @param alter Un identificador (`string`)
#' @param vr
#' @param lan
#' @param f_save
#' @return
#' @export
#' @rdname amb
DatAmb$plotModelDatMet <- function(princ = c("MU62","MU21"), alter = "CA42",
                                   vr = "Prec", fin = dflt$fin, lan = dflt$lan,
                                   carpeta = paths$saves(lan), f_save = NULL) {

  dms <- DatAmb$readDatMet(list(princ,alter), fin = fin)
  y <- dms[[1]][[vr]]
  x <- dms[[2]][[vr]]

  idx <- !is.na(y) & !is.na(x)
  df_train <- data.frame(x = x[idx], y = y[idx])
  model <- stats::lm(y ~ x, df_train)
  b <- coef(model)[1]; a <- coef(model)[2]

  idx <- is.na(y) & !is.na(x)
  y[idx] <- stats::predict(model, data.frame(x = x[idx]))
  df_pred <- data.frame(x = x[idx], y = y[idx])

  gg <- ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x, y), df_train) +
    ggplot2::geom_abline(slope = a, intercept = b, colour = "blue") +
    ggplot2::geom_point(ggplot2::aes(x, y), df_pred, colour = "red") +
    ggplot2::labs(title = "",
                  subtitle = "",
                  x = paste0(vr,"_",names(dms)[2]),
                  y = paste0(vr,"_",names(dms)[1]))

  if (!is.null(f_save)) {
    fichero <- paste(names(dms)[1], names(dms)[2], vr, sep = "_")
    if (!is.null(fin)) fichero %<>% paste0('_l_',fin)
    fichero %<>% paste0(".",f_save)
    msg("Guardando ",fichero)
    ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                    width = 7, height = 5, units = "in")
  }

  list(model, gg)
}

####
# Tarea 6.2. Tablas e histogramas
#' @param consulta
#' @param vars
#' @param dms
#' @return
#' @export
#' @rdname amb
DatAmb$consultarDatMet <- function(consulta = "tablaNA",
                                   fin = dflt$fin,
                                   vars = DatAmb$vars,
                                   dms = dflt$dms()) {


  switch (consulta,
    "tablaNA" = {
      if (!is.null(fin)) dms %<>% dplyr::filter(Fecha < as.POSIXct(fin))
      if (!is.null(vars)) dms %<>% dplyr::select(c("Estacion",dplyr::all_of(vars)))
      dms$Estacion %<>% as.factor()
      dms %<>% dplyr::filter(!stats::complete.cases(.))
      dms %<>% dplyr::group_by(Estacion, .drop = F)
      tabla <- merge(#cbind(
        dms %>% dplyr::summarise_all(numNA),
        dms %>% dplyr::summarise(numNCCs = dplyr::n()))
      rownames(tabla) <- NULL
      return(tabla)
    },
    stop("Consulta no válida: ", consulta)
  )
}

###############################
# Fichero 4.2. Datos evapotranspiración

#'
#' @return Un `string` con la subcadena de estacion utilizada
#' @rdname amb
DatAmb$calcularET0 <- function(estacion = c("MU62","MU21"),
                               crop = dflt$crop) {

  de <- DatGeo$leerEstMet(estacion[1])
  dm <- DatAmb$leerDatMet(estacion)
  estacion <- basename_sans_ext(DatAmb$getPathDatMet(estacion))

  # devtools::install_github("VicenteYago/ET.PenmanMonteith")
  et0 <- ET.PenmanMonteith::et0(dates = dm$Fecha,
                                temp  = dm$Tmed,
                                hr    = dm$HRmed,
                                uz    = dm$VVmed,
                                rs    = dm$RADmed,
                                lat   = de$LATITUD,
                                elev  = de$ALTITUD,
                                crop  = crop) %>%
    # data.table::as.data.table() %>%
    data.table::setDT() %>%
    dplyr::rename(Fecha = date, ET0 = et0) %>%
    # colnames(et0) <- c("Fecha", paste0("ET0.",crop))
    dplyr::mutate_at("Fecha", lubridate::as_date)

  fichero <- paths$et0(crop, estacion)
  data.table::fwrite(et0, fichero, sep = ";", na = "NA")
  estacion
}

#' @return Un `string` con el fichero de datos más completo (con más estaciones)
#'   que encuentre, o `NULL` si no encuentra ninguno.
#' @rdname amb
DatAmb$getPathET0 <- function(estacion = c("MU62","MU21"),
                              crop = dflt$crop) {

  posibilidades <- rev(purrr::accumulate(estacion, paste, sep = "_"))
  ficheros <- paths$et0(crop, posibilidades)
  if (!any(file.exists(ficheros)))
    return(NULL)
  ficheros[which.max(file.exists(ficheros))]
}

#'
#' @return Nada
#' @rdname amb
DatAmb$initET0 <- function(estaciones = dflt$estaciones(),
                           crops = dflt$crop) {

  lapply(crops, function(crop) {
    unlink(file.path(paths$et0(crop),'*'))
    msg("Inicializando datos de evapotranspiracion de referencia ",crop," ...")
    # mc.preschedule = F,
    my.lapply(estaciones, function(estacion) {
      fichero <- DatAmb$getPathET0(estacion, crop)
      if (is.null(fichero))
        msg("#######     ",DatAmb$calcularET0(estacion, crop),"     #######")
    })
  }) %>% invisible()
}

#' @return Un `data.table` con...
#' @rdname amb
DatAmb$leerET0 <- function(estacion = c("MU62","MU21"),
                           crop = dflt$crop,
                           ini = NULL, fin = NULL) {

  fichero <- DatAmb$getPathET0(estacion, crop)
  if (is.null(fichero)) {
    msg("Inicializando datos de evapotranspiracion de referencia ",crop," ...")
    msg("#######     ",DatAmb$calcularET0(estacion, crop),"     #######")
  }

  fichero <- DatAmb$getPathET0(estacion, crop)
  et0 <- freadDateSerie(fichero, ini, fin)
  attr(et0, "param") <- list(crop = crop)
  et0
}

#' @param ...
#' @return Una `list` de `data.table` con...
#' @rdname amb
DatAmb$readET0 <- function(estaciones = dflt$estaciones(),
                           crop = dflt$crop, ...) {

  et0s <- my.lapply(estaciones, DatAmb$leerET0, crop, ...)
  names(et0s) <- basename_sans_ext(vapply(estaciones, DatAmb$getPathDatMet, ""))
  attr(et0s, "param") <- list(crop = crop)
  et0s
}

###############################
# Fichero 4.3. Datos ambientales

#'
#' @return Nada
#' @rdname amb
#' @export
DatAmb$initDatAmb <- function(contratos = NULL,
                              crop = dflt$crop) {

  estaciones = DatGeo$consultarDatGeo("setEstMet", contratos)
  DatAmb$initDatMet(estaciones)
  DatAmb$initET0(estaciones, crop)
}

#' @return Un `data.table` con...
#' @rdname amb
#' @export
DatAmb$leerDatAmb <- function(estacion = c("MU62","MU21"),
                              crop = dflt$crop,
                              ini = NULL, fin = NULL) {

  dm <- DatAmb$leerDatMet(estacion, ini, fin) %>%
    dplyr::mutate_at("Fecha", lubridate::date) %>%
    dplyr::rename(DPVmed = DPV) %>%
    agregarPorFecha()
  et0 <- DatAmb$leerET0(estacion, crop, ini, fin)
  da <- merge(dm, et0)
  attr(da, "param") <- list(crop = crop)
  da
}

#' @param ...
#' @return Una `list` de `data.table` con...
#' @rdname amb
#' @export
DatAmb$readDatAmb <- function(estaciones = dflt$estaciones(),
                              crop = dflt$crop, ...) {

  das <- my.lapply(estaciones, DatAmb$leerDatAmb, crop, ...)
  names(das) <- basename_sans_ext(vapply(estaciones, DatAmb$getPathDatMet, ""))
  attr(das, "param") <- list(crop = crop)
  das
}



