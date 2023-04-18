if (!exists("paths") || is.null(paths)) {
  wd <- if (Sys.info()["sysname"] == "Windows") "." else
    file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  source(file.path(wd,"code","include.R"), encoding = "UTF-8")
}



#' Datos integrados
#'
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
#' @param crop(s)
#'   (Vector de) cultivo(s) de referencia para la `et0`.
#'   Posibles valores: `r dflt$crops`.
#' @param int(s)
#'   (Vector de) método(s) de integración de series de datos.
#'   Posibles valores: `r dflt$ints`.
#' @param ini,fin
#' @name int
DatInt <- list(
  getVars = function(stats = "Mean", ind = dflt$ind, mask = dflt$mask, agg = dflt$agg) {
    paste0(Verdor$getVars(ind,mask,agg),"_",stats)
  },
  getVarsSym = function(stats = "Mean", ind = dflt$ind, mask = dflt$mask, agg = dflt$agg) {
    as.symbol(DatInt$getVars(stats, ind, mask, agg))
  },
  levCD = {c("Ninguna",
             "ConsumoCero",
             "LecturasPocoTardias",
             "FaltanAlgunasLecturas",
             "FaltanMuchasLecturas",
             "ConsumoNegativo",
             "LecturasTardias",
             "VariosContadores_FaltanConsumos",
             "LecturasMuyTardias",
             "VariosContratos",
             "LecturasNoActuales",
             "SinLecturas",
             "SinContrato",
             "SinUbicacion")},

  levCDR = {c("Ninguna",
              "ConsumoCero",
              "FaltanMuchasLecturas",
              "ConsumoNegativo",
              "VariosContadores",
              "FaltanConsumos",
              "VariosContadores_FaltanConsumos",
              "LecturasTardias",
              "LecturasNoActuales",
              "VariosContratos",
              "SinLecturas",
              "SinContrato",
              "SinUbicacion")},
  levCDV = {c("Ninguna",
              "SinVegetacion",
              "SinSerieSCL",
              "SinSerieNA",
              "SinImagenes",
              "SinProcesar",
              "SinUbicacion")}
)

###############################
# Tarea 3. Integracion

#' @return Una `list` de `data.table` con...
#' @export
#' @rdname int
DatInt$leerSeries <- function(contrato, ind = dflt$ind, mask = dflt$mask,
                              crop = dflt$crop, ini = NULL, fin = NULL) {

  estacion <- DatGeo$consultarDatGeo("listEstMet", contratos = contrato)

  list(DatAmb = DatAmb$leerDatAmb(estacion, crop, ini, fin) %>%
         dplyr::mutate(Estacion = estacion[1], .before = 1),
       Riego  = Riego$leerConsumos(contrato, ini, fin) %>%
         dplyr::mutate(Contrato = as.character(contrato), .before = 1),
       Verdor = Verdor$leerSumInd(contrato, ind, mask, ini, fin) %>%
         dplyr::mutate(Contrato = as.character(contrato), .before = 1))
}

#' @return Una `list` de `data.table` con...
#' @export
#' @rdname int
DatInt$readSeries <- function(contratos = dflt$contratos("conSerie"),
                              ind = dflt$ind, mask = dflt$mask,
                              crop = dflt$crop, ini = NULL, fin = NULL) {

  if (is.null(contratos))
    contratos <- dflt$contratos("conSeries", ind, mask)
  else
    contratos %<>% intersect(dflt$contratos("conSeries", ind, mask))

  estaciones <- DatGeo$consultarDatGeo("setEstMet", contratos = contratos)

  list(DatAmb = listDF2DF(DatAmb$readDatAmb(estaciones, crop, ini, fin),"Estacion"),
       Riego  = listDF2DF(Riego$readConsumos(contratos, ini, fin)),
       Verdor = listDF2DF(Verdor$readSumInd(contratos, ind, mask, ini, fin)))
}

#'
#' @return Nada
#' @rdname int
DatInt$integrarSeries <- function(contrato, ind = dflt$ind,
                                  mask = dflt$mask, agg = dflt$agg,
                                  crop = dflt$crop, int = dflt$int) {

  dts <- DatInt$leerSeries(contrato, ind, mask, crop)
  dts$DatAmb %<>% dplyr::select(Fecha, Prec, ET0)
  dts$Riego  %<>% dplyr::select(Fecha, Consumo)
  dts$Verdor %<>% dplyr::select(Fecha, Verdor$getVars(ind,mask,agg))
  # Juntamos los datos diarios
  dts$DatAmb %<>% merge(dts$Riego)
  names(dts)[names(dts) == "DatAmb"] <- "DatDia"
  dts$Riego <- NULL
  # Duplicamos la serie de indices
  dts$VerdorObj <- dts$Verdor
  # Prepararamos las fechas para que indiquen el dia con el que se relacionan
  dts$DatDia %<>% dplyr::mutate(DiasAntes = 5 - ( (as.integer(Fecha)+3) %% 5 ),
                                Fecha = Fecha + lubridate::days(DiasAntes))
  dts$Verdor %<>% dplyr::mutate(Fecha = Fecha + lubridate::days(5))
  switch(int,
    # Redimensionamos las series diarias (de cada serie pasamos a 5 series)
    'redim' = {
      ddas <- lapply(5:1, function(n) dts$DatDia %>%
                       dplyr::filter(DiasAntes == n) %>%
                       dplyr::select(!DiasAntes) %>%
                       dplyr::rename_at(dplyr::vars(!Fecha), paste0, ".", n))
      dts$DatDia <- purrr::reduce(ddas, merge)
      dts$Verdor %<>% dplyr::rename_at(dplyr::vars(!Fecha), paste0, ".", 5)
    },
    # Agregamos las series diarias (1 valor cada 5 días)
    'agg' = {
      dts$DatDia %<>% dplyr::select(!DiasAntes) %>% agregarPorFecha()
      dts$VerdorObj %<>% dplyr::rename_at(dplyr::vars(!Fecha), paste0,  ".obj")
    },
    stop("int no válida: ",int))
  # Juntamos todos los datos
  dts %<>% purrr::reduce(merge)

  fichero <- paths$int(int, crop, ind, mask, agg, contrato)
  data.table::fwrite(dts, fichero, sep = ";", na = "NA")
}

#'
#' @return Nada
#' @export
#' @rdname int
DatInt$initDatInt <- function(contratos = dflt$contratos("ConSerie"),
                              inds = dflt$ind, masks = unique(c(NA,dflt$mask)),
                              aggs = dflt$agg, crops = dflt$crop, ints = dflt$ints,
                              initRes = F) {

  # if (initRes)
  #   DatInt$resumirJardines(version = 1)
  lapply(inds, function(ind) {
    lapply(masks, function(mask) {
      lapply(aggs, function(agg) {
        # if (initRes)
        #   DatInt$resumirJardines(version = 2, ind, mask, agg)
        cs <- intersect(contratos, dflt$contratos("ConSeries", ind, mask, agg))
        msg("Inicializando datos integrados de ",length(cs)," contratos...")
        lapply(crops, function(crop) {
          lapply(ints, function(int) {
            msg("#######     ",int,"_",crop,"_",ind,".",mask,".",agg,"     #######")
            my.lapply(cs, function(contrato) {
              DatInt$integrarSeries(contrato, ind, mask, agg, crop, int)
            })
            if (initRes) {
              msg("Leyendo y resumiendo datos integrados...")
              dts <- listDF2DF(DatInt$readDatInt(NULL, ind, mask, agg, crop, int))
              DatInt$resumirDatInt(dts = dts)
            }
          })
       })
      })
    })
  })
  invisible(NULL)
}

#' @return Un `data.table` con...
#' @export
#' @rdname int
DatInt$leerDatInt <- function(contrato, ind = dflt$ind, mask = dflt$mask,
                              agg = dflt$agg, crop = dflt$crop, int = dflt$int,
                              ini = NULL, fin = NULL) {

  fichero <- paths$int(int, crop, ind, mask, agg, contrato)
  if (!file.exists(fichero)) {
    msg("Inicializando datos integrados de ",contrato,"...")
    msg("#######     ",int,"_",crop,"_",ind,".",mask,".",agg,"     #######")
    DatInt$integrarSeries(contrato, ind, mask, agg, crop, int)
  }

  dts <- freadDateSerie(fichero,ini, fin)

  attr(dts, "param") <- list(ind = ind, mask = mask, agg = agg,
                             crop = crop, int = int)
  dts
}

#' @return Una `list` de `data.table` con...
#' @export
#' @rdname int
DatInt$readDatInt <- function(contratos = NULL, ind = dflt$ind, mask = dflt$mask,
                              agg = dflt$agg, crop = dflt$crop, int = dflt$int, ...) {

  if (is.null(contratos))
    contratos <- dflt$contratos("conDatInt", ind, mask, agg, crop, int)
  else
    contratos %<>% intersect(dflt$contratos("conDatInt", ind, mask, agg, crop, int))
  dts <- my.lapply(contratos, DatInt$leerDatInt, ind, mask, agg, crop, int, ...)
  names(dts) <- contratos
  attr(dts, "param") <- list(ind = ind, mask = mask, agg = agg,
                             crop = crop, int = int)
  dts
}

#'
#' @return Nada
#' @rdname int
DatInt$resumirDatInt <- function(ini = dflt$ini, fin = dflt$fin,
                                 dts = dflt$datos()) {
  ind <- attr(dts, "param")$ind
  mask <- attr(dts, "param")$mask
  agg <- attr(dts, "param")$agg
  crop <- attr(dts, "param")$crop
  int <- attr(dts, "param")$int
  msg("Inicializando resumen de ",int,"_",crop,"_",ind,".",mask,".",agg," ...")

  dts %<>% dplyr::filter(Fecha >= ini, Fecha < fin)

  datosRtotal <- dts %>% dplyr::group_by(Contrato) %>%
    dplyr::summarise(Obs_Total = dplyr::n())
  dts %<>% dplyr::filter(complete.cases(.))
  datosRccs <- dts %>% dplyr::group_by(Contrato) %>%
    dplyr::summarise(Obs_CCs = dplyr::n())

  datosRls <- list(datosRtotal, datosRccs)
  datosR <- purrr::reduce(datosRls, merge, by = "Contrato")
  msg("Guardando resumen de ",int,"_",crop,"_",ind,".",mask,".",agg," ...")
  fichero <- paths$intR(int, crop, ind, mask, agg)
  data.table::fwrite(datosR, fichero, sep = ";", na = "NA")
}

#' @return Un `data.table` con...
#' @export
#' @rdname int
DatInt$leerResumenDatInt <- function(ind = dflt$ind, mask = dflt$mask,
                                     agg = dflt$agg, crop = dflt$crop,
                                     int = dflt$int) {

  fichero <- paths$intR(int, crop, ind, mask, agg)
  if (!file.exists(fichero)) {
    dts <- listDF2DF(DatInt$readDatInt(NULL, ind, mask, agg, crop, int), "Contrato")
    DatInt$resumirDatInt(dts = dts)
  }

  freadResumen(fichero) %>%
    dplyr::mutate(Obs_NCCs = Obs_Total - Obs_CCs)# %>%
    # dplyr::rename_with(~paste0(int,".",.x), -1)
}

###############################
# Tarea 2. Seleccion

#'
#' @param version
#' @return Nada
#' @rdname int
DatInt$resumirJardines <- function(version = 2, ind = dflt$ind,
                                   mask = dflt$mask, agg = dflt$agg) {
  msg("Inicializando resumen jardines v",version,"...")

  dg = DatGeo$leerJardines() %>%# sf::st_drop_geometry() %>%
    dplyr::select(Acometida, MaxNodos, Nodos, Poligonos, Area, MasPeque, Contrato,
                  Distancia, Principal, Alternativa1, Mosaico, Mosaico2)
  contadoresR = Riego$leerResumenContadores() %>%
    dplyr::select(Contrato, TieneContadorEnLect)
  lecturasR = Riego$leerResumenLecturas() %>%
    dplyr::select(Contrato, Fecha_Min, Fecha_Max, Tiempo_Max)
  consumosR = Riego$leerResumenConsumos() %>%
    dplyr::select(Contrato, Consumo_Min, Consumo_Max, Consumo_MaxNA)
  jardinesRls <- list(dg, contadoresR, lecturasR, consumosR)

  if (version == 2) {
    listado = Verdor$consultarDirsImgs("conCarpeta") %>%
      dplyr::select(Contrato, Total)
    idvsRna = Verdor$leerResumenSumInd(ind, NA, agg) %>%
      # dplyr::select(Contrato, vapply(c("Min","Mean","Max"), DatInt$getVars, "", ind, NA, agg))
      dplyr::select(Contrato, DatInt$getVars("Mean", ind, NA, agg))
    idvsRscl = Verdor$leerResumenSumInd(ind, mask, agg) %>%
      dplyr::select(Contrato, DatInt$getVars("Total", ind, mask, "n"),
                    # vapply(c("Min","Mean","Max","TotalNA"), DatInt$getVars, "", ind, mask, agg))
                    vapply(c("Mean","Max"), DatInt$getVars, "", ind, mask, agg, USE.NAMES = F))
    idvsR = if (is.na(mask)) idvsRscl else merge(idvsRna, idvsRscl, by = "Contrato")
    jardinesRls %<>% c(list(listado, idvsR))
  }

  jardinesR <- purrr::reduce(jardinesRls, merge, by = "Contrato", all = T)
  variosContratos <- jardinesR %>% dplyr::count(Acometida) %>%
    dplyr::filter(n>1) %>% dplyr::pull(Acometida)

  jardinesR <- switch (version,
    jardinesR %>% dplyr::mutate(
      CausaDescarte = dplyr::case_when(
        #is.na(Abcisa) | is.na(Norte)       ~ "SinUbicacion",
        is.na(Contrato)                     ~ "SinContrato",
        TieneContadorEnLect == "Ninguno"    ~ "SinLecturas",
        Acometida %in% variosContratos      ~ "VariosContratos",
        Fecha_Max < dflt$noActuales         ~ "LecturasNoActuales",
        Fecha_Min > as.Date("2020-01-01")   ~ "LecturasMuyTardias",
        # Consumo_MaxNA > 0                   ~ "FaltanConsumos",
        # TieneContadorEnLect == "Varios"     ~ "VariosContadores",
        TieneContadorEnLect == "Varios"     ~ "VariosContadores_FaltanConsumos",
        Fecha_Min > as.Date("2019-01-01")   ~ "LecturasTardias",
        Consumo_Min < dflt$eps              ~ "ConsumoNegativo",
        Tiempo_Max > "17 days"              ~ "FaltanMuchasLecturas",
        Tiempo_Max > "9 days"               ~ "FaltanAlgunasLecturas",
        Fecha_Min > as.Date("2018-01-01")   ~ "LecturasPocoTardias",
        abs(Consumo_Min) <= dflt$eps &
          abs(Consumo_Max) <= dflt$eps      ~ "ConsumoCero",
        TRUE                                ~ "Ninguna"
      )
    ),
    jardinesR %>% dplyr::mutate(
      CausaDescarteRiego = dplyr::case_when(
        #is.na(Abcisa) | is.na(Norte)       ~ "SinUbicacion",
        is.na(Contrato)                     ~ "SinContrato",
        TieneContadorEnLect == "Ninguno"    ~ "SinLecturas",
        Acometida %in% variosContratos      ~ "VariosContratos",
        Fecha_Max < dflt$noActuales         ~ "LecturasNoActuales",
        Fecha_Min > dflt$tardias            ~ "LecturasTardias",
        Consumo_MaxNA > 0                   ~ "FaltanConsumos",
        TieneContadorEnLect == "Varios"     ~ "VariosContadores",
        Consumo_Min < dflt$eps              ~ "ConsumoNegativo",
        Tiempo_Max > "15 days"              ~ "FaltanMuchasLecturas",
        abs(Consumo_Min) <= dflt$eps &
          abs(Consumo_Max) <= dflt$eps      ~ "ConsumoCero",
        TRUE                                ~ "Ninguna"
      ),
      CausaDescarteVerdor = dplyr::case_when(
        #is.na(Abcisa) | is.na(Norte)                            ~ "SinUbicacion",
        is.na(Total)                                             ~ "SinProcesar",
        Total == 0                                               ~ "SinImagenes",
        is.na(!!DatInt$getVarsSym("Mean",ind,NA,agg))             ~ "SinSerieNA",
        is.na(!!DatInt$getVarsSym("Mean",ind,mask,agg))           ~ "SinSerieSCL",
        !!DatInt$getVarsSym("Max",ind,mask,agg) < dflt$vegetacion ~ "SinVegetacion",
        TRUE                                                     ~ "Ninguna"
      )
    )
  )

  jardinesR %<>% dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Tiempo_")),
                                  lubridate::period_to_seconds)

  msg("Guardando resumen jardines v",version,"...")
  fichero = switch (version, paths$jardiR(),
                    paths$jardiR2(ind, mask, agg))
  if (file.exists(fichero)) file.remove(fichero)
  sf::st_write(jardinesR, fichero, driver = "GeoJSON", quiet = T)
  # data.table::fwrite(jardinesR, paths$jardiR2(), sep = ";", na = "NA")
}

#' @param maxDkm
#' @param minAPm2
#' @param maxCD
#' @param maxCDR
#' @param maxCDV
#' @param version
#' @return Un `data.table` con...
#' @export
#' @rdname int
DatInt$leerResumenJardines <- function(maxDkm = 20, minAPm2 = 0,
                                       maxCD  = "SinUbicacion",
                                       maxCDR = "SinUbicacion",
                                       maxCDV = "SinUbicacion",
                                       version = 2, ind = dflt$ind,
                                       mask = dflt$mask, agg = dflt$agg) {

  fichero = switch (version, paths$jardiR(),
                    paths$jardiR2(ind, mask, agg))
  if (!file.exists(fichero))
    DatInt$resumirJardines(version, ind, mask, agg)

  # data.table::fread(paths$jardiR2(), sep = ";") %>%
  jardi <- streadGeoJSON(fichero)
  jardi <- switch (version,
    jardi %>% dplyr::mutate_at("CausaDescarte", factor,
                               levels = DatInt$levCD, ordered = T),
    jardi %>% dplyr::mutate_at("CausaDescarteRiego", factor,
                               levels = DatInt$levCDR, ordered = T) %>%
              dplyr::mutate_at("CausaDescarteVerdor", factor,
                               levels = DatInt$levCDV, ordered = T)
  )
  if (!is.null(maxDkm))
    jardi %<>% dplyr::filter(Distancia / 1000 < maxDkm)
  if (!is.null(minAPm2))
    jardi %<>% dplyr::filter(MasPeque > minAPm2)
  if (version == 1 && !is.null(maxCD))
    jardi %<>% dplyr::filter(CausaDescarte < maxCD)
  if (version == 2 && !is.null(maxCDR))
    jardi %<>% dplyr::filter(CausaDescarteRiego < maxCDR)
  if (version == 2 && !is.null(maxCDV))
    jardi %<>% dplyr::filter(CausaDescarteVerdor < maxCDV)
  jardi
}

###############################
# Tarea 6.3. Graficos

obtenerElemento <- function(v) {
  v %>% stringr::str_replace_all("(med|max|min)$","") %>%
    stringr::str_replace_all("\\.[0-9a-zA-Z]+$","") %>%
    stringr::str_replace_all(c(
      "PtoRocio" = "T",
      "Prec|ET0|Consumo" = "BH" #BalanceHidrico
    ))
}

#' @return Un `data.table` con...
#' @export
DatInt$reestructurarSeries <- function(dts) {

  dts$Riego$ConsumoTotal <- NULL
  dts$Verdor %<>% dplyr::select(!dplyr::ends_with(".n"))
  # dts$Verdor$Tiempo <- NULL
  dts %>% lapply(function(df) df %>%
                   dplyr::rename(Entidad = 1) %>%
                   reshape2::melt(id.vars = c("Fecha","Entidad"),
                                  variable.name = "Variable",
                                  value.name = "Valor")) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(Elemento = forcats::as_factor(obtenerElemento(Variable)),
                  .before = Variable)
}

DatInt$leerParamTS <- function(param = c("Ele","Var")[1], lan = dflt$lan) {
  fichero <- paths$params(lan, param)
  read.table(fichero, sep = ";")
}

redondea2 <- function(num) {
  # round(num,digits = 2)
  # format(num, nsmall = 2)
  format(round(num, digits = 2), scientific = F)
}

#' @export
DatInt$plotTS <- function(obj_dt, eleORvar, # .bw = 1,
                          ini = NULL, fin = NULL, psz = 0.5, lsz = 0.3,
                          ply = T, lan = dflt$lan,
                          carpeta = paths$saves(lan), f_save = NULL) {

  param_v <- DatInt$leerParamTS("Var", lan)
  param_e <- DatInt$leerParamTS("Ele", lan)

  if (eleORvar %in% rownames(param_e)) {
    obj_dt %<>% dplyr::filter(Elemento == eleORvar)
    param_y <- param_e
    nombres <- unique(obj_dt$Variable)
    colores <- param_v[nombres,"col"]
  } else if (eleORvar %in% rownames(param_v)) {
    obj_dt %<>% dplyr::filter(Variable == eleORvar)
    param_y <- param_v
    nombres <- eleORvar
    colores <- param_e[obtenerElemento(eleORvar),"col"]
  } else {
    stop("Indica una variable o elemento (grupo de variables) de los siguientes:\n",
         "Variables: ",rownames(param_v),"\nElementos: ",rownames(param_e))
  }

  N <- nrow(obj_dt)
  obj_dt %<>% dplyr::filter(!is.na(Valor))
  n <- N - nrow(obj_dt)
  if (n > 0)
    msg("Eliminadas ",n," observaciones NA de ",N," (",redondea2(n/N*100),"%)")

  if (!is.null(ini) || !is.null(fin)) {
    # N <- nrow(obj_dt)
    if (!is.null(ini)) obj_dt %<>% dplyr::filter(Fecha >= ini)
    if (!is.null(fin)) obj_dt %<>% dplyr::filter(Fecha < fin)
    # n <- N - nrow(obj_dt)
    # if (n > 0)
    #   msg("Eliminadas ",n," observaciones de ",N," (",redondea2(n/N*100),"%)")
  }

  # x_vals <- obj_dt %>% dplyr::pull(Fecha)
  # x_param <- param.Date(x_vals, .bw)
  # y_vals <- obj_dt %>% dplyr::pull({{eleORvar}})
  # y_param <- param.default(y_vals)
  y_lab <- paste(param_y[eleORvar,"tit"], param_y[eleORvar,"unit"])
  gg <- ggplot2::ggplot(obj_dt, ggplot2::aes(Fecha, Valor, colour = Variable)) +
    ggplot2::geom_point(size = psz) + ggplot2::geom_line(size = lsz) +
    # ggplot2::geom_smooth(method = 'loess', formula = y ~ x, na.rm = T) +
    # ggplot2::scale_x_date(breaks = x_param$brks, limits = x_param$lims) +
    # ggplot2::scale_y_continuous(breaks = seq(param_y[eleORvar,"limInf"],
    #                                          param_y[eleORvar,"limSup"],
    #                                          param_y[eleORvar,"by"]),
    #                             # limits = c(param_y[eleORvar,"limInf"],
    #                             #            param_y[eleORvar,"limSup"]),
    #                             labels = redondea2(breaks)) +
    ggplot2::scale_colour_manual(breaks = nombres,
                                 values = as.character(dflt$cols[colores])) +
    ggplot2::labs(# title = switch(lan,
                  #   "sp" = paste("Datos de",param_y[eleORvar,"tit"]),
                  #   paste(param_y[eleORvar,"tit"],"data")
                  # ),
                  # x = switch(lan,
                  #   "sp" = paste(dflt$units$sp$datetime[as.integer(.bw)],"de",x_lab),
                  #   paste(dflt$unist$en$datetime[as.integer(.bw)],"of",x_lab)
                  # ),
                  x = switch(lan, "sp" = "Fecha", "Date"),
                  y = if (!ply) latex2exp::TeX(y_lab) else
                    stringr::str_remove_all(y_lab,"\\$"))

  if (length(nombres) == 1)
    gg <- gg + ggplot2::theme(legend.position="none")

  if (!is.null(f_save)) {
    fichero <- eleORvar
    if (!is.null(ini)) fichero %<>% paste0('_ge_',ini)
    if (!is.null(fin)) fichero %<>% paste0('_l_',fin)
    fichero %<>% paste0(".",f_save)
    msg("Guardando ",fichero)
    ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                    width = 7, height = 5, units = "in")
  }

  if (ply)
    return(plotly::ggplotly(gg))
  else
    return(gg)
}

#' @export
DatInt$plotHistTS <- function(obj_dt, variable,
                              ini = NULL, fin = NULL,
                              ply = T, lan = dflt$lan,
                              carpeta = paths$saves(lan), f_save = NULL) {

  param_v <- DatInt$leerParamTS("Var", lan)
  # param_e <- DatInt$leerParamTS("Ele", lan)

  obj_dt %<>% dplyr::filter(Variable == variable)

  N <- nrow(obj_dt)
  obj_dt %<>% dplyr::filter(!is.na(Valor))
  n <- N - nrow(obj_dt)
  if (n > 0)
    msg("Eliminadas ",n," observaciones NA de ",N," (",redondea2(n/N*100),"%)")

  if (!is.null(ini) || !is.null(fin)) {
    # N <- nrow(obj_dt)
    if (!is.null(ini)) obj_dt %<>% dplyr::filter(Fecha >= ini)
    if (!is.null(fin)) obj_dt %<>% dplyr::filter(Fecha < fin)
    # n <- N - nrow(obj_dt)
    # if (n > 0)
    #   msg("Eliminadas ",n," observaciones de ",N," (",redondea2(n/N*100),"%)")
  }

  gg <- ggplot2::ggplot(obj_dt, ggplot2::aes(Valor)) +
    # Representamos histograma de la variable
    ggplot2::geom_histogram(binwidth = param_v[variable,"bw"],
                            boundary = param_v[variable,"limInf"],
                            fill = "white", color = "black", na.rm = T) +
    # Marca el valor de la media con una línea azul vertical
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(Valor, na.rm = T),
                                     color = "Media")) +
    # Marca el valor de la mediana con una línea roja vertical
    ggplot2::geom_vline(ggplot2::aes(xintercept = median(Valor, na.rm = T),
                                     color = "Mediana")) +
    ggplot2::scale_colour_manual(name="", values = c("blue","red")) +
    ggplot2::scale_x_continuous(breaks = seq(param_v[variable,"limInf"],
                                             param_v[variable,"limSup"],
                                             param_v[variable,"by"]),
                                limits = c(param_v[variable,"limInf"],
                                           param_v[variable,"limSup"]),
                                labels = redondea2) +
    ggplot2::labs(#title = switch(lan,
                  #  "sp" = paste("Histograma de",param_v[variable,"tit"]),
                  #  paste(param_v[variable,"tit"],"histogram")),
                  x = paste(param_v[variable,"tit"], param_v[variable,"unit"]))

  if (!is.null(f_save)) {
    fichero <- variable
    if (!is.null(ini)) fichero %<>% paste0('_ge_',ini)
    if (!is.null(fin)) fichero %<>% paste0('_l_',fin)
    fichero %<>% paste0(".",f_save)
    msg("Guardando ",fichero)
    ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                    width = 7, height = 5, units = "in")
  }

  if (ply)
    return(plotly::ggplotly(gg))
  else
    return(gg)
}

###############################
# Tarea 6.2. Tablas e histogramas

param.default <- function(x, .bw = c("corto","largo")[1]) {
  l.inf <- min(x, na.rm = T)
  l.sup <- max(x, na.rm = T)
  bw <- ceiling(max(abs(c(l.inf,l.sup))) / 100)
  # bw <- max(1,floor(l.sup / 100))
  bw %<>% my.ceiling(-switch(.bw,"corto" = n.digits(bw)-1,"largo" = n.digits(bw)))
  # bw %<>% signif(switch(.bw,"corto" = 2,"largo" = 1))
  l.inf %<>% my.floor(-n.digits(bw))
  l.sup %<>% my.ceiling(-n.digits(bw))
  # l.inf %<>% signif(3)
  # l.sup %<>% signif(3)
  bys <- 1:21*(5*bw)
  n_breaks <- (l.sup - l.inf) / bys
  by <- bys[utils::head(which(n_breaks < 10),n=1)]
  by %<>% my.ceiling(-(n.digits(bw)-1))
  return(list(clsd = "right", bw = bw, by = by, lims = c(l.inf,l.sup)))
}

param.Date <- function(x, .bw = c("sec","min","hour","day","week","month","quarter","year")[8]) {
  bw = .bw
  .bw %<>% factor(levels = dflt$units$datetime, ordered = T)
  l.inf <- min(x, na.rm = T)
  l.sup <- max(x, na.rm = T)
  .by <- if (.bw == "year") "year" else dflt$units$datetime[as.integer(.bw)+1]
  l.inf %<>% floor.instant(.by)
  l.sup %<>% ceiling.instant(.by)
  by <- if (.bw == "year") paste(floor((l.sup - l.inf)/5),"year") else .by
  return(list(clsd = "left", bw = bw, by = by, lims = c(l.inf,l.sup)))
}

#' @export
DatInt$plotHist <- function(obj_dt, variable, x_lab = substitute(variable),
                            inf = NULL, sup = NULL,
                            .bw = if (lubridate::is.instant(obj_dt %>% dplyr::pull({{variable}})))
                              c("sec","min","hour","day","week","month","quarter","year")[8] else
                                c("corto","largo")[1],
                            ply = T, lan = dflt$lan,
                            carpeta = paths$saves(lan), f_save = NULL) {

  N <- nrow(obj_dt)
  obj_dt %<>% dplyr::filter(!is.na({{variable}}))
  n <- N - nrow(obj_dt)
  if (n > 0)
    msg("Eliminadas ",n," observaciones NA de ",N," (",redondea2(n/N*100),"%)")

  if (!is.null(inf) || !is.null(sup)) {
    N <- nrow(obj_dt)
    if (!is.null(inf)) obj_dt %<>% dplyr::filter({{variable}} >= inf)
    if (!is.null(sup)) obj_dt %<>% dplyr::filter({{variable}} < sup)
    n <- N - nrow(obj_dt)
    if (n > 0)
      msg("Eliminadas ",n," observaciones de ",N," (",redondea2(n/N*100),"%)")
  }

  x_vals <- obj_dt %>% dplyr::pull({{variable}})
  if (lubridate::is.instant(x_vals)) {
    x_param <- param.Date(x_vals, .bw)
    if (lubridate::is.POSIXt(x_vals))
      scale_x <- ggplot2::scale_x_datetime
    else if (lubridate::is.Date(x_vals))
      scale_x <- ggplot2::scale_x_date
    else
      stop("Tipo de dato no soportado: ", class(x_vals))
  } else {
    x_param <- param.default(x_vals, .bw)
    scale_x <- ggplot2::scale_x_continuous
  }
  # print(x_param)

  # y_vals <- obj_dt %>% dplyr::count(cut({{variable}}, breaks = hist_brks)) %>% dplyr::pull(n)
  # y_param <- param.default(y_vals)
  # print(y_param)

  if (lubridate::is.instant(x_vals)) {
    .bw %<>% factor(levels = dflt$units$datetime, ordered = T)
    x_lab <- switch(lan,
      "sp" = paste(dflt$units$sp$datetime[as.integer(.bw)],"de",x_lab),
      paste(dflt$units$en$datetime[as.integer(.bw)],"of",x_lab)
    )
  }
  gg <- ggplot2::ggplot(obj_dt, ggplot2::aes({{variable}})) +
    ggplot2::geom_histogram(breaks = seq(x_param$lims[1], x_param$lims[2], x_param$bw),
                            closed = x_param$clsd, colour = "white", na.rm = T) +
    # ggplot2::geom_histogram(binwidth = x_param$bw, boundary = x_param$lims[1],
    #                         closed = x_param$clsd, colour = "white", na.rm = T) +
    # scale_x(breaks = seq(x_param$lims[1], x_param$lims[2], x_param$by),
    #         limits = x_param$lims) +
    # ggplot2::scale_y_continuous(breaks = seq(y_param$lims[1], y_param$lims[2], y_param$by),
    #                             limits = y_param$lims) +
    ggplot2::labs(x = x_lab, y = switch(lan,
                                        "sp" = "Cantidad de parques",
                                        "Number of gardens"))
  if (is.numeric(x_vals))
    gg <- gg +
      ggplot2::scale_x_continuous(breaks = seq(x_param$lims[1], x_param$lims[2], x_param$by),
                                  limits = x_param$lims)

  if (!is.null(f_save)) {
    fichero <- substitute(variable)
    if (lubridate::is.instant(x_vals))
      fichero %<>% paste0("_",stringr::str_sub(dflt$units$sp$datetime[as.integer(.bw)], end = 3))
    if (!is.null(inf)) fichero %<>% paste0('_ge_',inf)
    if (!is.null(sup)) fichero %<>% paste0('_l_',sup)
    fichero %<>% paste0(".",f_save)
    msg("Guardando ",fichero)
    ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                    width = 7, height = 5, units = "in")
  }

  if (ply)
    return(plotly::ggplotly(gg))
  else
    return(gg)
}



