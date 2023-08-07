if (!exists("paths") || is.null(paths)) {
  wd <- if (Sys.info()["sysname"] == "Windows") "." else
    file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  source(file.path(wd,"code","include.R"), encoding = "UTF-8")
}



DatGeo <- list(
  epsg = list(
    wgs84geo  = 4326L, wgs84utm30N  = 32630L,
    etrs89geo = 4258L, etrs89utm30N = 25830L,
    ed50geo   = 4230L, ed50utm30N   = 23030L
  )
)

###############################
# Fichero 1.1. Contratos

#' @param contratos Identificadores (`strings`) de los contratos de interés.
#' @return Un `data.table` con los contratos y su tipo.
DatGeo$leerContratos <- function(contratos = DatGeo$leerRelaciones()$Contrato) {

  fichero = paths$geo("contratos")
  if (!file.exists(fichero))
    stop("No se encuentra el fichero ", fichero)

  data.table::fread(fichero, sep = ";", encoding = 'UTF-8') %>%

    dplyr::rename_at(dplyr::vars(-`NIF/NIE`), stringr::str_to_title) %>%
    dplyr::rename_all(stringr::str_replace_all, "\\.| ", "") %>%

    dplyr::select(Contrato, TipoContrato) %>%

    dplyr::mutate_at("Contrato", stringr::str_replace_all, "\\.", "") %>%
    # dplyr::mutate_at("Contrato", as.integer) %>%
    # dplyr::mutate_all(as.factor) %>%
    dplyr::mutate_at("TipoContrato", as.factor) %>%

    dplyr::filter(Contrato %in% contratos)
}

###############################
# Fichero 1.2. Relaciones

#' @param acometidas Identificadores (`strings`) de las acometidas de interés.
#' @param objetivo
#' @return Un `data.table` con las acometidas, sus contratos y otros datos
#'   dependiendo de `objetivo`.
DatGeo$leerRelaciones <- function(acometidas = DatGeo$leerResumenZonasVerdes()$Acometida,
                                  objetivo = c("ubicacion", "consumo", "ambos")[3]) {

  fichero = paths$geo("relaciones")
  if (!file.exists(fichero))
    stop("No se encuentra el fichero ", fichero)

  tipo <- c("Contrato", "Acometida", "Actividad", "Ext/Int", "Prop/Inqui")
  ubicacion <- c("Direccion", "ComplementoDireccion", "Localidad", "Barrio")
  contador <- c("Contador", "FechaInstContador", "Telectura",
                "TipoContador", "ModeloContador", "Calibre")
  vars <- switch(objetivo,
                 "ubicacion" = c(tipo,ubicacion),
                 "consumo" = c(tipo,contador),
                 c(tipo,ubicacion,contador))

  readxl::read_excel(fichero) %>%
    data.table::as.data.table() %>%
    # data.table::setDT() %>%

    dplyr::rename_all(stringr::str_replace_all, "Nº ?|DE ", "") %>%
    dplyr::rename_all(stringr::str_replace_all, "_|\\.", " ") %>%
    dplyr::rename_at(dplyr::vars(-CNAE), stringr::str_to_title) %>%
    dplyr::rename_all(stringr::str_replace_all, " ", "") %>%

    # dplyr::select(dplyr::all_of(vars)) %>%
    dplyr::filter(!is.na(Acometida) & Acometida %in% acometidas) %>%
    # dplyr::filter(#Actividad != "VIVIENDAS" &
    #                 TipoSuministro == "CONTADOR") %>%# & `Ext/Int` == "E") %>%

    dplyr::mutate_all(as.factor) %>%
    dplyr::mutate_at(c(tipo[1:2],ubicacion[1:2],contador[1:2]), as.character) %>%
    dplyr::mutate_at("FechaInstContador",
                     ~ifelse(.x=="01/01/0001", NA_character_, .x)) %>%
    dplyr::mutate_at("FechaInstContador", as.numeric) %>%
    dplyr::mutate_at("FechaInstContador", as.Date, origin = "1899-12-30") %>%

    dplyr::select(dplyr::all_of(vars))
}

###############################
# Fichero 2.1. Acometidas

#' @param acometidas Identificadores (`strings`) de las acometidas de interés.
#' @return Un `data.table` con las acometidas y sus coordenadas UTM.
DatGeo$leerAcometidas <- function(acometidas = DatGeo$leerResumenZonasVerdes()$Acometida) {

  fichero = paths$geo("acometidas")
  if (!file.exists(fichero))
    stop("No se encuentra el fichero ", fichero)

  data.table::fread(fichero, sep = ";", encoding = 'UTF-8') %>%

    dplyr::rename_all(stringr::str_extract, "[^,]*(?=,)") %>%
    dplyr::rename(Acometida = AC_ID, Abcisa = COOR_X, Norte = COOR_Y) %>%

    dplyr::select(Acometida, Abcisa, Norte) %>%
    dplyr::filter(!is.na(Acometida) & Acometida %in% acometidas) %>%

    # dplyr::mutate_at("Acometida", as.factor) %>%
    dplyr::mutate_at("Acometida", as.character) %>%
    dplyr::mutate_at(c("Abcisa", "Norte"), stringr::str_replace, ",", ".") %>%
    dplyr::mutate_at(c("Abcisa", "Norte"), as.numeric)
}

###############################
# Fichero 2.2. Zonas Verdes

#' @return Un objeto `sf` con los contornos y áreas de las zonas verdes,
#'   a qué parque (acometida) pertenecen, entre otros.
DatGeo$leerZonasVerdes <- function() {

  fichero = paths$geo("zonas")
  if (!file.exists(fichero))
    stop("No se encuentra el fichero ", fichero)

  # data.table::fread(fichero, encoding = 'UTF-8') %>%
  sf::st_read(fichero, drivers = "ESRI Shapefile", quiet = T) %>%

    dplyr::rename_all(stringr::str_replace_all, "_?ID_?", "_ID_") %>%
    dplyr::rename_all(stringr::str_replace_all, "_", " ") %>%
    dplyr::rename_all(stringr::str_to_title) %>%
    dplyr::rename_all(stringr::str_replace_all, " ", "") %>%
    dplyr::rename(Acometida = CapaCad, Observacion = GnrTxt2,
                  Area = ShapeStar, Perimetro = ShapeStle) %>%

    dplyr::select(#ZrArtific, ZrSuperfi, NTx,
                  Acometida, Observacion, Area, Perimetro) %>%

    # dplyr::mutate_if(is.character, ~ifelse(.x=="", NA_character_, .x)) %>%
    dplyr::filter(!is.na(Acometida) & # 2 Contratos: 6412629, 6412630 para 1 Acometida: 2115050
                    !Acometida %in% c("AGRI","ASIGNAR RANACO","MURCIA_CASCO","2115050")) %>%
    dplyr::distinct() %>%

    # dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::mutate(Poligonos = vapply(Geometry, length, 0L),
                  Nodos = vapply(Geometry, function(mp)
                    sum(vapply(mp, function(p)
                      sum(vapply(p, nrow, 0L)-1L), 0L)), 0L))
}

#' Agrega los datos (contornos, áreas, ...) de `zonas` por parque (acometida) y
#'   guarda los resultados.
#' @param zonas Un objeto `sf` con los datos por zonas verdes ([DatGeo$leerZonasVerdes()]).
#' @return Nada.
DatGeo$resumirZonasVerdes <- function(zonas = DatGeo$leerZonasVerdes()) {
  msg("Resumiendo zonas verdes...")

  N <- dplyr::n_distinct(zonas$Acometida)

  suppressWarnings(
    zonas %<>% dplyr::group_by(Acometida) %>%
      dplyr::summarise(ZonasVerdes = dplyr::n()) %>% dplyr::ungroup() %>%
      split(1L:N) %>% lapply(sf::st_cast, "POLYGON") %>% dplyr::bind_rows() %>%
      dplyr::mutate(Area = sf::st_area(.),
                    Perimetro = sf::st_length(sf::st_boundary(.)),
                    Nodos = vapply(Geometry, function(p)
                      sum(vapply(p, nrow, 0L)-1L), 0L)) %>%
      dplyr::group_by(Acometida) %>%
      dplyr::summarise(
        ZonasVerdes = unique(ZonasVerdes),
        Poligonos = dplyr::n(),
        MasGrande = max(Area),
        MasPeque = min(Area),
        Area = sum(Area),
        MasLargo = max(Perimetro),
        MasCorto = min(Perimetro),
        Perimetro = sum(Perimetro),
        MaxNodos = max(Nodos),
        MinNodos = min(Nodos),
        Nodos = sum(Nodos)
      ) %>% dplyr::ungroup())

  msg("Guardando resumen zonas verdes...")
  fichero = paths$geo("zonasR")
  if (file.exists(fichero)) file.remove(fichero)
  sf::st_write(zonas, fichero, driver = "GeoJSON", quiet = T)
}

#' @return Un objeto `sf` con los contornos y áreas de los parques (acometida)
#'   generado por [DatGeo$resumirZonasVerdes()].
DatGeo$leerResumenZonasVerdes <- function(update = F) {

  fichero = paths$geo("zonasR")
  if (!file.exists(fichero) || update)
    DatGeo$resumirZonasVerdes()

  sf::st_read(fichero, drivers = "GeoJSON", quiet = T) %>%
    # dplyr::mutate_if(is.character, as.factor) %>%
    dplyr::rename(Geometry = geometry)
}

###############################
# Tarea 1.1. Zonas

#' @param zonas Un objeto `sf` (ver [DatGeo$leerResumenZonasVerdes()]).
#' @param acometidas Un `data.table` (ver [DatGeo$leerAcometidas()]).
#' @param relaciones Un `data.table` (ver [DatGeo$leerRelaciones()]).
#' @param contratos Un `data.table` (ver [DatGeo$leerContratos()]).
#' @param indicacion Un `booleano`.
#' @return Un objeto `sf` con todos los datos juntos.
DatGeo$reunirTablas <- function(zonas = DatGeo$leerResumenZonasVerdes(),
                                acometidas = DatGeo$leerAcometidas(zonas$Acometida),
                                relaciones = DatGeo$leerRelaciones(zonas$Acometida, "ambos"),
                                contratos = DatGeo$leerContratos(relaciones$Contrato),
                                indicacion = T) {
  msg("Reuniendo zonas verdes, acometidas y contratos...")

  if (indicacion)
    relaciones %<>% dplyr::mutate(Indicacion = Direccion %>%
        stringr::str_extract("(?<= )(SN|[0-9]+( E/[AB]| M-1)?-JA).*") %>%
        as.factor(), .after = "Direccion") %>%
      dplyr::mutate_at("Direccion", stringr::str_extract,
                       ".*(?= (SN|[0-9]+( E/[AB]| M-1)?-JA))")

  dg <- merge(relaciones, contratos, by = "Contrato", all = T)
  dg <- merge(acometidas, dg, by = "Acometida", all = T)
  dg <- merge(zonas, dg, by = "Acometida", all = T)
  dg %>% dplyr::arrange(Localidad, Barrio, Contrato, Acometida)
}

###############################
# Tarea 1.2. EstMet

#' @param estaciones Identificadores (`strings`) de las estaciones meteorológicas de interés.
#' @return Un `data.table`
#' @export
DatGeo$leerEstMet <- function(estaciones = dflt$eemm) {

  fichero = paths$geo("estaciones")
  if (!file.exists(fichero))
    stop("No se encuentra el fichero ", fichero,
         "\nAquí, https://datosabiertos.regiondemurcia.es/carm/catalogo/medio-ambiente/estaciones-meteorologicas-de-la-red-siam-carm,",
         "\nte puedes descargar un CSV con las estaciones meteorológicas y datos sobre ellas.")

  data.table::fread(fichero) %>%
    dplyr::filter(CODEST %in% estaciones)
}

#' @param dg Un objeto `sf`
#' @param estaciones
#' @return Un objeto `sf`
#' @seealso [DatGeo$leerEstMet()]
DatGeo$asignarEstMet <- function(dg = DatGeo$reunirTablas(),
                                 estaciones = dflt$eemm) {
  msg("Asignando estaciones principal y alternativas a los parques...")

  de <- DatGeo$leerEstMet(estaciones) %>%
    sf::st_as_sf(coords = c("LONGITUD", "LATITUD"), crs = DatGeo$epsg$etrs89geo)
  # ptos <- sf::st_drop_geometry(dg) %>%
  #   sf::st_as_sf(coords = c("Abcisa", "Norte"), crs = DatGeo$epsg$etrs89utm30N)
  ptos <- sf::st_centroid(dg) %>%
    sf::st_transform(sf::st_crs(de))

  prox <- sf::st_distance(ptos, de)
  # units(prox) <- units::make_units(km)
  rownames(prox) <- paste(ptos$Contrato,ptos$Acometida, sep = "_")
  colnames(prox) <- de$CODEST

  best_prox <- apply(prox, 1L, min)
  # df_best_prox <- data.frame(Distancia = best_prox)
  dg$Distancia <- best_prox

  names_by_prox <- t(apply(prox, 1L, function(fila) colnames(prox)[order(fila)]))
  # colnames(names_by_prox) <- c("Principal",paste0("Alternativa",1L:(length(estaciones)-1L)))
  # df_names_by_prox <- as.data.frame(names_by_prox)
  dg$Principal <- names_by_prox[,1]
  for (i in 1L:(length(estaciones)-1L))
    dg[[paste0("Alternativa",i)]] <- names_by_prox[,i+1L]

  # dg %>% cbind(df_best_prox) %>% cbind(df_names_by_prox)
  dg
}

###############################
# Tarea 1.3. MosSen2

#' @param dg Un objeto `sf`
#' @return Un objeto `sf`
DatGeo$asignarMosSen2 <- function(dg = DatGeo$asignarEstMet()) {
  msg("Identificando mosaicos Sentinel-2 en los que se encuentran los parques...")

  .s2tiles <- sen2r::s2_tiles()
  sel_tiles <- sen2r::tiles_intersects(dg, all = T, .s2tiles = .s2tiles)
  .s2tiles %<>% dplyr::filter(tile_id %in% sel_tiles)
  dgs <- split(dg,1L:nrow(dg))

  min_tiles_intersects <- function(dg) {
    length(sen2r::tiles_intersects(dg, .s2tiles = .s2tiles))
  }
  dg$MinMosaicos <- vapply(dgs, min_tiles_intersects, 0L)

  max_tiles_intersects <- function(dg) {
    length(sen2r::tiles_intersects(dg, all = T, .s2tiles = .s2tiles))
  }
  dg$MaxMosaicos <- vapply(dgs, max_tiles_intersects, 0L)

  dg$Mosaico <- vapply(dgs, sen2r::tiles_intersects, "", .s2tiles = .s2tiles)

  .s2tiles %<>% dplyr::arrange(dplyr::desc(tile_id))
  dg$Mosaico2 <- vapply(dgs, sen2r::tiles_intersects, "", .s2tiles = .s2tiles)

  dg
}

###############################
# Tarea 1.

#'
#' @param estaciones
#' @return Nada
#' @seealso [DatGeo$reunirTablas()],[DatGeo$asignarEstMet()],[DatGeo$asignarMosSen2()]
#' @export
DatGeo$initJardines <- function(estaciones = dflt$eemm) {
  msg("Inicializando datos geograficos de jardines...")

  # DatGeo$resumirZonasVerdes()
  dg <- DatGeo$reunirTablas()
  dg <- DatGeo$asignarEstMet(dg, estaciones)
  dg <- DatGeo$asignarMosSen2(dg)

  msg("Guardando datos geograficos de jardines...")
  fichero = paths$geo("jardi")
  if (file.exists(fichero)) file.remove(fichero)
  sf::st_write(dg, fichero, driver = "GeoJSON", quiet = T)

  # dg %<>% sf::st_transform(crs = 4326) # NO es necesario
  # GUARDAMOS LAS PARCELAS, CADA UNA EN SU FICHERO
  dg %<>% dplyr::mutate(ID = ifelse(is.na(Contrato), Acometida, Contrato))
  dg %<>% split(dg$ID)
  msg("Guardando parcelas para sen2r...")
  carpeta <- paths$s2r("parcelas")
  ficheros <- newFile(carpeta, names(dg), "geojson")
  file.remove(ficheros[file.exists(ficheros)])
  status <- mapply(sf::st_write, dg, ficheros, driver = "GeoJSON", quiet = T)
}

#' @param contratos Identificadores (`strings`) de los contratos de interés
#' @param acometidas Identificadores (`strings`) de las acometidas de interés
#' @param estaciones
#' @return Un objeto `sf` ... [DatGeo$initJardines()]
#' @export
DatGeo$leerJardines <- function(contratos = NULL, acometidas = NULL,
                                estaciones = dflt$eemm,
                                update = F) {

  fichero = paths$geo("jardi")
  if (!file.exists(fichero) || update)
    DatGeo$initJardines(estaciones)

  dg <- streadGeoJSON(fichero, estaciones)

  if (!is.null(acometidas))
    dg %<>%
      dplyr::slice(match(acometidas, Acometida))
  else if (!is.null(contratos))
    dg %<>% dplyr::filter(Contrato %in% contratos) %>%
      dplyr::arrange(factor(Contrato, levels = contratos))

  dg
}

###############################
# Tareas 3. y 6.2

####
#' @param consulta Una de `c("tablaEstMet", "tablaEstMet2", "tablaMosSen2", "listEstMet", "listEstMet2", "listMosSen2", "setEstMet", "setEstMet2", "setMosSen2")`
#' @param contratos Identificadores (`strings`) de los contratos de interés
#' @param acometidas Identificadores (`strings`) de las acometidas de interés
#' @param estaciones
#' @param dg  Un objeto `sf`
#' @return
#' @export
DatGeo$consultarDatGeo <- function(consulta = "listEstMet",
                                   contratos = NULL,
                                   acometidas = NULL,
                                   estaciones = dflt$eemm,
                                   dg = DatGeo$leerJardines(contratos,acometidas,estaciones)) {
  if (!is.null(acometidas))
    dg %<>%
    dplyr::slice(match(acometidas, Acometida))
  else if (!is.null(contratos))
    dg %<>% dplyr::filter(Contrato %in% contratos) %>%
      dplyr::arrange(factor(Contrato, levels = contratos))

  if (inherits(dg,"sf")) dg %<>% sf::st_drop_geometry()
  dg %<>% dplyr::distinct(Acometida, .keep_all = T)

  if (stringr::str_detect(consulta, "MosSen2$"))
    dg %<>% dplyr::transmute(Mosaicos = ifelse(MaxMosaicos==1,
                                               as.character(Mosaico),"Ambos"))
  else if (stringr::str_detect(consulta, "EstMet2$"))
    dg %<>% dplyr::select(c("Principal","Alternativa1"))
  else if (stringr::str_detect(consulta, "EstMet$"))
    dg %<>% dplyr::select(c("Principal",paste0("Alternativa",1L:(length(estaciones)-1L))))
  else
    stop("Consulta no válida: ", consulta)


  if (stringr::str_detect(consulta, "^set"))
    dg %<>% dplyr::distinct()

  if (stringr::str_detect(consulta, "^(set|list)")) {
    df2vect <- function(df) as.vector(as.matrix(df))

    if (nrow(dg) == 1L)
      lista <- df2vect(dg)
    else
      lista <- lapply(split(dg, 1L:nrow(dg)), df2vect)

    if (stringr::str_detect(consulta, "MosSen2$")) {
      ambos2tiles <- function(v) if (v == "Ambos") c("30SXH","30SXG") else v

      if (nrow(dg) == 1L)
        lista %<>% ambos2tiles()
      else
        lista %<>% lapply(ambos2tiles)
    }

    return(lista)

  } else if (stringr::str_detect(consulta, "^tabla")) {
    tabla <- switch(consulta,
                    "tablaEstMet" = dg %>% sapply(summary) %>% t(),
                    "tablaEstMet2" = table(dg$Principal,dg$Alternativa1),
                    "tablaMosSen2" = table(dg$Mosaicos),
                    stop("Consulta no válida: ", consulta)
    )
    if (stringr::str_detect(consulta, "^tablaEstMet2?$")) {
      Total <- apply(tabla, 1L, sum)
      tabla %<>% cbind(Total)
      Total <- apply(tabla, 2L, sum)
      tabla %<>% rbind(Total)
    }
    return(tabla)

  } else {
    stop("Consulta no válida: ", consulta)
  }

}

#' @param consulta Una de `c("sinUbicacion", "sinContrato", "variosContratos")`
#' @export
DatGeo$consultarJardines <- function(consulta = "conContrato",
                                     res = DatGeo$leerJardines()) {

  variosContratos <- res %>% dplyr::count(Acometida) %>%
    dplyr::filter(n>1) %>% dplyr::pull(Acometida)
  switch (consulta,
    "sinUbicacion" = res %>% dplyr::filter(is.na(Abcisa) | is.na(Norte)),
    "conUbicacion" = res %>% dplyr::filter(!is.na(Abcisa) & !is.na(Norte)),
    "sinContrato" = res %>% dplyr::filter(is.na(Contrato)),
    "conContrato" = res %>% dplyr::filter(!is.na(Contrato)),
    "variosContratos" = res %>% dplyr::filter(Acometida %in% variosContratos),
    "unContrato" = res %>% dplyr::filter(!is.na(Contrato) & !Acometida %in% variosContratos),
    stop("Consulta no válida: ", consulta)
  )
}

###############################
# Tarea 6.1. Mapas e imágenes

#' @param obj_sf
#' @param group_var
#' @param filter_var
#' @param filter_values
#' @param plot_var
#' @param palete
#' @param n
#' @param color
#' @param fillColor
#' @param cluster
#' @param lan
#' @param nombre_fichero
#' @param width
#' @param height
#' @param remove_controls
#' @return
#' @export
DatGeo$plotMapa <- function(obj_sf = DatGeo$leerJardines(), group_var = "Barrio",
                            filter_var = NULL, filter_values = NULL, plot_var = NULL,
                            n = 3L, cluster = T, palete = NULL, fillColor = NULL,
                            color = NULL, sz = 1, r = 10, lan = dflt$lan,
                            carpeta = paths$saves(lan), nombre_fichero = NULL,
                            width = 700, height = 500, remove_controls = T) {

  if (!is.null(filter_var) & !is.null(filter_values))
    obj_sf %<>% dplyr::filter(obj_sf[[filter_var]] %in% filter_values)
  obj_sf %<>% dplyr::mutate_if(is.factor, droplevels)

  # reproject
  obj_sf %<>% sf::st_transform(DatGeo$epsg$wgs84geo)

  mapa <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 10,
                                                             maxZoom = 21))

  group <- if (is.null(group_var)) switch(lan,
                                          "sp" = "Jardines",
                                          "Gardens") else obj_sf[[group_var]]
  popup <- if ("Contrato" %in% names(obj_sf))
    paste0("<strong>Acometida: </strong>", obj_sf$Acometida,
           "<br><strong>Contrato: </strong>", obj_sf$Contrato) else
             if ("CODEST" %in% names(obj_sf))
               paste0("<br><strong>Estacion: </strong>", obj_sf$CODEST)

  if (!is.null(plot_var)) {

    label <- paste0(plot_var,": ", obj_sf[[plot_var]])

    # numeric to factor (colorQuantile may fail)
    if (is.numeric(obj_sf[[plot_var]])) {
      breaks_qt <- stats::quantile(obj_sf[[plot_var]], seq(0,1,length.out = n+1L))
      breaks_qt[1] %<>% floor()
      breaks_qt %<>% ceiling()
      while (anyDuplicated(breaks_qt))
        breaks_qt[duplicated(breaks_qt)] <- breaks_qt[duplicated(breaks_qt)] + 1
      obj_sf[[plot_var]] %<>% cut(breaks_qt, right = F, include.lowest = T)

      if (is.null(palete)) palete = "YlOrRd"
      # pal_fun <- leaflet::colorNumeric(palete, NULL)
    } else {
      if (is.null(palete)) palete = "Spectral"
    }
    pal_fun <- leaflet::colorFactor(palete, obj_sf[[plot_var]])
    fillColor = pal_fun(obj_sf[[plot_var]])

    # if (is.null(color)) color = "black"
    if (is.null(color)) color = fillColor

  } else {

    label = NULL
    if (is.null(fillColor)) fillColor = "transparent"
    if (is.null(color)) color = "#03F"

  }

  type <- sf::st_geometry_type(obj_sf, F)
  if (type %in% c("POLYGON","MULTIPOLYGON","CURVEPOLYGON")) {

    mapa %<>% leaflet::addPolygons(
      data = obj_sf,
      smoothFactor = 0.5, weight = sz, color = color, opacity = 1,
      fillColor = fillColor, fillOpacity = 0.8,
      popup = popup, label = label, group = group,
      highlightOptions = leaflet::highlightOptions(
        weight = 2, color = "white", bringToFront = T
      )
    )

  } else if (type %in% c("POINT","MULTIPOINT")) {

    # maxClusterRadius = 50
    if (cluster) clusterOptions = leaflet::markerClusterOptions()
    else clusterOptions = NULL

    g_obj_sf <- split(obj_sf, group)
    g_popup <- split(popup, group)

    if (!is.null(plot_var)) {

      g_label <- split(label, group)
      g_fillColor <- split(fillColor, group)

      for (g in names(g_obj_sf))
        mapa %<>% leaflet::addCircleMarkers(
          data = g_obj_sf[[g]],
          radius = r, weight = sz, color = "black", opacity = 1,
          fillColor = g_fillColor[[g]], fillOpacity = 0.8,
          popup = g_popup[[g]], label = g_label[[g]], group = g,
          clusterOptions = clusterOptions
        )

    } else {

      for (g in names(g_obj_sf))
        mapa %<>% leaflet::addMarkers(
          data = g_obj_sf[[g]],
          popup = g_popup[[g]], group = g,
          clusterOptions = clusterOptions
        )

    }

  }

  if (!is.null(plot_var))
    mapa %<>% leaflet::addLegend("bottomleft", title = plot_var,
                                 pal = pal_fun, values = obj_sf[[plot_var]])

  # bb <- sf::st_bbox(obj_sf)

  mapa %<>%
    # leaflet::setView(lng = -1.150927, lat = 37.89295, zoom = 10) %>%
    # leaflet::setMaxBounds(lng1 = bb[1], lat1 = bb[2],
    #                       lng2 = bb[3], lat2 = bb[4]) %>%
    leaflet::addTiles(group = "STREET") %>%
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "WORLD") %>%
    # leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap, group = "STREET") %>%
    leaflet::addMiniMap(tiles = leaflet::providers$Esri.WorldStreetMap, toggleDisplay = T) %>%
    leaflet::addLayersControl(
      # options = leaflet::layersControlOptions(collapsed = F),
      baseGroups = c("WORLD", "STREET"), overlayGroups = unique(group)
    )

  if (!is.null(nombre_fichero)) {
    fichero <- newFile(carpeta, nombre_fichero, "png")
    msg("Guardando ",fichero)
    if (remove_controls)
      mapview::mapshot(mapa, file = fichero, vwidth = width, vheight = height)
    else
      mapview::mapshot(mapa, file = fichero, vwidth = width, vheight = height,
                       remove_controls = NULL)

  }

  mapa
}



