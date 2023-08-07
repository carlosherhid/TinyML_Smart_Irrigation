wd <- if (Sys.info()["sysname"] == "Windows") "." else
  file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
source(file.path(wd,"code","include.R"), encoding = "UTF-8")

msg("------------------ BEGIN savesPaper ------------------")

dirs$data <- "datosTFG"
dirs$saves <- "savesPaper"
sapply(dflt$eemm, DatAmb$completarDatMet)
dflt$ini <- as.Date("2019-07-01")
dflt$fin <- as.Date("2021-05-17")

ply <- F
dflt$lan <- "en"
f_save <- "png"
# f_save <- "svg"

#####
contratos <- dflt$contratos("ConSeries")
#####

contrato <- "6373577"
dia <- "20200510"

{
  dg <- dflt$dg() %>%
    #####
  dplyr::filter(Contrato %in% contratos)
  #####

  {
    # webshot::install_phantomjs()
    m <- DatGeo$plotMapa(dg,"Contrato","Contrato",contrato,
                         nombre_fichero = "MapaCWorld")
    m <- DatGeo$plotMapa(dg,"Contrato","Contrato",contrato,
                         nombre_fichero = "MapaCWorldCtrl", remove_controls = F)

    m <- DatGeo$plotMapa(dg,"Contrato", width = 1050, height = 750,
                         nombre_fichero = "MapaWorld")
    m <- DatGeo$plotMapa(dg,"Contrato", width = 1050, height = 750,
                         nombre_fichero = "MapaWorldCtrl", remove_controls = F)
  }

  {
    val <- 50; x_lab <- "Number of polygons"
    DatInt$plotHist(dg, Poligonos, x_lab,# sup = val,
                    f_save = f_save, ply = ply)
    # Eliminadas 2 observaciones de 157 (1.27%)
    n = dg %>% dplyr::filter(Poligonos == 1) %>% nrow()
    N = dg %>% nrow()
    msg(n," / ",N," = ",redondea2(n/N*100)," % (Poligonos = 1)")
    # 30 / 157 = 19.11 % (Poligonos = 1)

    val <- 12000; x_lab <- latex2exp::TeX("Total garden area ($m^2$)")
    DatInt$plotHist(dg, Area, x_lab,# sup = val,
                    f_save = f_save, ply = ply)
    # Eliminadas 4 observaciones de 157 (2.55%)

    val <- 10; x_lab <- latex2exp::TeX("Distance from the garden to your main station ($km$)")
    DatInt$plotHist(dg %>% dplyr::mutate(Distancia = Distancia/1000), Distancia,# sup = val,
                    x_lab, f_save = f_save, ply = ply)
    # Eliminadas 6 observaciones de 157 (3.82%)
  }

  {
    # DatGeo$consultarDatGeo("tablaEstMet", dg = dg)
    escribirTablaLatex(DatGeo$consultarDatGeo("tablaEstMet", dg = dg), "estaciones")

    # DatGeo$consultarDatGeo("tablaEstMet2", dg = dg)
    escribirTablaLatex(DatGeo$consultarDatGeo("tablaEstMet2", dg = dg), "estaciones2")

    # t(DatGeo$consultarDatGeo("tablaMosSen2", dg = dg))
    escribirTablaLatex(t(DatGeo$consultarDatGeo("tablaMosSen2", dg = dg)), "mosaicos")
  }
}

{
  # DatAmb$consultarDatMet("tablaNA")
  escribirTablaLatex(DatAmb$consultarDatMet("tablaNA"), "NAestaciones", F)

  DatAmb$plotModelDatMet("CA42", "CA91", "Prec", dflt$fin, f_save = f_save)
}

{
  # l <- dflt$lecturas()
  # Riego$resumirContadores(lecturas = l)
  # Riego$resumirLecturas(lecturas = l)

  contadoresR <- Riego$leerResumenContadores() %>%
    #####
  dplyr::filter(Contrato %in% contratos)
  #####

  # Riego$consultarResumenContadores("tabla", contadoresR)
  escribirTablaLatex(Riego$consultarResumenContadores("tabla", contadoresR), "contadores")


  lecturasR <- Riego$leerResumenLecturas() %>%
    #####
  dplyr::filter(Contrato %in% contratos) %>%
    #####
  dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), lubridate::period_to_seconds) %>%
    dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), as.difftime, units = "secs")
  units(lecturasR$Tiempo_Min) <- "mins"
  units(lecturasR$Tiempo_Mean) <- "hours"
  units(lecturasR$Tiempo_Max) <- "days"
  lecturasR %<>%
    dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), as.double) %>%
    dplyr::mutate_at(dplyr::vars(contains("Tiempo_")), round, 2)

  val <- as.Date("2017-09-30"); x_lab <- switch(dflt$lan,
                                                "sp" = "primera telelectura",
                                                "first remote reading"
  )
  DatInt$plotHist(lecturasR, Fecha_Min, x_lab,
                  # .bw = "month",
                  .bw = "quarter",
                  # inf = val,
                  f_save = f_save, ply = ply)
  # Eliminadas 130 observaciones de 157 (82.80%)

  # val <- as.Date("2021-05-01"); x_lab <- switch(dflt$lan,
  #   "sp" = "última telelectura",
  #   "last remote reading"
  # )
  # DatInt$plotHist(lecturasR, Fecha_Max, x_lab,
  #                 # .bw = "month",
  #                 .bw = "quarter",
  #                 sup = val,
  #                 f_save = f_save, ply = ply)
}

{
  {
    dg <- dflt$dg() %>%
      dplyr::filter(Contrato %in% contrato)

    Verdor$plotImgInd(contrato, 'RGB', NA, dia, 0, dg, f_save = f_save)

    Verdor$plotImgInd(contrato, 'SCL', NA, dia, 3, dg, f_save = f_save)

    # Verdor$plotImgInd(contrato, dflt$ind, NA, dia, 1, dg, f_save = f_save)
    # Verdor$plotImgInd(contrato, dflt$ind, NA, dia, 2, dg, f_save = f_save)
    Verdor$plotImgInd(contrato, dflt$ind, NA, dia, 3, dg, f_save = f_save)

    # Verdor$plotImgInd(contrato, dflt$ind, "nomask", dia, 3, dg, f_save = f_save)
    # Verdor$plotImgInd(contrato, dflt$ind, "nodata", dia, 3, dg, f_save = f_save)
    Verdor$plotImgInd(contrato, dflt$ind, dflt$mask, dia, 3, dg, f_save = f_save)

    # Verdor$plotHistImgInd(contrato, dflt$ind, NA, dia, f_save = f_save)
  }

  {
    v <- DatInt$getVarsSym("TotalNoNA")
    n <- paste0("N_",dflt$ind)
    idvsR <- Verdor$leerResumenSumInd() %>%
      #####
    dplyr::filter(Contrato %in% contratos) %>%
      #####
    dplyr::rename("{n}" := !!v) %>%
      dplyr::filter(!!as.symbol(n) > 0)


    x_lab <- switch(dflt$lan,
                    "sp" = paste("Valores",dflt$ind,"disponibles"),
                    paste("Available",dflt$ind,"values")
    )
    # DatInt$plotHist(idvsR, !!as.symbol(n), x_lab,
    #                 f_save = f_save, ply = ply)
    DatInt$plotHist(idvsR, N_NDVI, x_lab,
                    f_save = f_save, ply = ply)
  }
}

{
  dtsR <- DatInt$leerResumenDatInt() %>%
    #####
  dplyr::filter(Contrato %in% contratos) %>%
    #####
  dplyr::rename(N_Obs = Obs_CCs)

  x_lab <- switch(dflt$lan,
                  "sp" = "Observaciones completas disponibles",
                  "Available complete cases"
  )
  DatInt$plotHist(dtsR, N_Obs, x_lab,
                  f_save = f_save, ply = ply)
}

{
  dts <- DatInt$leerSeries(contrato) %>%
    DatInt$reestructurarSeries()

  lapply(c("BH","ET0","Prec","Consumo",Verdor$getVars()),
         function(v) DatInt$plotTS(dts, eleORvar = v, fin = dflt$fin,
                                   f_save = f_save, ply = ply))
  # Eliminadas 45 observaciones NA de 144 (31.25%)
}

{
  clust <- Clust$calcularClust()
  Clust$plotClust(clust, k = 11L, ts.as.lab = T, f_save = png) # bmp, jpeg, tiff



  res_None <- list(gbm = ML$leerResultsNone(model = "gbm", pca = 0L),
                   rf  = ML$leerResultsNone(model = "rf" , pca = 0L)) %>%
    listDF2DF("Model") %>%
    dplyr::rename(Time = elapsed) %>%
    # dplyr::mutate_at("Time", lubridate::seconds_to_period) %>%
    dplyr::mutate_if(is.numeric, round, 4) %>%
    dplyr::mutate(Scenario = ifelse(n == 1L, 1L, 2L), .before = n) %>%
    dplyr::arrange(Scenario) %>%
    dplyr::select(!c(PCA,CV,user,system))
  escribirTablaLatex(res_None, "res12", F)


  res_todos <- res_None %>%
    dplyr::filter(Scenario == 1L) %>%
    dplyr::select(!Scenario)


  lan = dflt$lan
  carpeta = paths$saves(lan)


  res_TSclust <- Clust$readResultsClust(series = dflt$serie, pkgs = "TSclust",
                                        disss = dflt$disss[-c(26,27,33)],
                                        models = dflt$models[5:6]) %>%
    dplyr::rename(Time = elapsed) %>%
    # dplyr::mutate_at("Time", lubridate::seconds_to_period) %>%
    # dplyr::mutate_if(is.numeric, round, 4) %>%
    dplyr::select(!c(Pkg, Serie, PCA, CV, user, system))


  best_TSclust <- res_TSclust %>% dplyr::group_by(Diss, Model) %>%
    dplyr::slice_min(CVRMSE) %>% dplyr::ungroup() %>%
    data.table::as.data.table() %>%
    dplyr::arrange(factor(Diss, levels = dflt$disss)) %>%
    dplyr::mutate_if(is.numeric, round, 4)
  best_TSclust_model <- split(best_TSclust, by = "Model", keep.by = F) %>%
    lapply(function(dt) dt %>%
             dplyr::arrange(factor(Diss, levels = dflt$disss)) %>%
             dplyr::mutate_at("Time", lubridate::seconds_to_period))
  mapply(escribirTablaLatex, best_TSclust_model, names(best_TSclust_model), F)


  best_TSclust_vs_None <- dplyr::bind_rows(
    res_todos %>% dplyr::mutate(Diss = "None", .before = n),
    best_TSclust)
  range(best_TSclust_vs_None$CVRMSE)
  best_TSclust_vs_None$Diss %<>% factor(levels = unique(.))
  ggplot2::ggplot(best_TSclust_vs_None, ggplot2::aes(Diss, CVRMSE, fill = Model)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 50)) +
    ggplot2::coord_cartesian(ylim = c(0.2,0.26)) +
    ggplot2::xlab("Clustering")
  fichero <- paste0("clust_vs_CVRMSE",".",f_save)
  msg("Guardando ",fichero)
  ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                  width = 7, height = 5, units = "in")


  res_TSclust_n_1 <- dplyr::bind_rows(
    tidyr::expand_grid(Diss = unique(res_TSclust$Diss),
                       res_todos),
    res_TSclust)
  best_diss_4_TSclust <- best_TSclust %>%
    dplyr::group_by(Model) %>% dplyr::slice_min(CVRMSE, n = 4) %>%
    dplyr::ungroup() %>% dplyr::select(Model, Diss)
  res_4_TSclust_n_1 <- dplyr::semi_join(res_TSclust_n_1,
                                        best_diss_4_TSclust)
  ggplot2::ggplot(res_4_TSclust_n_1, ggplot2::aes(n, CVRMSE, colour = Diss)) +
    ggplot2::geom_point(size = 0.5) + ggplot2::geom_line(size = 0.1) +
    ggplot2::facet_wrap(ggplot2::vars(Model))
  fichero <- paste0("n_vs_CVRMSE",".",f_save)
  msg("Guardando ",fichero)
  ggplot2::ggsave(fichero, device = f_save, path = carpeta,
                  width = 7, height = 5, units = "in")
}

msg("------------------ END savesPaper ------------------")
