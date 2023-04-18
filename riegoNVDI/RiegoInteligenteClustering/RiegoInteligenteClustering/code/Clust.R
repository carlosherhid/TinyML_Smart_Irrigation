if (!exists("paths") || is.null(paths)) {
  wd <- if (Sys.info()["sysname"] == "Windows") "." else
    file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  source(file.path(wd,"code","include.R"), encoding = "UTF-8")
}



#' Clustering de jardines
#'
#' @param ind(s)
#'   (Vector de) Ã­ndice(s) de vegetaciÃ³n.
#'   Posibles valores: `r dflt$inds`.
#' @param mask(s)
#'   (Vector de) mÃ¡scara(s) de nubes.
#'   Posibles valores: `r dflt$masks`.
#' @param agg(s)
#'   (Vector de) mÃ©todo(s) de agregaciÃ³n de imÃ¡genes.
#'   Posibles valores: `r dflt$aggs`.
#' @param crop(s)
#'   (Vector de) cultivo(s) de referencia para la `et0`.
#'   Posibles valores: `r dflt$crops`.
#' @param int(s)
#'   (Vector de) mÃ©todo(s) de integraciÃ³n de series de datos.
#'   Posibles valores: `r dflt$ints`
#' @param model(s)
#'   (Vector de) modelo(s) de machine learning para entrenar.
#'   Posibles valores: `r dflt$models`.
#' @param serie(s)
#'   (Vector de) serie(s) de datos para realizar el clustering
#'   Uno o varios de `r dflt$series`.
#' @param pkg
#'   Paquete a usar para realizar el clustering.
#'   Uno de `r dflt$pkgs`.
#' @param diss,disss
#'   MÃ©todo de disimilitud de series temporales.
#'   SÃ³lo tiene utilidad para `pkg="TSclust"`.
#'   Uno o varios de `r dflt$disss`.
#' @param aggl
#'   MÃ©todo de aglomeraciÃ³n de clustering jerÃ¡rquico.
#'   Uno de `c("complete","average","single")`.
#' @param ini,fin
#' @name clust
Clust <- list()

#' @rdname clust
Clust$prepararDatos <- function(ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                                serie = dflt$serie, pkg = dflt$pkg,
                                ini = dflt$ini, fin = dflt$fin) {

  contratos <- dflt$contratos("ConSeries", ind, mask, agg)
  if (serie == "Riego")
    contratos %<>% intersect(dflt$contratos("ConConsumos"))

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
          idvs <- Verdor$readSumInd(contratos, ind, mask, ini = ini, fin = fin)
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

  attr(S, "param")   <- list(cjto = list(ind = ind, mask = mask, agg = agg),
                             serie = serie, pkg = pkg)
  return(S)
}

#' @rdname clust
Clust$tsclust <- function(S, diss = dflt$diss, ..., aggl = "complete") {

  if (diss %in% c("NCDsym", "CDMsym")) {
    S %<>% dplyr::mutate_all(function(v) (v - mean(v)) / sd(v))
    # if (!is.null(w)) S %<>% dplyr::mutate_all(TSclust::PAA, w)
    S %<>% dplyr::mutate_all(TSclust::convert.to.SAX.symbol, alpha = 27)
  }

  D <- switch (diss,
    "FRECHET" = {
      capture.output(
        D <- TSclust::diss(S, diss, ...)
      , file = nullfile(), type = "output")
      D
    },
    "DTWARP"  = {
      suppressWarnings(
        D <- TSclust::diss(S, "DTWARP", dist.method = "Manhattan",  distance.only = T)
      )
      D
    },
    "CORT.EUCL"    = TSclust::diss(S, "CORT", k = 2, deltamethod = "Euclid"),
    "CORT.FRECHET" = {
      capture.output(
        D <- TSclust::diss(S, "CORT", k = 2, deltamethod = "Frechet")
      , file = nullfile(), type = "output")
      D
    },
    "CORT.DTW"     = {
      suppressWarnings(
        D <- TSclust::diss(S, "CORT", k = 2, deltamethod = "DTW")
      )
      D
    },
    "COR.1"   = TSclust::diss(S, "COR", beta = NULL),
    "COR.2.0" = TSclust::diss(S, "COR", beta = 0.5),
    "COR.2.1" = TSclust::diss(S, "COR", beta = 1),
    "COR.2.2" = TSclust::diss(S, "COR", beta = 2),
    "ACFU" = TSclust::diss(S, "ACF", p = NULL),
    "ACFG" = TSclust::diss(S, "ACF", p = 0.05),
    "PACFU" = TSclust::diss(S, "PACF", p = NULL),
    "PACFG" = TSclust::diss(S, "PACF", p = 0.05),
    "NORM.PER"     = TSclust::diss(S, "PER", normalize = T, logarithm = F),
    "LOG.NORM.PER" = TSclust::diss(S, "PER", normalize = T, logarithm = T),
    "INT.PER"      = TSclust::diss(S, "INT.PER", normalize = F),
    "NORM.INT.PER" = TSclust::diss(S, "INT.PER", normalize = T),
    "SPEC.WLS" = TSclust::diss(S, "SPEC.LLR", alpha = 0.5, method = "DLS"),
    "SPEC.WLK" = TSclust::diss(S, "SPEC.LLR", alpha = 0.5, method = "LK"),
    "MINDIST.SAX" = TSclust::diss(S, "MINDIST.SAX", w = nrow(S), alpha = 27),
    "AR.MAHext"   = TSclust::diss(S, "AR.MAH", dependence = T),
    "AR.LPC.CEPS" = {
      suppressWarnings(
        D <- TSclust::diss(S, diss, ...)
      )
      D
    },
    "NCDsym" = TSclust::diss(S, "NCD", type = "min"),
    "CDMsym" = TSclust::diss(S, "CDM", type = "min"),
    "PDCext" = if (attr(S, "param")$serie == "Riego")
        TSclust::diss(S, "PDC", m = 5, t = 7)
      else TSclust::diss(S, "PDC", m = 3, t = 4),
    # "PRED" = TSclust::diss(S, "PRED", h = 1,
    #               # models = c("ets","arima"),
    #               logarithms = F, differences = F)
    TSclust::diss(S, diss, ...)
  ) #se termina de definir D
  if (diss %in% c("AR.MAH", "AR.MAHext"))
    D <- D$statistic

  hcl <- stats::hclust(D, aggl) ##clustering, despues se hace cuttree
  hcl$diss  <- diss
  hcl$N <- length(hcl$order)
  hcl$data <- S
  hcl$D <- D
  hcl$multichannel <- F
  return(hcl)
}
#Esta funcion hace calculos para un número máximo de clusters, y crea clústers probando desde 2 hasta el numero maximo de clusters
#si max_n_clusts== -1 se calculan todos los clustersm es decur max_n_clusts=dim(S)[2]/2 cuando max_n_clusts == -1
#' @rdname clust
Clust$clust <- function(max_n_clusts,S, pkg = dflt$pkg, diss = if (pkg == "TSclust") dflt$diss,
                        ..., aggl = "complete") {

  clust <- switch (pkg,
    # PDC with an automatic selection of the embedding dimension
    "pdc" = pdc::pdclust(S, ..., clustering.method = aggl),
    "TSclust" = Clust$tsclust(S, diss, ..., aggl = aggl),
    stop("pkg no vÃ¡lido: ",pkg)
  )
  attr(clust, "param") <- switch (pkg,
    "pdc" = attr(S, "param"),
    "TSclust" = c(attr(S, "param"), list(diss = diss)),
    stop("pkg no vÃ¡lido: ",pkg)
  )
  # the group memberships of the time series for a desired number of clusters
  # (from 2 to #contratos/2 )
  clust$grp.m <- stats::cutree(clust, k = 2:max_n_clusts)#, order_clusters_as_data = F)  se obtienen las etiquetas

  # size of the smallest group (for each number of clusters)
  clust$N.min <- apply(clust$grp.m, 2, function(g) with(rle(sort(g)), min(lengths[values])))
  # size of the biggest group (for each number of clusters)
  clust$N.max <- apply(clust$grp.m, 2, function(g) with(rle(sort(g)), max(lengths[values])))
  # contratos of the group memberships(for each number of clusters)
  clust$grp.s   <- apply(clust$grp.m, 2, function(g) lapply(1:max(g), function(k) names(g[g==k])))
  # predictive accuracy of the clustering (for each number of clusters)
  oldclass <- class(clust)
  class(clust) <- c("pdclust", "hclust")
  clust$pac     <- apply(clust$grp.m, 2, function(g) pdc::loo1nn(clust, g))
  class(clust) <- oldclass
  # clust$pac     <- apply(clust$grp.m, 2, function(g) TSclust::loo1nn.cv(clust, g))

  return(clust)
}

#' @export
#' @rdname clust
#' #Calcula los clusters llamando a la función clust, a la que se le pasa el numero maximo de clusters
Clust$calcularClust <- function(max_n_clust,ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                                serie = dflt$serie, pkg = dflt$pkg,
                                diss = if (pkg == "TSclust") dflt$diss,
                                ..., aggl = "complete",
                                ini = dflt$ini, fin = dflt$fin,
                                update = F) {

  S <- Clust$prepararDatos(ind, mask, agg, serie, pkg, ini, fin)
  clust <- Clust$clust(max_n_clust,S, pkg, diss, ..., aggl = aggl)

  # fichero <- paths$clu(list(ind,mask,agg), serie, pkg, diss)
  fichero <- do.call(paths$clu, attr(clust, "param"))
  if (!file.exists(fichero) || update) {
    clust_grp.t <- data.table::as.data.table(clust$grp.m, keep.rownames = "Contrato")
    data.table::fwrite(clust_grp.t, fichero, sep = ";")
    # save(clust, file = fichero)
  }

  return(clust)
}

#' @param clust
#' @param subcjto
#' @param k
#' @param ts.as.lab
#' @param carpeta
#' @param f_save
#' @export
#' @rdname clust
#' #hace un plot del clust en cuestion
Clust$plotClust <- function(clust, subcjto = NULL, k = 2L, ts.as.lab = T,
                            carpeta = paths$saves(), f_save = NULL) {

  contratos <- clust$labels
  if (ts.as.lab) {
    if (!is.null(subcjto)) {
      colores <- rep(dflt$cols$red, times = length(contratos))
      colores[ !(contratos %in% subcjto) ] <- dflt$cols$blue
    } else {
      # colores <- clust$grp.t[[k]]
      colores <- clust$grp.m[,as.character(k)]
    }
    if (attr(clust, "param")$pkg == "TSclust")
      class(clust) <- c("pdclust", "hclust")
    myplot <- function() {
      library(pdc)
      plot(clust, cols = colores)
      # detach("package:pdc", unload = T)
    }
  } else {
    if (!is.null(subcjto)) {
      contratos[ !(contratos %in% subcjto) ] <- ""
      labs = contratos
    } else {
      labs = F
    }
    myplot <- function() plot(clust, labels = labs)
    # abline(h = clust$height, col = "red", untf = T, lty = 2)
  }
  if (!is.null(f_save)) {
    param <- attr(clust, "param")
    param$cjto %<>% paste0(collapse = "_")
    if (param$pkg == "TSclust")
      param$pkg %<>% paste(param$diss, sep = "_")
    file_name <- paste(param[c("pkg","serie","cjto")],collapse = "_")
    if (!ts.as.lab)
      file_name %<>% paste0("_labels")
    if (!is.null(subcjto)) {
      file_name %<>% paste0("_sub")
    } else {
      if (ts.as.lab)
        file_name %<>% paste0("_",k)
    }
    fichero <- newFile(carpeta, file_name, substitute(f_save))
    msg("Guardando ",fichero)
    f_save(fichero, width = 7, height = 10, units = "in", res = 300)
    myplot()
    dev.off()
  }
  myplot()
}

#' @export
#' @rdname clust
#' #Inicializa los clusters y llama a calcularClust para que se ejecuten los clusters y se calculen
Clust$initClusts <- function(max_n_clust,inds = dflt$ind, masks = dflt$mask, aggs = dflt$agg,
                             series = dflt$series, pkgs = dflt$pkgs,
                             disss = dflt$disss[-c(26,27,33)], ...) {

  lapply(inds, function(ind) {
    lapply(masks, function(mask) {
      lapply(aggs, function(agg) {
        msg("Inicializando clusters de contratos con ",
            paste(ind,mask,agg, sep = "_"),"...")
        lapply(series, function(serie) {
          lapply(pkgs, function(pkg) {
            if (pkg != "TSclust" || serie != "VerdorMulti") {
              switch (pkg,
                "pdc" = {
                  msg("#######     ",paste(pkg,serie, sep = "_"),"     #######")
                  Clust$calcularClust(ind, mask, agg, serie, pkg, ...)
                },
                "TSclust" = my.lapply(disss, function(diss) {
                  msg("#######     ",paste(pkg,diss,serie, sep = "_"),"     #######")
                  Clust$calcularClust(max_n_clust,ind, mask, agg, serie, pkg, diss, ...)
                }),
                stop("pkg no vÃ¡lido: ",pkg)
              )
            }
          })
        })
      })
    })
  }) %>% invisible()
}

#' @export
#' @rdname clust
#' #' llama a initClusts y lee los clusts
Clust$leerClust <- function(max_n_clust,ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                            serie = dflt$serie, pkg = dflt$pkg,
                            diss = if (pkg == "TSclust") dflt$diss, ...) {

  fichero <- paths$clu(list(ind,mask,agg), serie, pkg, diss)
  if (!file.exists(fichero))
    Clust$initClusts(max_n_clust,ind, mask, agg, serie, pkg, diss, ...)

  clust <- data.table::fread(fichero, sep = ";") %>%
    dplyr::mutate_at("Contrato", as.character)
  attr(clust, "param") <- list(cjto = list(ind = ind, mask = mask, agg = agg),
                               serie = serie, pkg = pkg)
  if (pkg == "TSclust")
    attr(clust, "param")$diss <- diss
  # load(file = fichero)
  return(clust)
}

Clust$probarClust <- function(max_n_clust,ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                              crop = dflt$crop, int = dflt$int,
                              serie = dflt$serie, pkg = dflt$pkg,
                              diss = if (pkg == "TSclust") dflt$diss,
                              model = dflt$model,
                              control = ML$control, ..., extended = F) {

  clust <- Clust$leerClust(max_n_clust,ind, mask, agg, serie, pkg, diss)
  allGroups <- lapply(2:ncol(clust), function(n_clusters) {
    split(clust$Contrato, clust[[n_clusters]]) %>%
      vapply(paste0, "", collapse = "_")
  }) %>% purrr::reduce(c) %>% unique()
  results <- ML$leerResults(ind, mask, agg, crop, int, model, 0L)
  processedGroups <- results$Group
  newGroups <- setdiff(allGroups, processedGroups)
  my.lapply(newGroups, function(group) {
    contratos <- stringr::str_split(group, "_")
    ML$probarModelo(contratos, ind, mask, agg, crop, int,
                    model, 0L, control, ..., extended = extended)
  })
}
#' @rdname clust
#' #' prueba el clust y llama a leerClust, leerResults y mas importante, probarModelo, modificado el bucle para que se haga con el numero de clusters indicado
Clust$probarClust <- function(max_n_clust,ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                              crop = dflt$crop, int = dflt$int,
                              serie = dflt$serie, pkg = dflt$pkg,
                              diss = if (pkg == "TSclust") dflt$diss,
                              model = dflt$model,
                              control = ML$control, ..., extended = F) {

  clust <- Clust$leerClust(max_n_clust,ind, mask, agg, serie, pkg, diss)
  results <- ML$leerResults(ind, mask, agg, crop, int, model, 0L)
  processedGroups <- results$Group

  # bucle NO paralelizable
  for (n_clusters in 2:max_n_clust) {
    msg("\tn: ",n_clusters)

    # bucle paralelizable
    for (cluster_k_n in 1:n_clusters) {
      contratos_k_n <- clust$Contrato[clust[[n_clusters]]==cluster_k_n]
      grupo_k_n <- paste0(contratos_k_n, collapse = "_")

      # como mucho, para cada valor de n_clusters habrÃ¡ dos valores cluster_k_n
      # para los que se llame a ML$probarModelo, ya que el clustering es
      # jerÃ¡rquico
      if (!grupo_k_n %in% processedGroups) {
        msg("\t\tk: ",cluster_k_n,
            "\tProcesando grupo de ",length(contratos_k_n)," contratos")
        processedGroups %<>% c(grupo_k_n)
        ML$probarModelo(contratos_k_n, ind, mask, agg, crop, int,
                        model, 0L, control, ..., extended = extended)
      }
    }
  }
}

#' @rdname clust
#' #' saca un promedio de los resultados, llama a leerResults y leerClust
Clust$promediarResults <- function(max_n_clust,ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                                   crop = dflt$crop, int = dflt$int,
                                   serie = dflt$serie, pkg = dflt$pkg,
                                   diss = if (pkg == "TSclust") dflt$diss,
                                   model = dflt$model) {

  clust <- Clust$leerClust(max_n_clust,ind, mask, agg, serie, pkg, diss)
  results <- ML$leerResults(ind, mask, agg, crop, int, model, 0L)

  # results %<>% dplyr::select(!c(Clust))
  # msg("Grupos sin resultados: ", numNA(results$CVRMSE))

  final.results <- list()
  contratos_nmenos1 <- list(clust$Contrato)
  for (n_clusters in as.character(2:ncol(clust))) {
    # contratos_n <- lapply(1:n_clusters, function(cluster_k_n)
    #   clust$Contrato[clust[[n_clusters]]==cluster_k_n])
    contratos_n <- split(clust$Contrato, clust[[n_clusters]])
    grupos_n <- vapply(contratos_n, paste0, "", collapse = "_")
    results_n <- results %>% dplyr::filter(Group %in% grupos_n) %>%
      dplyr::arrange(factor(Group, levels = grupos_n))
    # results_n <- results %>%
    #   dplyr::slice(match(grupos_n, Group))
    # como vamos modificando results, todos los grupos sin resultados que
    # aparezcan para n clusters tienen resultado para n-1 clusters y siempre
    # hay resultado para 1 cluster
    clusters_no_results_n <- which(is.na(results_n$CVRMSE))
    for (cluster_k_n in clusters_no_results_n) {
      contratos_k_n <- contratos_n[[cluster_k_n]]
      grupo_k_n <- grupos_n[cluster_k_n]

      cluster_k_nmenos1 <- which(vapply(contratos_nmenos1, is.subset, F, x = contratos_k_n))
      contratos_k_nmenos1 <- contratos_nmenos1[[cluster_k_nmenos1]]
      grupo_k_nmenos1 <- paste0(contratos_k_nmenos1, collapse = "_")

      results_k_nmenos1 <- results %>% dplyr::filter(Group == grupo_k_nmenos1)
      results_k_n_new <- results_k_nmenos1 %>%
        dplyr::mutate(Group = results_n$Group[cluster_k_n],
                      N = results_n$N[cluster_k_n])
      results_n[cluster_k_n,] <- results_k_n_new
      results[results$Group == grupo_k_n, ] <- results_k_n_new
    }
    wt <- results_n$N / sum(results_n$N)
    weighted.mean.results <- results_n %>%
      dplyr::select(!c(Group,N,PCA,user,system,elapsed)) %>%
      dplyr::summarise_if(is.numeric, weighted.mean, wt)
    sum.times <- results_n %>%
      dplyr::select(user,system,elapsed) %>%
      dplyr::summarise_if(is.numeric, sum)
    final.results[[n_clusters]] <- cbind(weighted.mean.results, sum.times)
    contratos_nmenos1 <- contratos_n
  }
  final.results %<>% dplyr::bind_rows(.id = "n")
  final.results %<>% dplyr::mutate(PCA = 0L, .after = n)
  fichero <- paths$res(int, crop, list(ind, mask, agg), model, pkg, serie, diss)
  data.table::fwrite(final.results, fichero, sep = ";", na = "NA")
}

#' @export
#' @rdname clust
#' #' inicializa los resultados del clustering, llama a probarClust  y a promediarResults (que dejaremos desactivado por ahora, ya que realiza la inicializacion de los clusts y la lectura dos veces, lo hace ya probarClusts)
Clust$initResultsClusts <- function(max_n_clust,inds = dflt$ind, masks = dflt$mask, aggs = dflt$agg,
                                    crops = dflt$crop, ints = dflt$int,
                                    series = dflt$series, pkgs = dflt$pkgs,
                                    disss = dflt$disss[-c(26,27,33)],
                                    models = dflt$models[5:6],
                                    control = ML$control, ..., extended = F) {

  lapply(inds, function(ind) {
    lapply(masks, function(mask) {
      lapply(aggs, function(agg) {
        lapply(crops, function(crop) {
          lapply(ints, function(int) {
            msg("Inicializando resultados de contratos con ",
                paste(int,crop,ind,mask,agg, sep = "_"),"...")
            lapply(models, function(model) {
              lapply(series, function(serie) {
                lapply(pkgs, function(pkg) {
                  if (pkg != "TSclust" || serie != "VerdorMulti") {
                    switch (pkg,
                      "pdc" = {
                        msg("#######     ",paste(model,pkg,serie, sep = "_"),"     #######")
                        Clust$probarClust(max_n_clust,ind, mask, agg, crop, int, serie, pkg, NULL,
                                          model, control, ..., extended = extended)
                        Clust$promediarResults(max_n_clust,ind, mask, agg, crop, int,
                                               serie, pkg, NULL, model)
                      },
                      "TSclust" = lapply(disss, function(diss) {
                        msg("#######     ",paste(model,pkg,diss,serie, sep = "_"),"     #######")
                        Clust$probarClust(max_n_clust,ind, mask, agg, crop, int, serie, pkg, diss,
                                          model, control, ..., extended = extended)
                        Clust$promediarResults(max_n_clust,ind, mask, agg, crop, int,
                                               serie, pkg, diss, model)
                      }),
                      stop("pkg no vÃ¡lido: ",pkg)
                    )
                  }
                })
              })
            })
          })
        })
      })
    })
  }) %>% invisible()
}

#' @export
#' @rdname clust
#' #' se encarga de devolver los datos de calcular los datos por medio de clustering, llama a initResultsClusts
Clust$leerResultsClust <- function(max_n_clust,ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                                   crop = dflt$crop, int = dflt$int,
                                   serie = dflt$serie, pkg = dflt$pkg,
                                   diss = if (pkg == "TSclust") dflt$diss,
                                   model = dflt$model) {

  fichero <- paths$res(int, crop, list(ind, mask, agg), model, pkg, serie, diss)
  if (!file.exists(fichero))
    Clust$initResultsClusts(max_n_clust,ind, mask, agg, crop, int, serie, pkg, diss, model)

  res <- data.table::fread(fichero, sep = ";")
  attr(res, "param") <- list(ind = ind, mask = mask, agg = agg,
                             crop = crop, int = int, model = model,
                             serie = serie, pkg = pkg)
  if (pkg == "TSclust")
    attr(res, "param")$diss <- diss
  return(res)
}

#' @export
#' @rdname clust
#' #' devuelve los datos del clustering, llama a leerResultsClust
Clust$readResultsClusts <- function(max_n_clust,ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                                    crop = dflt$crop, int = dflt$int,
                                    series = dflt$series, pkgs = dflt$pkgs,
                                    disss = dflt$disss[-c(26,27,33)],
                                    models = dflt$models[5:6]) {

  res <- listDF2DF(sapply(models, function(model) {
    listDF2DF(sapply(series, function(serie) {
      listDF2DF(sapply(pkgs, function(pkg) {
        switch (pkg,
          "pdc" = {
            Clust$leerResultsClust(max_n_clust,ind, mask, agg, crop, int,
                                 serie, pkg, NULL, model) %>%
              dplyr::mutate(Diss = NA_character_) %>%
              dplyr::relocate(Diss)
          },
          "TSclust" = {
            listDF2DF(sapply(disss, function(diss) {
              Clust$leerResultsClust(ind, mask, agg, crop, int,
                                   serie, pkg, diss, model)
            }, simplify = F), id = "Diss")
          },
          stop("pkg no vÃ¡lido: ",pkg)
        )
      }, simplify = F), id = "Pkg")
    }, simplify = F), id = "Serie")
  }, simplify = F), id = "Model")
  attr(res, "param") <- list(ind = ind, mask = mask, agg = agg,
                             crop = crop, int = int)
  return(res)
}

