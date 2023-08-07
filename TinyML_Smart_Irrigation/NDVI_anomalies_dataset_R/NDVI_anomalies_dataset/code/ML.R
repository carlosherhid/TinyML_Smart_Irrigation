if (!exists("paths") || is.null(paths)) {
  wd <- if (Sys.info()["sysname"] == "Windows") "." else
    file.path("","home",Sys.info()["effective_user"],"RiegoInteligente")
  source(file.path(wd,"code","include.R"), encoding = "UTF-8")
}



#' Machine learning con caret
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
#' @param model(s)
#'   (Vector de) modelo(s) de machine learning para entrenar.
#'   Posibles valores: `r dflt$models`.
#' @param ini,fin
#' @name ml
ML <- list(
  control = caret::trainControl(## 10-fold CV
                                method = "repeatedcv",
                                number = 10,
                                ## repeated ten times
                                repeats = 10),
  controlLSTM = caret::trainControl(method = "timeslice",
                                    initialWindow = 30,
                                    horizon = 1,
                                    fixedWindow = T,
                                    skip = 0)
)

vect2filDF <- function(v) {
  data.table::data.table(
    matrix(v, ncol = length(v),
           dimnames = list(NULL, names(v)))
  )
}

redim <- function(m) {
  m %<>% as.matrix()
  dim(m) <- c(nrow(m),1L,ncol(m))
  m
}

ML$particionarDatos <- function(ds, p = 0.75, random = T) {
  ds <- ds[,-1L] # delete column Date
  ds <- ds[complete.cases(ds), ] # remove rows with NAs
  #NORMALMENTE,  la variable a predecir se encuentra la última
  nc <- ncol(ds)
  if (length(unique(ds[[nc]])) < 2L) {
    return(list(train = ds %>% dplyr::slice_sample(n = 0L),
                test = ds))
  }
  train_idx <- if (random) {
    set.seed(1234)
    caret::createDataPartition(ds[[nc]], p = p, list = F)
  } else {
    1L:round(p*nrow(df),0L)
  }
  list(train = ds[train_idx,],
       test = ds[-train_idx,])
}

ML$preprocesarDatos <- function(ds, pca = 0L, b = NULL) {
  nc <- ncol(ds$train) # == ncol(ds$test)
  #Now using preProcess, you need to set the pcaComp = 7, or thresh = 0.8
  #you may need to center and scale first and then apply PCA or just use method = c("pca")
  #create the preProc object, remember to exclude the response (medv)
  method <- c("zv", if (is.null(b)) "center", if (is.null(b)) "scale",
              if (!is.null(b)) "range", if (pca != 0L) "pca")
  preProc <- caret::preProcess(ds$train[,1L:(nc-1L)], method,
                               rangeBounds = b, pcaComp = if (pca != 0L) pca) # or thresh = 0.8
  #Apply the processing to the train and test data, and add the response to the dataframes
  list(train = list(X = predict(preProc, ds$train[,1L:(nc-1L)]),
                    Y = ds$train[[nc]]),
       test  = list(X = predict(preProc, ds$test[,1L:(nc-1L)]),
                    Y = ds$test[[nc]]))
}

ML$entrenarModelo <- function(train, model = dflt$model, control = ML$control, ...) {
  set.seed(825)
  caret::train(train$X, train$Y, if (model != "lstm") model else lstm,
               trControl = if (model != "lstm") control else ML$controlLSTM, ...,
               ## This last option is actually one
               ## for gbm() that passes through
               verbose = F)
}

ML$testearModelo <- function(trainORtest, modelFit = NULL, prefix = "", extended = F) {
  real <- trainORtest$Y
  results <- if (!extended) c() else c(N_Obs = length(real),
                                       Min = min(real, na.rm = T),
                                       Max = max(real, na.rm = T),
                                       Mean = mean(real, na.rm = T))
  if (!is.null(modelFit)) {
    predictions <- predict(modelFit, newdata = trainORtest$X)
    results <- c(results,
                 # ModelMetrics, DescTools, modelr
                 MAE = DescTools::MAE(predictions, real),
                 MAPE = DescTools::MAPE(predictions, real),
                 MSE = DescTools::MSE(predictions, real),
                 RMSE = DescTools::RMSE(predictions, real),
                 CV = mean(real), ###############################################
                 CVRMSE = DescTools::RMSE(predictions, real) / mean(real))
  } else {
    results <- c(results,
                 MAE = NA, MAPE = NA,
                 MSE = NA, RMSE = NA, CV = NA, CVRMSE = NA)
  }
  names(results) <- paste0(prefix,names(results))
  vect2filDF(results)
}

#' @rdname ml
ML$probarModelo <- function(contratos = NULL, ind = dflt$ind,
                            mask = dflt$mask, agg = dflt$agg,
                            crop = dflt$crop, int = dflt$int,
                            model = dflt$model, byPCA = 1L,
                            control = ML$control, ..., extended = F) {

  cl <- parallel::makePSOCKcluster(5L)
  doParallel::registerDoParallel(cl)
  ## All subsequent models are then run in parallel

  if (is.null(contratos))
    contratos <- dflt$contratos("conDatInt", ind, mask, agg, crop, int)
  else
    contratos %<>% intersect(dflt$contratos("conDatInt", ind, mask, agg, crop, int))
  group <- paste0(contratos, collapse = "_")
  n <- length(contratos)

  # lista de data frames, uno por contrato
  ds <- DatInt$readDatInt(contratos, ind, mask, agg, crop, int)
  # lista de parejas de data frames, train y test, una pareja por contrato
  ds <- lapply(ds, ML$particionarDatos, random = model != "lstm")
  # lista de dos listas, una de data frames train y otra de data frames test
  ds <- list(train = lapply(ds, getElement, "train"),
             test  = lapply(ds, getElement,  "test"))
  # lista de dos data frames, train y test
  ds$train %<>% listDF2DF() %>% dplyr::select(!Contrato)
  ds$test  %<>% listDF2DF() %>% dplyr::select(!Contrato)

  pcas = if (byPCA == 0L) 0L else seq(0L, ncol(ds$train)-1L, byPCA)
  lapply(pcas, function(pca) {
    time <- summary( system.time( tryCatch(
      {
        dsp <- ML$preprocesarDatos(ds, pca, b = if (model == "lstm") c(-1,1)) #c(0,1)
        if (model == "lstm")
          # dsp <- lapply(dsp, function(trainORtest) lapply(trainORtest, function(XorY) redim(XorY)))
          # dsp <- lapply(dsp, function(trainORtest) lapply(trainORtest, redim))
          dsp <- lapply(dsp, lapply, redim)
        modelFit <- ML$entrenarModelo(dsp$train, model, control, ...)
        if (extended) {
          tune <- modelFit$bestTune
          trainResult <- ML$testearModelo(dsp$train, modelFit, "Train.")
        }
        testResult <- ML$testearModelo(dsp$test, modelFit, "")
      },
      error = function(cond) {
        msg("Hubo en error en la ejecucion para ",group," ",pca," ",model)
        msg("Here's the original error message:")
        msg(cond)
        msg("")
        if (extended) {
          param <- if (model == "lstm")
            lstm$parameters$parameter else
              caret::modelLookup(model)$parameter
          tune <<- rep(NA, times = length(param))
          names(tune) <<- param
          tune <<- vect2filDF(tune)
          trainResult <<- ML$testearModelo(dsp$train, NULL, "Train.")
        }
        testResult <<- ML$testearModelo(dsp$test, NULL, "")
      }
    ) ) )
    time %<>% vect2filDF()

    results <- data.table::data.table(Group = group, N = n, PCA = pca,
                                      if (extended) tune,
                                      if (extended) trainResult,
                                      testResult, time)
    fichero <- paths$res(int, crop, list(ind, mask, agg), model, NA)
    data.table::fwrite(results, fichero, append = T, sep = ";", na = "NA")
  })

  ## When you are done:
  parallel::stopCluster(cl)
}

#' @rdname ml
ML$leerResults <- function(ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                           crop = dflt$crop, int = dflt$int,
                           model = dflt$model, pca = NULL) {

  fichero <- paths$res(int, crop, list(ind, mask, agg), model, NA)
  if (!file.exists(fichero))
    stop("File not exists! ", fichero)

  res <- data.table::fread(fichero, sep = ";")
  if (!is.null(pca)) res %<>% dplyr::filter(PCA %in% pca)
  attr(res, "param") <- list(ind = ind, mask = mask, agg = agg,
                             crop = crop, int = int, model = model)
  return(res)
}

#' @rdname ml
ML$promediarResults <- function(ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                                crop = dflt$crop, int = dflt$int,
                                model = dflt$model) {

  results <- ML$leerResults(ind, mask, agg, crop, int, model, NULL)# %>%
    # dplyr::select(!Clust)
  contratos <- dflt$contratos("ConSeries", ind, mask, agg)
  todos <- results %>% dplyr::filter(Group == paste0(contratos, collapse = "_")) %>%
    dplyr::select(!c(Group,N))
  cada1 <- results %>% dplyr::filter(Group %in% contratos) %>%
    dplyr::select(!c(Group,N))

  todos %<>% dplyr::mutate(n = 1, .before = "PCA")
  cada1 <- purrr::reduce(list(
    cada1 %>% dplyr::group_by(PCA) %>%
      dplyr::summarise(n = numNoNA(CVRMSE)) %>%
      dplyr::ungroup(),
    cada1 %>% dplyr::group_by(PCA) %>%
      dplyr::select(!c(user,system,elapsed)) %>%
      dplyr::summarise_if(is.numeric, mean, na.rm = T) %>%
      dplyr::ungroup(),
    cada1 %>% dplyr::group_by(PCA) %>%
      dplyr::select(c(PCA,user,system,elapsed)) %>%
      dplyr::summarise_if(is.numeric, sum) %>%
      dplyr::ungroup()
  ), merge)
  cada1 %<>% dplyr::relocate(n)

  final.results <- list(todos, cada1)
  final.results %<>% dplyr::bind_rows()
  fichero <- paths$res(int, crop, list(ind, mask, agg), model, "None")
  data.table::fwrite(final.results, fichero, sep = ";", na = "NA")
}

#' @export
#' @rdname ml
ML$initResultsNone <- function(inds = dflt$ind, masks = unique(c(NA,dflt$mask)),
                               aggs = dflt$agg, crops = dflt$crop, ints = dflt$ints,
                               models = dflt$models[5:6], byPCA = 1L,
                               control = ML$control, ..., extended = F) {

  lapply(inds, function(ind) {
    lapply(masks, function(mask) {
      lapply(aggs, function(agg) {
        contratos <- dflt$contratos("ConSeries", ind, mask, agg)
        # msg("Inicializando resultados de ",length(contratos)," contratos...")
        lapply(crops, function(crop) {
          lapply(ints, function(int) {
            msg("Inicializando resultados de contratos con ",
                paste(int,crop,ind,mask,agg, sep = "_"),"...")
            lapply(models, function(model) {
              msg("#######     ",paste(model,"all", sep = "_"),"     #######")
              ML$probarModelo(contratos, ind, mask, agg, crop, int,
                              model, byPCA, control, ..., extended = extended)
              msg("#######     ",paste(model,"each1", sep = "_"),"     #######")
              lapply(contratos, function(contrato) {
                ML$probarModelo(contrato, ind, mask, agg, crop, int,
                                model, byPCA, control, ..., extended = extended)
              })
              msg("#######     ",paste(model, sep = "_"),"     #######")
              ML$promediarResults(ind, mask, agg, crop, int, model)
            })
          })
        })
      })
    })
  }) %>% invisible()
}

#' @export
#' @rdname ml
ML$leerResultsNone <- function(ind = dflt$ind, mask = dflt$mask, agg = dflt$agg,
                               crop = dflt$crop, int = dflt$int,
                               model = dflt$model, pca = NULL) {

  fichero <- paths$res(int, crop, list(ind, mask, agg), model, "None")
  if (!file.exists(fichero)) {
    byPCA <- if (!is.null(pca) && pca == 0L) 0L else 1L
    ML$initResultsNone(ind, mask, agg, crop, int, model, byPCA)
  }

  res <- data.table::fread(fichero, sep = ";")
  if (!is.null(pca)) res %<>% dplyr::filter(PCA %in% pca)
  attr(res, "param") <- list(ind = ind, mask = mask, agg = agg,
                             crop = crop, int = int, model = model, pkg = "None")
  return(res)
}

#' @export
#' @rdname ml
ML$readResultsNone <- function(inds = dflt$ind, masks = unique(c(NA,dflt$mask)),
                               aggs = dflt$agg, crops = dflt$crop, ints = dflt$ints,
                               models = dflt$models[5:6], pca = NULL) {

  listDF2DF(sapply(inds, function(ind) {
    listDF2DF(sapply(masks, function(mask) {
      listDF2DF(sapply(aggs, function(agg) {
        listDF2DF(sapply(crops, function(crop) {
          listDF2DF(sapply(ints, function(int) {
            listDF2DF(sapply(models, function(model) {
              ML$leerResultsNone(ind, mask, agg, crop, int, model, pca)
            }, simplify = F), id = "Model")
          }, simplify = F), id = "Int")
        }, simplify = F), id = "Crop")
      }, simplify = F), id = "Agg")
    }, simplify = F), id = "Mask")
  }, simplify = F), id = "Ind") %>%
    # dplyr::mutate(Pkg = "None", .before = n) %>%
    dplyr::relocate(Model, .before = PCA)
}



