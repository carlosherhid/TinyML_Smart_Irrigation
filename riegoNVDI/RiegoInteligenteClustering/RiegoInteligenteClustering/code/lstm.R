# resamples, summary, compare_models


lstm <- list(
  # is an optional character string that names the model
  label = "Long-Short Term Memory",
  # is an optional character vector that has subjects associated with the model
  tags = c("Time Series Forecasting", "Recurrent Neuronal Networks", "Neural Network", "L2 Regularization"),
  # is an optional character vector that can be used to document non-obvious aspects of the model
  notes = "After `train` completes, the keras model object is serialized so that it can be used between R session. When predicting, the code will temporarily unsearalize the object. To make the predictions more efficient, the user might want to use  `keras::unsearlize_model(object$finalModel$object)` in the current R session so that that operation is only done once. Also, this model cannot be run in parallel due to the nature of how tensorflow does the computations. Finally, the cost parameter weights the first class in the outcome vector. Unlike other packages used by `train`, the `dplyr` package is fully loaded when this model is used.",
  # is a simple character vector with values "Classification", "Regression" or both
  type = "Regression",#"Classification",
  # is a character vector of package names that will be needed to fit the model or calculate predictions
  library = c("keras", "tensorflow", "ModelMetrics"),
  # is an optional function that can be used to check the system/install to make sure that any atypical software requirements are available to the user
  check = function(pkg) {
    testmod <- try(keras::keras_model_sequential(),
                   silent = TRUE)
    if (inherits(testmod, "try-error"))
      stop("Could not start a sequential model. ",
           "`tensorflow` might not be installed. ",
           "See `?install_tensorflow`.",
           call. = FALSE)
    TRUE
  },
  # is an optional function for advanced users for models that can create multiple submodel predictions from the same object
  loop = NULL,



  # is a data frame with three simple attributes for each tuning parameter (if any): the argument name, the type of data in the parameter grid and textual labels for the parameter.
  parameters = data.frame(
    parameter = c("size","activation",#"dropout","lambda",
                  "lr","rho","decay",#"optimizer",#"cost",
                  "batch_size","epochs"),
    class = c("numeric","character",#"numeric","numeric",
              "numeric","numeric","numeric",#"character",#"numeric",
              "numeric","numeric"),
    label =  c("#Hidden Units","Activation Function",#"Dropout Rate","L2 Regularization",
               "Learning Rate","Rho","Learning Rate Decay",#"Optimizer (sgd, rmsprop, adam,...),#"Cost",
               "Batch Size", "#Epochs")
  ),



  # is a function that is used to create the tuning grid (unless the user gives the exact values of the parameters via tuneGrid)
  grid = function(x, y, len = NULL, search = "grid") {
    act_funs <- c("sigmoid", "relu", "tanh")
    opt_funs <- c("sgd", "rmsprop", "adam", "nadam", "adamax", "adagrad", "adadelta")
    if (search == "grid") {
      out <- expand.grid(
        size = ((1:len) * 2) - 1,
        activation = "relu",
        # dropout = seq(0, .7, length = len),
        # lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)),
        lr = 2e-6,
        rho = .9,
        decay = 0,
        ## cost = 1:len,
        # optimizer = "adam",
        epochs = 1000,
        batch_size = floor(nrow(x)/3)
      )
    } else if ("mygrid") {
      n <- 1L
      out <- data.frame(
        size = c(200L,100L,50L,1L),
        activation = act_funs[c(2L,2L,3L,3L)],
        # dropout = seq(0, .7, length = len),
        # lambda = c(0, 10 ^ seq(-1, -4, length = len - 1)),
        lr = 0.001,
        rho = .9,
        decay = 0,
        # optimizer = opt_funs[c(3L,3L,2L,3L)],
        epochs = c(1000L,300L,20L,50L),
        batch_size = c(32L,32L,32L,1L)#floor(dim(train$X)[1]/3)
      )
    } else {
      n <- nrow(x)
      out <- data.frame(
        size = sample(2:20, replace = TRUE, size = len),
        activation = sample(act_funs, size = len, replace = TRUE),
        # dropout = runif(len, max = .7),
        # lambda = 10^runif(len, min = -5, 1),
        lr = runif(len),
        rho = runif(len),
        decay = 10^runif(len, min = -5, 0),
        ## cost = runif(len, min = 1, max = 20),
        # optimizer = sample(opt_funs, size = len, replace = TRUE),
        epochs = 1000,
        batch_size = floor(n*runif(len, min = .1))
      )
    }
    out
  },


  # is a function that sorts the parameter from most complex to least
  # sort = function(x) { x[order(x$size, -x$dropout),] },
  # sort = function(x) { x[order(x$size, -x$lambda),] },
  sort = function(x) { x[order(x$size, -x$decay), ] },


  # is a function that fits the model
  fit = function(x, y, wts, param, lev, last, classProbs, ...) {
    require(dplyr)
    K <- keras::backend()
    K$clear_session()
    if (!is.matrix(x)) x <- as.matrix(x)
    model <- keras::keras_model_sequential()
    # 1 capa oculta
    model %>% keras::layer_lstm(
      input_shape = dim(x)[2L:3L],
      units = param$size,
      activation = as.character(param$activation),
      return_sequences = TRUE#, stateful= TRUE
      # kernel_initializer = keras::initializer_glorot_uniform(),
      # kernel_regularizer = keras::regularizer_l2(param$lambda)
    )
    # 2 capas ocultas
    model %>% keras::layer_lstm(
      units = param$size,
      activation = as.character(param$activation)
      # return_sequences = TRUE#, stateful= TRUE
      # kernel_initializer = keras::initializer_glorot_uniform(),
      # kernel_regularizer = keras::regularizer_l2(param$lambda)
    )
    # model %>% keras::layer_dropout(rate = param$dropout,
    #                                seed = sample.int(1000, 1))
    # if (is.factor(y)) {
    #   y <- class2ind(y)
    #   # capa de salida
    #   model %>% keras::layer_dense(
    #     units = prod(dim(y)[2L:3L])*length(lev),
    #     activation = 'softmax' # 'tanh' ????????
    #     # kernel_regularizer = keras::regularizer_l2(param$lambda)
    #   )
    #   # summary(model)
    #   model %>% keras::compile(
    #     loss = "categorical_crossentropy",
    #     # loss_weights = list(param$cost),
    #     optimizer = keras::optimizer_adam( # rmsprop
    #       learning_rate = param$lr,
    #       # rho = param$rho,
    #       decay = param$decay
    #     )
    #     # metrics = "accuracy"
    #   )
    # } else {
      # capa de salida
      model %>% keras::layer_dense(
        units = prod(dim(y)[2L:3L])*1L,
        activation = 'linear' # 'tanh' ????????
        # kernel_regularizer = keras::regularizer_l2(param$lambda)
      )
      # summary(model)
      model %>% keras::compile(
        loss = "mean_squared_error",
        optimizer = keras::optimizer_adam( # rmsprop
          learning_rate = param$lr,
          # rho = param$rho,
          decay = param$decay
        )
        # metrics = "mean_squared_error"
      )
    # }
    history <- model %>% keras::fit(
      x = x, y = y,
      batch_size = param$batch_size,
      # epochs = param$epochs,
      # verbose = 0,
      # validation_split = 0.2,
      ...
    )
    if (last)
      model <- keras::serialize_model(model)
    list(object = model, history = history)
  },


  # is the function that creates predictions
  predict = function(modelFit, newdata, submodels = NULL) {
    if (inherits(modelFit$object, "raw"))
      modelFit$object <- keras::unserialize_model(modelFit$object)
    if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
    out <- predict(modelFit$object, newdata)
    ## check for model type
    if (ncol(out) == 1) {
      out <- out[, 1]
    } else {
      out <- modelFit$obsLevels[apply(out, 1, which.max)]
    }
    out
  },



  # is a function that can be used to create class probabilities (if applicable)
  # prob = function(modelFit, newdata, submodels = NULL) {
  #   if(inherits(modelFit$object, "raw"))
  #     modelFit$object <- keras::unserialize_model(modelFit$object)
  #   if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
  #   out <- predict(modelFit$object, newdata)
  #   colnames(out) <- modelFit$obsLevels
  #   as.data.frame(out, stringsAsFactors = TRUE)
  # },
  # If a regression model is being used ... a value of NULL can be used here instead of a function
  prob = NULL,
  # is an optional function, primarily for classification models using S4 methods to return the factor levels of the outcome
  # levels = function(x) {},
  levels = NULL,
  # is an optional function that returns a character vector that contains the names of the predictors that we used in the prediction equation
  # predictors = function(x, ...) {},
  # is an optional function that calculates variable importance metrics for the model (if any)
  varImp = NULL
  # is another optional function that calculates out-of-bag performance estimates from the model object
  # oob = function(x),
)
