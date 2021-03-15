
out.abcrf <- function(x, training, obs = NULL, n.var = 20,
                         predict_ntree = 100, 
                         return_predict = TRUE, ...) {
  
  if (!inherits(obs, "data.frame") && !is.null(obs)) {
    stop("obs needs to be a data.frame object or NULL")
  }
  if (!inherits(training, "data.frame")) {
    stop("training needs to be a data.frame object")
  }

  if (length(x$group) != 0) {
    ngroup <- length(x$group)
    varn <- x$formula[[2]]
    training[[as.character(varn)]] <- as.vector(training[[as.character(varn)]])
    allmod <- unique(training[[as.character(varn)]])
    for (k in 1:ngroup) {
      for (l in 1:length(x$group[[k]])) {
        training[[as.character(varn)]][which(training[[as.character(varn)]] == x$group[[k]][l])] <- paste("g", k, sep = "")
      }
    }
    if (!setequal(allmod, unlist(x$group))) {
      diffe <- setdiff(allmod, unlist(x$group))
      for (l in 1:length(diffe)) training <- training[-which(training[[as.character(varn)]] == diffe[l]), ]
    }
    training[[as.character(varn)]] <- as.factor(training[[as.character(varn)]])
  }

  if (length(x$model.rf$variable.importance) < 20) {
    n.var <- length(x$model.rf$variable.importance)
  }

  mf <- match.call(expand.dots = FALSE)
  mf <- mf[1]
  mf$formula <- x$formula
  mf$data <- training
  mf[[1L]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  modindex <- model.response(mf)
  
  if (x$lda) {
    
    nmod <- length(x$model.rf$forest$levels)
    nstat <- x$model.rf$num.independent.variables
    projections <- predict(x$model.lda, training)$x
    predicted <- data.table(projections, modindex, type = "predicted")
    
    if (!is.null(obs)) {
      projobs <- predict(x$model.lda, obs)$x
      
      preds <- predict(x, obs, training, ntree = predict_ntree)
      observed <- data.table::data.table(projobs, 
                             modindex =  preds$allocation,
                             type = "observed")
  
    } else {
      observed <-NULL
    } 
    
    lda <- rbind(predicted, observed)
    
    if(!is.null(obs) & return_predict) {
      return(list(predicted = preds, lda = lda))
    } else {
      return(lda)
    }
    
  } else {
    stop("No lda values!")
  }
  
}

out <-
  function(...) UseMethod("out")
