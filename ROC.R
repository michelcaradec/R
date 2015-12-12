# ROC Helpers ----
rocContingency <- function(guess, truth, threshold = .5)
{
  # Missing factor levels will also appear in the table
  values.guess <- factor(guess >= threshold, levels = c(F, T))
  values.truth <- factor(truth, levels = c(F, T))
  
  tbl <- table(Real = values.truth, Predicted = values.guess, dnn = c("real", "predicted"))
  tp <- tbl[2, 2]
  tn <- tbl[1, 1]
  fp <- tbl[1, 2]
  fn <- tbl[2, 1]
  
  p <- tp + fn
  n <- fp + tn
  
  fp.rate <- fp / n
  tp.rate <- tp / p # = 1 - (fn / p)
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))
  recall <- tp / p
  accuracy <- (tp + tn) / (p + n)
  f.score <- 2 / (1 / precision + 1 / recall)
  
  return(
    list(
      threshold = threshold,
      tbl = tbl,
      fp.rate = fp.rate,
      tp.rate = tp.rate,
      precision = precision,
      recall = recall,
      accuracy = accuracy,
      f.score = f.score
    )
  )
}

rocContingencyPrint <- function(ctg)
{
  print("--------")
  print(paste0("threshold = ", round(ctg$threshold, 2)))
  print(ctg$tbl)
  print(
    paste0(
      "precision = ", round(ctg$precision, 2),
      " recall = ", round(ctg$recall, 2),
      " accuracy = ", round(ctg$accuracy, 2),
      " f-score = ", round(ctg$f.score, 2)
    )
  )
}

rocBuild <- function(guess, truth, steps = 10, verbose = F)
{
  if (!is.logical(truth)) { stop("truth argument must be logical") }
  
  fp.rates <- NULL
  tp.rates <- NULL
  precisions <- NULL
  recalls <- NULL
  accuracies <- NULL
  f.scores <- NULL
  
  # Test model with various thresholds
  guess <- unclass(guess) # In case of a factor
  thresholds <- seq(min(guess), max(guess), length.out = steps)
  for (threshold in thresholds)
  {
    ctg <- rocContingency(guess, truth, threshold)

    fp.rates <- c(fp.rates, ctg$fp.rate)
    tp.rates <- c(tp.rates, ctg$tp.rate)
    precisions <- c(precisions, ctg$precision)
    recalls <- c(recalls, ctg$recall)
    accuracies <- c(accuracies, ctg$accuracy)
    f.scores <- c(f.scores, ctg$f.score)
    
    if (verbose) rocContingencyPrint(ctg)
  }
  
  return(
    list(
      metrics = data.frame(
        fp.rate = fp.rates,
        tp.rate = tp.rates,
        precision = precisions,
        recall = recalls,
        accuracy = accuracies,
        f.score = f.scores,
        threshold = thresholds
      ),
      ctg05 = rocContingency(guess, truth)
    )
  )
}

# Drawing
rocPlot <- function(fp.rates, tp.rates, thresholds, title = "ROC Curve")
{
  plot(
    fp.rates, tp.rates,
    xlim = c(0, 1),
    ylim = c(0, 1),
    pch = 20,
    cex = 1,
    cex.axis = .8,
    cex.lab = .8,
    col = rgb(0, 0, 0, .5),
    xlab = "False positive rate",
    ylab = "True positive rate",
    main = title
  )
  text(
    jitter(fp.rates),
    jitter(tp.rates),
    format(thresholds, digits = 2),
    pos = 3,
    cex = .7,
    col = rgb(0, 0, 0, .5)
  )
  #lines(spline(fp.rate, tp.rate, 100), col = "red")
  lines(fp.rates, tp.rates, col = "red")
  abline(0, 1, lt = "dashed")
}

# 3D Drawing
rocPlot3D <- function(fp.rate, tp.rate, threshold)
{
  require(scatterplot3d)
  
  scatterplot3d(
    fp.rate, tp.rate, threshold,
    #color = rgb(0, 0, 255, alpha = 255 / (nrow(roc):1), maxColorValue = 255),
    highlight.3d = T,
    pch = 20,
    xlab = "False positive rate",
    ylab = "True positive rate",
    zlab = "Threshold",
    main = "ROC Curve"
  )
}

# 3D Dynamic Drawing
rocPlot3DDynamic <- function(fp.rate, tp.rate, threshold)
{
  require(rgl)
  plot3d(
    fp.rate, tp.rate, threshold,
    #col = ,
    size = 10,
    xlab = "False positive rate",
    ylab = "True positive rate",
    zlab = "Threshold",
    main = "ROC Curve"
  )
}

# Demonstration code ----
# (won't be executed)
if (F)
{
  rocDemo <- function(steps = 50)
  {
    # Dataset
    data <- iris
    data$isSetosa <- data$Species == levels(data$Species)[1]
    data$isVersicolor <- data$Species == levels(data$Species)[2]
    data$isVirginica <- data$Species == levels(data$Species)[3]
    
    train.size <- nrow(data) * 0.8
    train.indices <- sample(1:nrow(data), train.size, replace = F)
    train <- data[train.indices, ]
    test <- data[-train.indices, ]
    
    # Model
    model <-
      glm(
        formula = isVersicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
        family =  binomial,
        data = train
      )
    guess <- predict(model, test[, 1:4], type = "response")
    
    # ROC evaluation
    model.roc <- rocBuild(guess, test$isVersicolor, steps)
    rocPlot(
      model.roc$metrics$fp.rate,
      model.roc$metrics$tp.rate,
      model.roc$metrics$threshold,
      title = paste0("F-Score = ", round(model.roc$ctg05$f.score, 2))
    )
    #rocPlot3D(model.roc$metrics$fp.rate, model.roc$metrics$tp.rate, model.roc$metrics$threshold)
    #rocPlot3DDynamic(model.roc$metrics$fp.rate, model.roc$metrics$tp.rate, model.roc$metrics$threshold)
  }
}
