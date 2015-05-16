# ROC

# ----
# ROC Helpers
rocBuild <- function(guess, truth, step = .1)
{
  x <- NULL
  y <- NULL
  # Test model with various thresholds
  thresholds <- seq(min(guess), max(guess), step)
  for (threshold in thresholds)
  {
    # Missing factor levels will also appear in the table
    values.guess <- factor(guess >= threshold, levels = c(F, T))
    values.truth <- factor(truth, levels = c(F, T))
    tbl <- table(Predicted = values.guess, Observed = values.truth)
    tp <- tbl[2, 2]
    tn <- tbl[1, 1]
    fp <- tbl[2, 1]
    fn <- tbl[1, 2]
    
    p <- tp + fn
    n <- fp + tn
    
    fp.rate <- fp / n
    tp.rate <- tp / p
    precision <- tp / (tp + fp)
    recall <- tp / p
    accuracy <- (tp + tn) / (p + n)
    f.score <- 2 / (1 / precision + 1 / recall)
    
    x <- c(x, fp.rate)
    y <- c(y, tp.rate)
  }
  
  return(data.frame(fp.rate = x, tp.rate = y, threshold = thresholds))
}

# Drawing
rocPlot <- function(fp.rate, tp.rate, threshold)
{
  plot(
    fp.rate, tp.rate,
    xlab = "False positive rate",
    ylab = "True positive rate",
    main = "ROC Curve"
  )
  text(
    jitter(fp.rate),
    jitter(tp.rate),
    format(threshold, digits = 2),
    pos = 3,
    cex = .7
  )
  lines(spline(fp.rate, tp.rate, 100), col = "red")
  abline(0, 1, lt = "dashed")
}

# 3D Drawing
library(scatterplot3d)

rocPlot3D <- function(fp.rate, tp.rate, threshold)
{
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
library(rgl)

rocPlot3DDynamic <- function(fp.rate, tp.rate, threshold)
{
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

# ----
# Dataset
data <- iris
data$isSetosa <- data$Species == levels(data$Species)[1]
data$isVersicolor <- data$Species == levels(data$Species)[2]
data$isVirginica <- data$Species == levels(data$Species)[3]

train.size <- nrow(data) * 0.8
train.indices <- sample(1:nrow(data), train.size, replace = F)
train <- data[train.indices, ]
test <- data[-train.indices, ]

# ----
# Model
model <-
  glm(
    formula = isVersicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
    family =  binomial,
    data = train
  )
guess <- predict(model, test[, 1:4], type = "response")

# ----
# ROC evaluation
roc <- rocBuild(guess, test$isVersicolor)
rocPlot(roc$fp.rate, roc$tp.rate, roc$threshold)
rocPlot3D(roc$fp.rate, roc$tp.rate, roc$threshold)
rocPlot3DDynamic(roc$fp.rate, roc$tp.rate, roc$threshold)
