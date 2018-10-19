require(randomForest)
require(caret)
require(dplyr)
require(ggplot2)

attach(mtcars)

set.seed(102)

dim(mtcars)

dadosTreino <- sample(1:nrow(mtcars), 25)

carrosRF <- randomForest(carb ~ . , data = mtcars, subset = dadosTreino, ntree = 600)

plot(carrosRF)


treino.err = double(11)
teste.err = double(11)

for (aux in 1:11) {
  rf = randomForest(carb ~ . , data = mtcars, subset = dadosTreino, aux = aux, ntree = 400)
  treino.err[aux] = rf$mse[400]
  
  pred <- predict(rf, mtcars[-dadosTreino])
  teste.err[aux] = with(mtcars[-dadosTreino,], mean((carb - pred)^2))
  
  cat(aux, "")
}

matplot(1:aux , cbind(treino.err, teste.err), pch = 19, col = c("red", "blue"), type = "b", ylab = "Mean Squared Error", xlab = "Number of Predictors Considered at each Split")
