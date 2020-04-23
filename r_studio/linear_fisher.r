## Оценка ковариационной матрицы для ЛДФ
estimateFisherCovarianceMatrix <- function(objects1,
                                           objects2, mu1, mu2)
{
  rows1 <- dim(objects1)[1]
  rows2 <- dim(objects2)[1]
  rows <- rows1 + rows2
  cols <- dim(objects1)[2]
  print(objects1)
  print(objects2)
  sigma <- matrix(0, cols, cols)
  
  for (i in 1:rows1)
  {
    sigma <- sigma + (t(objects1[i,] - mu1) %*%
                        (objects1[i,] - mu1)) / (rows + 2)
  }
  
  for (i in 1:rows2)
  {
    sigma <- sigma + (t(objects2[i,] - mu2) %*%
                        (objects2[i,] - mu2)) / (rows + 2)
  }
  
  return (sigma)
}

estimateMu <- function(objects)
{
  ## mu = 1 / m * sum_{i=1}^m(objects_i)
  rows <- dim(objects)[1]
  cols <- dim(objects)[2]
  
  mu <- matrix(NA, 1, cols)
  
  for (col in 1:cols)
  {
    mu[1, col] = mean(objects[,col])
  }
  
  return(mu)
}
## Количество объектов в каждом классе
ObjectsCountOfEachClass <- 200
## Подключаем библиотеку MASS для генерации многомерного нормального распределения
library(MASS)
## Генерируем тестовые данные
Sigma1 <- matrix(c(3, 0, 0, 3), 2, 2)
Sigma2 <- matrix(c(3, 0, 0, 3), 2, 2)
Mu1 <- c(2, 10)
Mu2 <- c(8, 2)
xy1 <- mvrnorm(n=ObjectsCountOfEachClass, Mu1, Sigma1)
xy2 <- mvrnorm(n=ObjectsCountOfEachClass, Mu2, Sigma2)
## Собираем два класса в одну выборку
xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
## Рисуем обучающую выборку
colors <- c('green', 'yellow')
plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp =
       1)
## Оценивание
objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
mu1 <- estimateMu(objectsOfFirstClass)
mu2 <- estimateMu(objectsOfSecondClass)
Sigma <-
  estimateFisherCovarianceMatrix(objectsOfFirstClass,
                                 objectsOfSecondClass, mu1, mu2)
## Получаем коэффициенты ЛДФ
inverseSigma <- solve(Sigma)
alpha <- inverseSigma %*% t(mu1 - mu2)
mu_st <- (mu1 + mu2) / 2
beta <- mu_st %*% alpha



## Рисуем ЛДФ
abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col =
         "red", lwd = 3)