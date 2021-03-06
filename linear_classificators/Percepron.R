## ���������� �����������
trainPerceptron <- function(dm, dc) {
  ## � result ����� ������� ������ ����� � ����������� �� ��������, 
  ## ����� ����� ����� ���� ����������, ��� ����������� �������
  result <- list()
  oldW <- c()
  w <- rep(0, ncol(dm))
  while (!identical(w, oldW)) {
    oldW <- w
    for (i in 1:nrow(dm)) {
      ## %*% ����� �������� ��������� ������������
      pred <- sign(dm[i,] %*% w)
      w = w + (dc[i] - pred) * dm[i, ]
    }
    ## ��������� w � ����� ������
    result <- c(result, list(w))
  }
  result
}

## ������������ ���������� �������� pcResult � ������ ������� ������� steps
plotPerceptronSteps <- function(pcResult, dm, dc, steps) {
  for (s in steps) {
    w <- pcResult[[s]]
    print("ss")
    print(w)
    plot(dm[,1:2], main=paste("step", s), 
         pch=ifelse(dc > 0, 1, 2), sub=paste(w, collapse=' '))
    abline(-w[3] / w[2], -w[1] / w[2])
  }
}

data(iris)

## �������� ������ ������������ ��� ������ � ��������� �� � ������� dm
dd <- iris[iris$Species != "versicolor", -(3:4)]
dm <- data.matrix(dd[, 1:2])

## � ��� ����� ����������� ��������� ����� ��� ��
## y = ax, � y = ax + b, ��� ����������� � a, � b
dm <- cbind(dm, 1)

## dc � ������ ����� �������������� (����� �������)
## ������ ������ �� ������
dc <- rep(1, nrow(dm))
## � ���������� ��, ��� � ������ -1
dc[dd$Species == "virginica"] <- -1


## ��������� ��������:
pc <- trainPerceptron(dm, dc)
plotPerceptronSteps(pc, dm, dc, length(pc))

## ��������� ���������� � ������ ������� �������
par(mfrow=c(2,2))
plotPerceptronSteps(pc, dm, dc, c(10, 75, 230, length(pc)))

