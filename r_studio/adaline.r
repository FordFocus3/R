library("MASS")


normali <- function(xl) {
  n <- dim(xl)[2]
  for(i in 1:n)
  {
    min <- min(xl[,i])
    max <- max(xl[,i])
    xl[, i] <- (xl[, i] - min)/(max-min)
  }
  
  return(xl)
}


sg <- function(xl,coll, eta = 1, lambda = 1/6, eps = 1e-5) {
  l <- dim(xl)[1]
  n <- dim(xl)[2] - 1
  w <- rep(0.5, n)
  iterCount <- 0
  
  ## initialize Q
  Q <- 0
  
  for (i in 1:l)
  {
    ## calculate the scalar product <w,x>
    xi <- xl[i, 1:n]
    yi <- xl[i, n + 1]
    mi <-sum(w*xi) * yi
    Q <- Q + (mi - 1)^2
  }
  
  iterCount <- 0
  repeat
  {
    if (iterCount > 1000) {
      
      break
    }
    
    ## calculate the margins for all objects of the training sample
    margins <- array(dim = l)
    for (i in 1:l){
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      margins[i] <- crossprod(w , xi) * yi
      
    }
    
    ## select the error objects
    errorIndexes <- which(margins <= 0)
    if (length(errorIndexes) > 0)
    {
      # select the random index from the errors
      i <- sample(errorIndexes, 1)
      iterCount <- iterCount + 1
      xi <- xl[i, 1:n]
      yi <- xl[i, n + 1]
      
      
      ## calculate an error
      mi <- sum(w*xi)*yi
      ex <- (mi-1)^2
      ld <- (mi/yi - yi) * xi
      eta <- 1/iterCount
      w <- w - eta * ld
      x <- seq(-2, 2, len = 100)
      f <- function(x) {
        return( - x*w[1]/w[2] + w[3]/w[2] )
      }
      y <- f(x)
      lines(x, y, type="l",col=coll)
      
      ## Calculate a new Q
      Qprev <- Q
      Q <- (1 - lambda) * Q + lambda * ex
      if (abs(Q - Qprev) < 1e-5) {
        break
      }
      
    }
    else
    {
      break
    }
    
  }
  return(w)
}


n <- 100

sigma1 <- matrix(c(2,0, 0, 2), 2, 2)
sigma2 <- matrix(c(2, 0,0, 2), 2, 2)

mu1 <- c(4, 4)
mu2 <- c(12, 4)


xy1 <- mvrnorm(n=n, mu = mu1, Sigma = sigma1)
xy2 <- mvrnorm(n=n, mu = mu2, Sigma = sigma2)
xl <- rbind(xy1,xy2)
xl <- normali(xl)
xl <- cbind(xl, rep(-1, n+n))
xl <- cbind(xl, c(rep(-1, n), rep(1, n)))


# plotxmin <- min(xl[,1], xl[,1]) - 0.3
# plotxmax <- max(xl[,1], xl[,1]) + 0.3
# plotymin <- min(xl[,2], xl[,2]) - 0.5
# plotymax <- max(xl[,2], xl[,2]) + 0.5

# Рисуем обучающую выборку
colors <- c("green", "red")
plot(c(), type="n", xlab = "x", ylab = "y", xlim=c(plotxmin, plotxmax), ylim = c(plotymin, plotymax), main="ADALINE")

points(xl, pch=21, col=colors[ifelse(xl[,4] == -1, 1, 2)], bg=colors[ifelse(xl[,4] == -1, 1, 2)])
ada_res <- sg(xl,coll="blue")
abline(a = ada_res[3] / ada_res[2], b = -ada_res[1] / ada_res[2], lwd = 3, col = "blue")