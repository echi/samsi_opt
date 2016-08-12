#' Soft-threshold

#' Soft-threshold elements of a vector
#'
#' \code{softthreshold} softthresholds the elements of a vector
#'
#' @param x vector to shrink
#' @param lambda regularization parameter
#' @export
softthreshold <- function(x, lambda) {
  sgn <- sign(x)
  z <- abs(x) - lambda
  z[z < 0] <- 0
  return(sgn*z)
}

#' Lasso (Coordinate Descent)
#'
#' \code{lasso_cd} solves the lasso problem using coordinate descent.
#'
#' @param y Response variable
#' @param X design matrix
#' @param beta0 initial guess of regression parameter
#' @param lambda regularization parameter
#' @param max_iter maximum number of iterations
#' @param tol convergence tolerance
#' @export
#' @examples
#' set.seed(12345)
#' n <- 100
#' p <- 200
#'
#' X <- matrix(rnorm(n*p),n,p)
#' beta0 <- matrix(0,p,1)
#' beta0[1:5] <- runif(5)
#'
#' y <- X%*%beta0 + rnorm(n)
#'
#' b0 <- matrix(0,p,1)
#' lambda <- 2
#' sol <- lasso_cd(y,X,b0, lambda)
lasso_cd <- function(y, X, beta0, lambda, max_iter=1e2, tol=1e-3) {
  b <- beta0
  p <- ncol(X)
  obj <- double(max_iter)
  fx <- function(b) {0.5*sum( (y - X%*%b)**2 ) + lambda*sum(abs(b))}
  for (i in 1:max_iter) {
    b_last <- b
    obj[i] <-fx(b)
    for (j in 1:p) {
      r <- y - X[,-j,drop=FALSE] %*% b[-j,drop=FALSE]
      b[j] <- softthreshold(t(r)%*%X[,j,drop=FALSE],lambda)/sum(X[,j]**2)
    }
    if (abs(fx(b) - fx(b_last)) < (1 + abs(fx(b_last)))*tol)
      break
  }
  return(list(b=b, obj=obj[1:i]))
}

#' Lasso (Proximal Gradient Descent)
#'
#' \code{lasso_pgd} solves the lasso problem using proximal gradient descent with a fixed step size
#'
#' @param y Response variable
#' @param X design matrix
#' @param beta0 initial guess of regression parameter
#' @param lambda regularization parameter
#' @param t step-size
#' @param max_iter maximum number of iterations
#' @param tol convergence tolerance
#' @export
#' @examples
#' set.seed(12345)
#' n <- 100
#' p <- 200
#'
#' X <- matrix(rnorm(n*p),n,p)
#' beta0 <- matrix(0,p,1)
#' beta0[1:5] <- runif(5)
#'
#' y <- X%*%beta0 + rnorm(n)
#'
#' b0 <- matrix(0,p,1)
#' lambda <- 2
#' t <- 1/svd(X,1,1)$d[1]**2
#' sol_pgd <- lasso_pgd(y,X,b0, lambda, t)
#' ## KKT check
#' cbind(t(X)%*%(y - X%*%sol_pgd$b), sol_pgd$b)
lasso_pgd <- function(y, X, beta0, lambda, t, max_iter=1e2, tol=1e-3) {
  b <- beta0
  obj <- double(max_iter)
  fx <- function(b) {0.5*sum( (y - X%*%b)**2 ) + lambda*sum(abs(b))}
  for (i in 1:max_iter) {
    obj[i] <- fx(b)
    # forward step
    g <- t(X)%*%(X%*%b - y)
    # backward step
    b <- softthreshold(b - t*g,lambda*t)
  }
  return(list(b=b, obj=obj[1:i]))
}

#' Lasso (ADMM)
#'
#' \code{lasso_admm} solves the lasso problem using ADMM
#'
#' @param y Response variable
#' @param X design matrix
#' @param beta0 initial guess of regression parameter
#' @param lambda regularization parameter
#' @param rho parameter
#' @param max_iter maximum number of iterations
#' @param tol convergence tolerance
#' @export
#' @examples
#' set.seed(12345)
#' n <- 100
#' p <- 200
#'
#' X <- matrix(rnorm(n*p),n,p)
#' beta0 <- matrix(0,p,1)
#' beta0[1:5] <- runif(5)
#'
#' y <- X%*%beta0 + rnorm(n)
#'
#' b0 <- matrix(0,p,1)
#' lambda <- 2
#' sol_admm <- lasso_admm(y,X,b0, lambda)
#' ## KKT check
#' cbind(t(X)%*%(y - X%*%sol_admm$v), sol_admm$v)
lasso_admm <- function(y, X, beta0, lambda, rho=1, max_iter=1e2, tol=1e-3) {

  obj <- double(max_iter)
  fx <- function(b) {0.5*sum( (y - X%*%b)**2 ) + lambda*sum(abs(b))}
  n <- nrow(X); p <- ncol(X)
  # Cholesky decomposition
  G <- t(X)%*%X + rho*diag(1,p,p)
  R <- chol(G)
  b <- beta0
  v <- b
  gamma <- matrix(0,p,1)
  Xty <- t(X)%*%y
  for (i in 1:max_iter) {
    b_last <- b
    obj[i] <- fx(v)
    # Update b
    g <- Xty + rho*(v - gamma)
    b <- backsolve(R,forwardsolve(t(R), g))
    # Update v
    v <- softthreshold(gamma + b, lambda/rho)
    # Update gamma
    gamma <- gamma + b - v
  }
  return(list(b=b,v=v,gamma=gamma,obj=obj[1:i]))
}
