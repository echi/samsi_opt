#' Gradient Step
#'
#' \code{gradient_step} computes a single gradient step.
#'
#' @param gradf handle to function that returns gradient of objective function
#' @param x current parameter estimate
#' @param t step-size
#' @export
gradient_step <- function(gradf, t, x) {
 return(x - t*gradf(x))
}

#' Gradient Descent (Fixed Step-Size)
#'
#' \code{gradient_descent_fixed} performs gradient descent with a fixed step size.
#'
#' @param fx handle to function that returns objective function values
#' @param gradf handle to function that returns gradient of objective function
#' @param x0 initial parameter estimate
#' @param t step-size
#' @param max_iter maximum number of iterations
#' @param tol convergence tolerance
#' @export
gradient_descent_fixed <- function(fx, gradf, t, x0, max_iter=1e2, tol=1e-3) {
  x <- x0
  obj <- double(max_iter)
  nGrad <- double(max_iter)
  for (i in 1:max_iter) {
    obj[i] <- fx(x)
    nGrad[i] <- norm(as.matrix(gradf(x)),'F')
    x_plus <- gradient_step(gradf, t, x)
    if (abs(fx(x_plus) - fx(x)) < (1 + abs(fx(x)))*tol)
      break
    x <- x_plus
  }
  return(list(x=x_plus, obj=obj[1:i], nGrad=nGrad[1:i]))
}

#' Backtrack
#'
#' \code{backtrack} performs backtracking line search.
#'
#' @param fx handle to function that returns objective function values
#' @param x current parameter estimate
#' @param t current step-size
#' @param gradf handle to function that returns gradient of objective function
#' @param alpha the backtracking parameter
#' @param beta the decrementing multiplier
#' @export
backtrack <- function(fx, t, x, gradf, alpha=0.5, beta=0.9) {
  for (i in 1:50) {
    x_new <- x - t*gradf(x)
    if (fx(x_new) < fx(x) - alpha*t*norm(as.matrix(gradf(x)),'f')**2)
      break
    t <- beta * t
  }
  return (step_size=t)
}

#' Gradient Descent (Backtracking Step-Size)
#'
#' \code{gradient_descent_bactrack} performs gradient descent with backtracking line search.
#'
#' @param fx handle to function that returns objective function values
#' @param gradf handle to function that returns gradient of objective function
#' @param x0 initial parameter estimate
#' @param max_iter maximum number of iterations
#' @param tol convergence tolerance
#' @export
gradient_descent_backtrack <- function(fx, gradf, x0, t0=1, max_iter=1e2, tol=1e-3) {
  x <- x0
  t <- t0
  obj <- double(max_iter)
  nGrad <- double(max_iter)
  for (i in 1:max_iter) {
    obj[i] <- fx(x)
    nGrad[i] <- norm(as.matrix(gradf(x)),'F')
    t <- backtrack(fx, t, x, gradf)
    x_plus <- gradient_step(gradf, t, x)
    if (abs(fx(x_plus) - fx(x)) < (1 + abs(fx(x)))*tol)
      break
    x <- x_plus
  }
  return(list(x=x_plus, obj=obj[1:i], nGrad=nGrad[1:i]))
}

#' Gradient Descent
#'
#' @param fx handle to function that returns objective function values
#' @param gradf handle to function that returns gradient of objective function
#' @param x0 initial parameter estimate
#' @param t step-size
#' @param max_iter maximum number of iterations
#' @param tol convergence tolerance
#' @export
#' @examples
#' set.seed(12345)
#' n <- 100; p <- 3
#' X <- matrix(rnorm(n*p),n,p)
#' beta <- matrix(rnorm(p),ncol=1)
#' y <- runif(n) <= plogis(X%*%beta)
#'
#' fx <- function(beta) {
#'  return (fx_logistic(y=y, X=X, beta=beta, lambda=10))
#' }
#'
#' gradf <- function(beta) {
#'  return (gradf_logistic(y=y, X=X, beta=beta, lambda=10))
#' }
#'
#' b0 <- matrix(0,p,1)
#' ## Backtracking
#' sol_bt <- gradient_descent(fx, gradf, b0)
#' plot(sol_bt$obj)
#'
#' ## Fixed Step Size
#' t <- 1/svd(X,1,1)$d[1]^2
#' sol_fd <- gradient_descent(fx, gradf, b0, t=t)
#' plot(sol_fd$obj)
#'
#' library(lbfgs)
#' sol_bfgs <- lbfgs(fx, gradf, b0)
gradient_descent <- function(fx, gradf, x0, t=NULL, max_iter=1e2, tol=1e-3) {
  if (is.null(t)) {
    sol <- gradient_descent_backtrack(fx=fx, gradf=gradf, x0=x0, t0=1, max_iter=max_iter, tol=tol)
  } else {
    sol <- gradient_descent_backtrack(fx=fx, gradf=gradf, x0=x0, t0=1, max_iter=max_iter, tol=tol)
  }
  return(sol)
}

#' Negative log-likelihood of logistic regression
#'
#' \code{fx_logistic} computes the negative log-likelihood in ridge logistic regression.
#'
#' @param y binary response
#' @param X design matrix
#' @param beta regression coefficient vector
#' @param lambda regularization parameter
#' @export
fx_logistic <- function(y, X, beta, lambda=0) {
  fx <- -crossprod(X %*% beta, y)
  fx <- fx + sum(log(1 + exp(X %*% beta)))
  fx <- fx + 0.5*lambda * norm(as.matrix(beta),'f')**2
  return(fx)
}

#' Gradient of negative log-likelihood of logistic regression
#'
#' \code{gradf_logistic} computes the gradient of the negative log-likelihood in ridge logistic regression.
#'
#' @param y binary response
#' @param X design matrix
#' @param beta regression coefficient vector
#' @param lambda regularization parameter
#' @export
gradf_logistic <- function(y, X, beta, lambda=0) {
  grad <- t(X)%*%(exp(X%*%beta)/(1+exp(X%*%beta))-y)
  return(grad + lambda*beta)
}
