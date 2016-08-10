#' Gradient Step
#'
#' \code{gradient_step} computes a single gradient step.
#'
#' @param gradf handle to function that returns gradient of objective function
#' @param x current parameter estimate
#' @param t step-size
#' @export
gradient_step <- function(gradf, t, x) {

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

}
