#' Soft-threshold

#' Soft-threshold elements of a vector
#'
#' \code{softthreshold} softthresholds the elements of a vector
#'
#' @param x vector to shrink
#' @param lambda regularization parameter
#' @export
softthreshold <- function(x, lambda) {

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
lasso_cd <- function(y, X, beta0, lambda, max_iter=1e2, tol=1e-3) {

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
lasso_pgd <- function(y, X, beta0, lambda, t, max_iter=1e2, tol=1e-3) {

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
lasso_admm <- function(y, X, beta0, lambda, rho=1, max_iter=1e2, tol=1e-3) {

}

