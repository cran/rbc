#' Extract log-likelihood from a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param ... ignored 
#' @export
logLik.RegressionByComposition <- function(object, ...) {
  object$fits$fits[[object$fits$n_fits]]$value
}

#' Compute Akaike Information Criterion from a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param ... ignored 
#' @param k numeric, the _penalty_ per parameter to be used; ‘k = 2’ is the classical AIC.
#' @export
AIC.RegressionByComposition <- function(object, ..., k = 2) {
  -2 * stats::logLik(object) + k * Reduce("+", object$features$n_parameters)
}

#' Extract regression coefficients from a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param ... ignored 
#' @export
coef.RegressionByComposition <- function(object, ...) {
  object$fits$fits[[object$fits$n_fits]]$par
}

#' Extract variance-covariance matrix from a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param ... ignored 
#' @export
vcov.RegressionByComposition <- function(object, ...) {
  solve(-object$fits$fits[[object$fits$n_fits]]$hessian)
}

#' Compute fitted values from a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param ... further arguments passed to the R6 method $fitted() associated with the model's CompositeFamily 
#' @export
fitted.RegressionByComposition <- function(object, ...) {
  object$family$fitted(parameters =
    object$features$linear_predictors(stats::coef(object)), ...)
}

#' Compute predicted values from a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param newdata data.frame containing new data
#' @param ... further arguments passed to the R6 method $fitted() associated with the model's CompositeFamily 
#' @export
predict.RegressionByComposition <- function(object, newdata, ...) {
  newobject <- object$clone(deep = TRUE)
  if(!missing(newdata)) {
    newobject$features <- DataFeatures$new(object$features$formula, newdata) 
  }
  fitted.RegressionByComposition(newobject, ...)
}

#' Compute 'residuals' from a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param ... ignored 
#' @returns a vector of probabilities of the same length as the data
#' @export
residuals.RegressionByComposition <- function(object, ...) {
  object$family$cdf(object$features$response,
    parameters = object$features$linear_predictors(stats::coef(object)))
}

#' Summary of a regression by composition
#' @param object a RegressionByComposition object; 
#' usually the result of a call to rbc()
#' @param compact logical; should coefficients from all flows be compressed into a single matrix? 
#' @param ... ignored 
#' @export
summary.RegressionByComposition <- function(object, compact = FALSE, ...) {
  mat <- cbind(Estimate = stats::coef(object),
    "Std. Error" = sqrt(diag(stats::vcov(object))))
  mat <- cbind(mat, "z value" = mat[, 1] / mat[, 2])
  mat <- cbind(mat, "Pr(>|z|)" = 2 * stats::pnorm(-abs(mat[, 3])))
  if(!compact) {
    structure(split.data.frame(mat, object$features$parameter_groups),
      names = sapply(object$family$flows, function(flow) flow$name))
  } else {
    mat
  }
}

# include lrtest / BIC methods?
