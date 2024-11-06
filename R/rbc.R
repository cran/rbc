#' Fit a regression by composition model
#'
#' @param formula a formula object, with model components separated by '|'
#' @param init the initial distribution
#' @param flows a list of flows
#' @param family (optional) an object of class 'CompositeFamily';
#' if supplied, 'init' and 'flows' are ignored
#' @param data a data frame
#' @param par a vector of starting values
#' @param hessian logical; use Hessian matrix in model fitting?
#'
#' @return an rbc object
#' @examples
#' ## Annette Dobson (1990)
#' ## "An Introduction to Generalized Linear Models".
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
#' trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' dobson <- data.frame(
#'   weight = c(ctl, trt),
#'   group = gl(2, 10, 20, labels = c("Ctl", "Trt"))
#' )
#' dobson_fit <- rbc(weight ~ 1 | 1 + group,
#'   init = Normal(0, 1),
#'   flows = list(Scale, Translate),
#'   data = dobson
#' )
#'
#' starr_fit <- rbc(
#'   height ~ 1 | 0 + I((280 + age)^(-1)) | 1 | 1,
#'   init = LogNormal(),
#'   flows = list(Power, Moebius, Scale, Translate),
#'   data = subset(starr, id %in% unique(id)[1:10])
#' )
#' @export
rbc <- function(formula, init, flows, family, data, par, hessian = TRUE) {
  if (missing(family)) {
    family <- Reduce(append_flow, flows, init = init)
  }
  RegressionByComposition$new(
    features = DataFeatures$new(formula, data, response = TRUE),
    family = family
  )$optim_fit(par, hessian = hessian)
}
