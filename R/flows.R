# data transformation flows

#' Power flow
#'
#' @export
Power <- Flow$new(
  name = "Power",
  data_tsfm = DataTransformation$new(
    f = function(y, v) sign(y) * abs(y)^(exp(v)),
    deriv_f = function(y, v) {
      exp(v) * abs(y)^(exp(v) - 1)
    }, # WolframAlpha includes an extra Dirac delta term at 0
    grad_f = function(y, v) {
      sign(y) * exp(v) * abs(y)^exp(v) * log(abs(y))
    },
    deriv_deriv_f = function(y, v) {
      exp(v) * (exp(v) - 1) * sign(y) * abs(y)^(exp(v) - 2)
    },
    grad_deriv_f = function(y, v) {
      exp(v) * abs(y)^(exp(v) - 1) * (exp(v) * log(abs(y)) + 1)
    }
  ) 
)

#' Moebius flow
#'
#' @export
Moebius <- Flow$new(
  name = "Moebius",
  data_tsfm = DataTransformation$new(
    f = function(y, v) y / (1 + v * y),
    deriv_f = function(y, v) (1 + v * y)^(-2),
    grad_f = function(y, v) -1 * (y / (1 + v * y))^2,
    deriv_deriv_f = function(y, v) -2 * v * (1 + v * y)^(-3), 
    grad_deriv_f = function(y, v) -2 * y * (1 + v * y)^(-3)
  )
)

#' Scale flow
#'
#' @export
Scale <- Flow$new(
  name = "Scale",
  data_tsfm = DataTransformation$new(
    f = function(y, v) y * exp(v),
    deriv_f = function(y, v) exp(v),
    grad_f = function(y, v) y * exp(v),
    deriv_deriv_f = function(y, v) 0,
    grad_deriv_f = function(y, v) exp(v)
  )
)

#' Translate flow
#'
#' @export
Translate <- Flow$new(
  name = "Translate",
  data_tsfm = DataTransformation$new(
    f = function(y, v) y + v,
    deriv_f = function(y, v) 1,
    grad_f = function(y, v) 1,
    deriv_deriv_f = function(y, v) 0,
    grad_deriv_f = function(y, v) 0
  )
)

# probability transformation flows

#' ScaleOdds flow
#'
#' @export
ScaleOdds <- Flow$new(
  name = "ScaleOdds",
  prob_tsfm = ProbabilityTransformation$new(
    f = function(p, v) {
      1 / (1 + ((1 - p) / p) * exp(-v))
    },
    deriv_f = function(p, v) {
      exp(v) / (1 + p * (exp(v) - 1))^2
    },
    grad_f = function(p, v) {
      (1 - p) * p * exp(v) / (1 + p * (exp(v) - 1))^2
    }
  )
)

#' ScaleRisk0 flow
#'
#' @export
ScaleRisk0 <- Flow$new(
  name = "ScaleRisk0",
  prob_tsfm = ProbabilityTransformation$new(
    f = function(p, v) {
      1 - (1 - p) * exp(v) 
    },
    deriv_f = function(p, v) {
      exp(v)
    },
    grad_f = function(p, v) {
      (p - 1) * exp(v)
    }
  )
)

#' ScaleRisk1 flow
#'
#' @export
ScaleRisk1 <- Flow$new(
  name = "ScaleRisk1",
  prob_tsfm = ProbabilityTransformation$new(
    f = function(p, v) {
      p * exp(v)
    },
    deriv_f = function(p, v) {
      exp(v)
    },
    grad_f = function(p, v) {
      p * exp(v)
    }
  )
)

#' TranslateRisk1 flow
#'
#' @export
TranslateRisk1 <- Flow$new(
  name = "TranslateRisk1",
  prob_tsfm = ProbabilityTransformation$new(
    f = function(p, v) {
      p + v
    },
    deriv_f = function(p, v) 1,
    grad_f = function(p, v) 1
  )
)
