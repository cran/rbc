DataTransformation <- R6::R6Class("DataTransformation",
  public = list(
    f = NULL,
    deriv_f = NULL,
    grad_f = NULL,
    deriv_deriv_f = NULL,
    grad_deriv_f = NULL,
    f_inv = function(y, v) { 
      self$f(y, -v)
    },
    deriv_f_inv = function(y, v) {
      self$deriv_f(y, -v)
    },
    grad_f_inv = function(y, v) {
      -self$grad_f(y, -v)
    },
    log_deriv_f_inv = function(y, v) {
      log(self$deriv_f_inv(y, v))
    },
    grad_log_f_inv = function(y, v) {
      self$grad_f_inv(y, v) / self$f_inv(y, v)
    },
    grad_deriv_f_inv = function(y, v) {
      -self$grad_deriv_f(y, -v)
    },
    grad_log_deriv_f_inv = function(y, v) {
      self$grad_deriv_f_inv(y, v) / self$deriv_f_inv(y, v)
    },
    deriv_deriv_f_inv = function(y, v) {
      self$deriv_deriv_f(y, -v)
    },
    deriv_log_deriv_f_inv = function(y, v) {
      self$deriv_deriv_f_inv(y, v) / self$deriv_f_inv(y, v)
    },
    initialize = function(f,
      deriv_f, grad_f = NULL,
      deriv_deriv_f = NULL, grad_deriv_f = NULL) {
      self$f <- f
      self$deriv_f <- deriv_f
      self$grad_f <- grad_f
      self$deriv_deriv_f <- deriv_deriv_f
      self$grad_deriv_f <- grad_deriv_f
    }
  )
)

ProbabilityTransformation <- R6::R6Class(
  "ProbabilityTransformation",
  public = list(
    f = NULL,
    deriv_f = NULL,
    grad_f = NULL,
    initialize = function(f,
      deriv_f, grad_f) {
      self$f <- f
      self$deriv_f <- deriv_f 
      self$grad_f <- grad_f
    }
  )
)

Flow <- R6::R6Class("Flow",
  public = list(
    name = character(0),
    data_tsfm = NULL,
    prob_tsfm = NULL,
    initialize = function(name,
      data_tsfm = NULL, prob_tsfm = NULL) {
      self$name <- name
      self$data_tsfm <- data_tsfm
      self$prob_tsfm <- prob_tsfm
    }
  )
)
