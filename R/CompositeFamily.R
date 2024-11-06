#' @importFrom R6 R6Class
CompositeFamily <- R6::R6Class("CompositeFamily",
  public = list(
    name = character(0),
    flows = list(),
    n_flows = 0,
    initialize = function(name, flows) {
      self$name <- name
      self$flows <- flows
      self$n_flows <- length(self$flows)
    }
  )
)

ContinuousFamily <- R6::R6Class("ContinuousFamily",
  inherit = CompositeFamily,
  public = list( 
    log_density = NULL,
    deriv_log_density = NULL,
    grad_log_density = NULL,
    quantile = NULL,
    cdf = NULL,
    initialize = function(name, flows = list(),
      log_density, quantile, cdf,
      deriv_log_density = NULL,
      grad_log_density = function(y, parameters) {
        list()
      }) {
      super$initialize(name, flows)
      self$log_density <- log_density
      self$deriv_log_density <- deriv_log_density
      self$grad_log_density <- grad_log_density
      self$quantile <- quantile
      self$cdf <- cdf
    },
    fitted = function(parameters, p = 0.5) {
      self$quantile(p, parameters)
    }
  )
)

BinaryFamily <- R6::R6Class("BinaryFamily",
  inherit = CompositeFamily,
  public = list( 
    probability = NULL,
    grad_probability = NULL,
    mean = function(parameters) {
      self$probability(parameters)
    },
    quantile = function(p, parameters) {
      stats::qbinom(p, size = 1,
        prob = self$probability(parameters))
    },
    log_density = function(y, parameters) {
      stats::dbinom(y, size = 1,
        prob = self$probability(parameters), log = TRUE)
    },
    grad_log_density = function(y, parameters) {
      lapply(self$grad_probability(parameters),
        function(grad) {
          grad * (y / self$probability(parameters) -
            (1 - y) / (1 - self$probability(parameters)))
        }
      )
    },
    initialize = function(name, flows = list(), probability,
      grad_probability = function(parameters) {
        list()
      }) {
      super$initialize(name, flows)
      self$probability <- probability 
      self$grad_probability <- grad_probability 
    },
    fitted = function(parameters) {
      self$mean(parameters)
    }
  )
)
