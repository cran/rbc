#' @importFrom Formula as.Formula
DataFeatures <- R6::R6Class("DataFeatures",
  public = list(
    formula = NULL,
    n_maps = 0,
    model_matrices = NULL,
    parameter_names = character(0),
    n_parameters = integer(0),
    parameter_groups = integer(0),
    linear_predictors = function(par) {
      Map(function(mat, vec) {
          mat %*% vec
        }, self$model_matrices,
        split(par, self$parameter_groups)
      )
    },
    response = NULL,
    initialize = function(formula, data, response = FALSE) {
      self$formula <- Formula::as.Formula(formula)

      self$n_maps <- length(self$formula)[2]
      self$model_matrices <- lapply(1:self$n_maps,
        function(j) {
          stats::model.matrix(self$formula, data = data, rhs = j)
        }
      )
      self$parameter_names <- lapply(self$model_matrices,
        function(mat) dimnames(mat)[2][[1]])
      self$n_parameters <- lapply(self$parameter_names, length)
      self$parameter_groups <- rep(seq_along(self$n_parameters),
        times = self$n_parameters)
      if(response) {
        self$response <- Formula::model.part(
          self$formula, data = data, lhs = 1)[[1]]
      }
    }
  )
)
