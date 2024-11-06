ModelFits <- R6::R6Class("ModelFits",
  public = list(
    fits = list(),
    n_fits = 0,
    append_fit = function(fit) {
      self$n_fits <- self$n_fits + 1
      self$fits[[self$n_fits]] <- fit
    }
  )
)
