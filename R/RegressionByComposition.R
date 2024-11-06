RegressionByComposition <- R6::R6Class("RegressionByComposition",
  public = list(
    features = NULL,
    family = NULL,
    fits = NULL,
    initialize = function(features, family, fits) {
      if(features$n_maps != family$n_flows) {
        stop("number of linear maps != number of flows")
      }
      self$features <- features
      self$family <- family
      if(missing(fits)) {
        fits <- ModelFits$new()
      }
      self$fits <- fits
    },
    log_likelihood = function(par) {
      sum(self$family$log_density(self$features$response,
          self$features$linear_predictors(par)))
    },
    score = function(par) {
      unlist(Map(function(dldv, dvdb) apply(dldv[, 1] * dvdb, 2, sum), 
        self$family$grad_log_density(self$features$response,
          self$features$linear_predictors(par)),
        self$features$model_matrices))
    },
    optim_fit = function(par, hessian = TRUE) {
      if(missing(par)) {
        par <- numeric(sum(unlist(self$features$n_parameters)))
      }
      names(par) <- unlist(self$features$parameter_names)
      fit <- stats::optim(par,
        fn = self$log_likelihood,
        gr = self$score,
        # method = "Nelder-Mead",
        method = "BFGS",
        control = list(fnscale = -1, maxit = 1e6, reltol = 1e-15),
        hessian = hessian)
      if(!hessian) {
        fit$hessian <-
          stats::optimHess(self$fits$optim$par,
            fn = self$log_likelihood)
      }
      self$fits$append_fit(fit)
      invisible(self)
    },
    print = function(digits = max(3, getOption("digits") - 3),
      compact = FALSE, ...) {
      cat("Regression by composition\n")
      cat(paste0("family: ", self$family$name, "\n"))
      cat(paste0("formula: ",
          paste0(format(self$features$formula), collapse = "\n"), "\n"))
      cat(paste0("logLik: ", stats::logLik(self), "\n"))
      print(summary(self, compact), digits = digits, ...)
      invisible(self)
    }
  )
)
