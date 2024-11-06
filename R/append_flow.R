#' Append a flow to a CompositeFamily object
#'
#' @param family a CompositeFamily object
#' @param flow a Flow object
#' @returns a new CompositeFamily object
#' @examples
#' append_flow(Normal(0, 1), Translate)
#'
#' Reduce(append_flow, list(Scale, Translate), init = Normal(0, 1))
#' @export
append_flow <- function(family, flow) {
  UseMethod("append_flow")
}

#' @export
append_flow.ContinuousFamily <- function(family, flow) {
  ContinuousFamily$new(
    name = paste0(family$name, " | ", flow$name),
    flows = c(family$flows, flow),
    log_density = function(y, parameters) {
      flow$data_tsfm$log_deriv_f_inv(y,
        parameters[[family$n_flows + 1]]) +
      family$log_density(
        flow$data_tsfm$f_inv(y,
          parameters[[family$n_flows + 1]]),
        parameters[seq_len(family$n_flows)]) 
    },
    deriv_log_density = function(y, parameters) {
      flow$data_tsfm$deriv_log_deriv_f_inv(y,
        parameters[[family$n_flows + 1]]) +
      flow$data_tsfm$deriv_f_inv(y, 
        parameters[[family$n_flows + 1]]) *
      family$deriv_log_density(
        flow$data_tsfm$f_inv(y,
          parameters[[family$n_flows + 1]]),
        parameters[seq_len(family$n_flows)]) 
    },
    grad_log_density = function(y, parameters) {
      c(
        family$grad_log_density(flow$data_tsfm$f_inv(y,
            parameters[[family$n_flows + 1]]),
          parameters[seq_len(family$n_flows)]),
        list(flow$data_tsfm$grad_log_deriv_f_inv(y,
            parameters[[family$n_flows + 1]]) +
          # this part seems right
          flow$data_tsfm$grad_f_inv(y, 
            parameters[[family$n_flows + 1]]) *
          # the next part seems right
          family$deriv_log_density(
            flow$data_tsfm$f_inv(y,
              parameters[[family$n_flows + 1]]),
            parameters[seq_len(family$n_flows)])
        )
      ) 
    },
    quantile = function(p, parameters) {
      flow$data_tsfm$f(
        family$quantile(p,
          parameters[seq_len(family$n_flows)]),
        parameters[[family$n_flows + 1]])
    },
    cdf = function(q, parameters) {
      family$cdf(flow$data_tsfm$f_inv(
          q, parameters[[family$n_flows + 1]]),
        parameters[seq_len(family$n_flows)])
    }
  )
}

#' @export
append_flow.BinaryFamily <- function(family, flow) {
  BinaryFamily$new(
    name = paste0(family$name, " | ", flow$name),
    flows = c(family$flows, flow),
    probability = function(parameters) {
      flow$prob_tsfm$f(
        family$probability(
          parameters[seq_len(family$n_flows)]),
        parameters[[family$n_flows + 1]])
    },
    grad_probability = function(parameters) {
      c(lapply(
          family$grad_probability(
            parameters[seq_len(family$n_flows)]),
          function(grad) {
            flow$prob_tsfm$deriv_f(
              family$probability(
                parameters[seq_len(family$n_flows)]),
              parameters[[family$n_flows + 1]]) * grad
          }),
        list(flow$prob_tsfm$grad_f(
          family$probability(
            parameters[seq_len(family$n_flows)]),
          parameters[[family$n_flows + 1]])))
    }
  )
}
