% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/append_flow.R
\name{append_flow}
\alias{append_flow}
\title{Append a flow to a CompositeFamily object}
\usage{
append_flow(family, flow)
}
\arguments{
\item{family}{a CompositeFamily object}

\item{flow}{a Flow object}
}
\value{
a new CompositeFamily object
}
\description{
Append a flow to a CompositeFamily object
}
\examples{
append_flow(Normal(0, 1), Translate)

Reduce(append_flow, list(Scale, Translate), init = Normal(0, 1))
}
