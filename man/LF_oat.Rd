% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LF_oat.R
\docType{data}
\name{LF_oat}
\alias{LF_oat}
\title{Likelihood Factor perform outcome adjusted tmle using sl3.}
\format{\code{\link{R6Class}} object.}
\usage{
LF_oat
}
\value{
\code{LF_fit} object
}
\description{
Uses an \code{sl3} learner to estimate OAT
Inherits from \code{\link{LF_fit}}; see that page for documentation on likelihood factors in general.
}
\section{Constructor}{

  \code{define_lf(LF_oat, name, learner, ..., type = "density")}

  \describe{
    \item{\code{name}}{character, the name of the factor. Should match a node name in the nodes specified by \code{\link{tmle3_Task}$npsem}
    }
    \item{\code{learner}}{An sl3 learner to be used to estimate the factor
    }
    \item{\code{...}}{Not currently used.
    }
    \item{\code{type}}{character, either "density", for conditional density or, "mean" for conditional mean
    }
    }
}

\section{Fields}{

\describe{
    \item{\code{learner}}{The learner or learner fit object}
    }
}

\concept{Likelihood objects}
\keyword{data}
