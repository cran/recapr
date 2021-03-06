% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/funcs.R
\name{seChapman}
\alias{seChapman}
\title{Standard Error of the Chapman Estimator}
\usage{
seChapman(n1, n2, m2)
}
\arguments{
\item{n1}{Number of individuals captured and marked in the first sample. This
may be a single number or vector of values.}

\item{n2}{Number of individuals captured in the second sample.  This may be a
single number or vector of values.}

\item{m2}{Number of marked individuals recaptured in the second sample.  This
may be a single number or vector of values.}
}
\value{
The estimate variance of the Chapman estimator, calculated as
  sqrt((n1+1)*(n2+1)*(n1-m2)*(n2-m2)/((m2+2)*(m2+1)^2))
}
\description{
Calculates the standard error of the Chapman estimator in a
  mark-recapture experiment, with given values of sample sizes and number of
  recaptures.
}
\note{
Any Petersen-type estimator (such as this) depends on a set of
  assumptions: \itemize{ \item  The population is closed; that is, that there
  are no births, deaths, immigration, or emigration between sampling events
  \item All individuals have the same probability of capture in one of the
  two events, or complete mixing occurs between events \item Marking in the
  first event does not affect probability of recapture in the second event
  \item Individuals do not lose marks between events \item All marks will be
  reported in the second event }
}
\examples{
seChapman(n1=100, n2=100, m2=20)
}
\seealso{
\link{NChapman}, \link{vChapman}, \link{rChapman}, \link{pChapman},
  \link{powChapman}, \link{ciChapman}
}
\author{
Matt Tyers
}
